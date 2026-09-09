# Fix the failing UCRT64 installer on this runner

The goal is a green git-artifacts workflow, not merely a plausible patch.
You are on the Windows runner that just failed its installer job.
Use GPT-6 Astra throughout. You have a hard 120-minute step timeout;
use up to 115 minutes and reserve five minutes for final preservation.

Read the operating principles in
`workflow/.github/copilot-prompts/debug-git-artifacts.md` before starting.
Those evidence, hypothesis, subprocess, minimal-change, and preservation
rules apply here. Its package-build task does NOT apply: do not rebuild
Git packages. This document defines your task and success criteria.

## Inputs and exact environment

- The working directory is the GitHub workspace.
- `pkg-x86_64`, `bundle-artifacts`, and `build-installers.tgz` come from
  the successful build job of run 34355542074, SDK commit
  82a915cca957d3119776ac70267b2703b0ded492.
- The generated SDK is extracted under `build-installers`. Its Bash is
  `build-installers\usr\bin\bash.exe`; inside it `/` is that SDK root.
  Its build-extra is `/usr/src/build-extra`.
- `workflow` is a sparse checkout of the temporary SDK branch containing
  the current workflow, SDK selection files, and diagnosis scripts.
- `main-baseline/build-installers.tgz` is the actual passing main SDK
  from run 34324713244, commit
  7ca76c34e6aa2edcba062582a5338be2b50fe769.
  That run passed build, installer, portable, MinGit, and installer
  validation on the same day. Its full logs are in
  `main-baseline/logs` and `ci-debug/main-logs.zip`.
- The intended environment is `MSYSTEM=UCRT64`, `ARCH_NAME=x86_64`,
  and the generated SDK's executables on PATH. Inspect the actual
  environment and executable resolution instead of assuming them.

The prior UCRT64 installer failure reported that `/usr/bin/markdown`
could not load `Digest/MD5.pm`, then failed to render ReleaseNotes.html.
Its git-extra post_install also reported missing relative
`etc/pacman.conf` and missing `/etc/bash.bashrc`. These are observations,
not established root causes. Start with THIS run's full log at
`ci-debug/build-installer.log`.

## Investigation

Create `ci-debug/copilot-diagnosis.md` immediately and update it as you
work. Record multiple hypotheses, state predictions before probes, and
revise the model when results diverge.

Compare the failing SDK with the actual passing main SDK. Determine
whether files are absent, looked up relative to the wrong directory,
excluded by a sparse selection, or accessed through the wrong runtime.
Record actual paths, versions, package metadata, environment, and
relevant source differences. Distinguish MINGW64 versus UCRT64 inputs.

Focus on the SDK and workflow. The same main workflow passes, including
these jobs. UCRT64 installers are also already published using build-extra
through git-for-windows-automation's git-artifacts.yml. Compare that
working workflow's runner initialization when relevant.
Do not modify build-extra tooling or investigate a developer's local
checkout. Find the difference in this SDK/workflow's setup, using the
actual failure logs and passing inputs as evidence.

Trace any proposed fix through source selection, generated SDK contents,
runtime loading, and the exact installer command. A missing module is
not by itself evidence that adding an arbitrary dependency is correct.
For example, check how create-sdk-artifact chose its architecture and
dependency list, and check the working directory used by post_install.

Extract the baseline separately; never overlay it onto the failing SDK.
Do not mask the UCRT64 issue by silently using binaries or modules from
the system Git installation or the main MINGW64 artifact.

Before changing any file, preserve its original under `ci-debug/before`.
After each change, save its patched copy under `ci-debug/changes` and
a review-ready unified patch under `ci-debug/patches`. State which
repository each patch targets. Refresh these continuously so a timeout
does not lose the work. New diagnostic scripts and complete stdout/stderr
logs must also remain under `ci-debug`.

Do not commit, push, publish, or dispatch other workflows. Do not install
tools, delete valid build intermediates, or kill unrelated processes.
Never use a clean build. Reuse the already successful Git packages.
Never expose credentials, dump the complete environment, or copy
authentication configuration into artifacts.

## Apply the fix and verify

Apply the smallest fix directly on this runner and rerun the exact
installer command. From the workspace, using the generated SDK's Bash:

```bash
set -o pipefail
bash --noprofile --norc -e -o pipefail \
  ci-debug/build-installer.sh </dev/null 2>&1 |
  tee ci-debug/verification.log
```

Use a bounded subprocess, capture both streams, and preserve
`ci-debug/build-installer.log` as the original failure. If you need to
change an environment variable or working directory, record why,
demonstrate the source of the mismatch, and propose the corresponding
workflow/source fix rather than an unexplained manual workaround.

Verification is complete only when:

1. The exact installer build exits 0 and produces the expected nonempty
   UCRT64 installer executable.
2. The `Copy package-versions and pdbs` commands from the current
   workflow succeed with matching package versions and debug symbols.
3. The workflow's silent installer invocation succeeds.
4. Its `Validate` commands pass, including the installed Git version
   and the installer checklist. Preserve installer.log and validation
   stdout/stderr in `ci-debug`.

These commands are in `workflow/.github/workflows/git-artifacts.yml`.
Do not weaken checks, skip required files, or claim success from the
isolated module probe alone. Iterate on any later failure within the
same runner session and budget.

The original failed Actions step remains failed, so do not claim that
the whole workflow is green. Report exactly which commands you verified
and their exit codes. Preserve the installer under
`ci-debug/verified-artifacts` along with the evidence.

If time expires without all gates passing, leave a precise partial
diagnosis: tested hypotheses, observed results, patches attempted,
remaining blocker, and the cheapest next discriminating experiment.
The final report and patches are the handoff, not a chat-only answer.
