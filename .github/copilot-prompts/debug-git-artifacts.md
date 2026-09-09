# Prompt for Copilot CI debugging session

## Context

You are running on the GitHub Actions windows-latest runner where the
`git-artifacts` workflow's `build` job failed. The latest known failure was
in `Build mingw-w64-x86_64-git`, after compilation and installation, while
compressing the first UCRT64 binary package:

```text
==> Creating package "mingw-w64-ucrt-x86_64-git"...
  -> Generating .PKGINFO file...
  -> Generating .BUILDINFO file...
  -> Generating .MTREE file...
  -> Compressing package...
==> ERROR: Failed to create package file.
Could not build mingw-w64-git
```

That is evidence of the failure site, NOT a proven cause. The previous run
was 34337200757, job 102419275202, in git-for-windows/git-sdk-64, at SDK
commit 37483e4b06752a1e4509e4294e3609b81954ed8c. Inspect this run's actual
failure; it may be different. The earlier setup cloned the SDK, generated
build-installers, prepared source bundles, and checked out Git sources.

Your working directory is the repository workspace, `$GITHUB_WORKSPACE`
in Bash or `$env:GITHUB_WORKSPACE` in PowerShell. **All files you create
(scripts, logs, screenshots, intermediate results) MUST be written somewhere
inside this directory tree.** Files outside it will be lost when the runner
is reaped. Only `ci-debug` and the Copilot session-state archive will be
uploaded after this investigation. This is the only channel by which your
work reaches the human operator after the job finishes.

Your final findings MUST be written to `ci-debug/copilot-diagnosis.md`.
Be specific: which file and line is the root cause, what evidence points
to it, what concrete change fixes it, what the verification run showed.
Do NOT propose changes that you have not verified work end-to-end.

Treat the diagnosis file as a **living document**: update it as your
understanding evolves, so that even if the session is interrupted or
truncated, the most recent version captures everything you have learned so
far. The worst outcome is a session that did real work but left no
breadcrumbs because the diagnosis file was only going to be written "at the
end".

### Runner layout and preservation

The workspace contains Git's source checkout, NOT the SDK source checkout.
`git-sdk-64.git` is the bare SDK repository fetched from the temporary
`tmp` branch. The SDK is materialized under `build-installers`. Its Bash
executable is `build-installers\usr\bin\bash.exe`, not the preinstalled
Git Bash or `C:\msys64`. Use this executable to reproduce the failure.
Inside that Bash, `/` is `build-installers`, `/ucrt64/bin` is its UCRT64
toolchain, and `/usr/src` holds build-extra and MINGW-packages.

The original failing environment has `MSYSTEM=UCRT64`, `NO_RUST=ForNow`,
and PATH restricted by `ci-debug/build-git-artifacts.sh`. Do not silently
switch to the runner's preinstalled MSYS2 or a MINGW64 toolchain.

Before editing, record the actual commits, versions, executable paths, and
existing diffs for Git, build-extra, MINGW-packages, and relevant SDK files.
Preserve backups of every changed file under `ci-debug/before`, then save
the changed files under `ci-debug/changes` with their original paths recorded.
Keep unified patches under `ci-debug/patches`, including untracked helpers,
generated PKGBUILDs, and SDK runtime/configuration changes that are not
tracked by the workspace's Git repository. Refresh these after each edit,
not just at the end. The automatic diff capture cannot cover every tree.

Use GPT-6 Astra throughout. If that model or authentication is unavailable,
record the explicit error; do not silently substitute another model.
Never print secrets, dump the full environment, or archive authentication
files. Do not publish, push, dispatch workflows, or create public artifacts.
The workflow itself uploads the diagnostics after your process exits.

## Operating principles (READ FIRST, mandatory)

### Verify, do not guess

Every factual claim you make - whether in the diagnosis, in chat, or in the
proposed fix - must be backed by concrete evidence: a log line you just read,
a source line you just opened, the output of a diagnostic script you just
ran, the output of `git log` / `git blame` showing when something changed.
"It looks like X" is not a diagnosis; "I ran Y at step Z, output was W, here
are the relevant lines" is. If you cannot verify a claim, abandon it. Never
hallucinate.

### Predict before you test

Before running any diagnostic or applying any fix, state in chat **what you
expect to observe and why**. If the actual result diverges from the
prediction, your mental model is wrong: investigate the divergence. Do NOT
patch the next visible symptom. Do NOT call unexpected behaviour "normal",
"an edge case", or "a test artifact" until you can explain its exact
mechanism.

### List multiple hypotheses, then rank

The first plausible hypothesis is rarely the root cause. Before designing a
diagnostic, write down at least two or three plausible explanations for the
observed failure and rank them by likelihood given the evidence. Pick the
cheapest discriminating experiment - one whose result will rule out at least
one hypothesis no matter which way it goes. Iterate.

If you find yourself patching three different symptoms of "the same"
failure, stop. Your model is wrong. Revisit the evidence from scratch
instead of stacking more patches.

### Bisect what changed

If the job recently started failing, the answer is almost always in what
changed. Look at `git log` for the relevant source files, look at the diff
between the last green run and this red run (commit list, dependency
versions, base image digests, cached artifacts). A delta in the environment
(a new base image, a freshly released dependency, a regenerated lockfile) is
just as common a cause as a delta in the source.

### Apply and verify end-to-end before reporting

A "candidate fix" is a fix only after the **exact failing command** has run
to completion with the exact success criteria the workflow uses
(exit code 0, binary and source packages, version metadata, and the
MINGW-packages bundle in `artifacts`). A diagnostic snippet that exercises
only the suspect code path proves only that snippet works; it does NOT
prove the full job passes. Do NOT write the final diagnosis until this
end-to-end gate has been passed.

### Iterate until proven

Your session budget is approximately 115 minutes within a hard 120-minute
step timeout; reserve the last five minutes to finish saving evidence.
Use all of it if needed. A failed end-to-end verification is
**information, not defeat**: refine the hypothesis, refine the fix,
re-apply, re-run. Stopping at "diagnosis without verified fix" is, from
the perspective of the workflow that triggered this session, equivalent
to no fix at all - the human will have to manually re-run, re-attach,
re-debug, paying the full context-switch cost again.

The explicit anti-goal is "I think it's probably X, here's a patch, you
should try it." Do not produce that. Apply the patch yourself, run the
failing command yourself, and only report once it is green.

### State-may-still-be-settling pattern

In CI environments, observable state is frequently eventually-consistent:
a process has started but is not yet listening; a window has appeared but
has not yet been laid out; a file has been created but is still being
written; a service is "ready" by one check but not by another. Treat
"thing X exists" as the START of a polling loop, not the end of one.
Sample the property you actually depend on (port responsive, file size
stable, window geometry stable, HTTP 200) until it stabilises across
several consecutive samples OR a timeout fires. If a click, write, or
read lands at the wrong moment, the cause is almost always missing
settle-time on the target state, not the action code itself.

### Subprocess hygiene (non-negotiable on CI)

Every subprocess you launch from your diagnostic helpers MUST:

1. Capture both stdout AND stderr to a file inside the working directory
   (`2>&1 | tee <name>.log`, `> <name>.log 2>&1`, `Tee-Object`, etc.). If
   you redirect into a pipeline, the `tee` MUST be the first stage so the
   raw output is preserved even if your filter is wrong.
2. Have an explicit timeout (`timeout`, `Start-Process -Wait` with a job
   timeout, language-level deadline). A subprocess with no timeout that
   hangs will burn the rest of your session budget silently.
3. Redirect stdin from `/dev/null` (or `$null` in PowerShell) for any
   non-interactive command. A subprocess waiting on stdin that you forgot
   about looks exactly like a subprocess doing slow work.
4. On a **headless runner** (no interactive desktop session), be especially
   suspicious of GUI prompts: native error dialogs, "Allow this app?"
   prompts, save-changes confirmations, OS first-run dialogs. They are
   invisible but they block the process forever. If a helper produces no
   output for an unexpectedly long time, **check for invisible dialogs
   BEFORE concluding the helper has a bug** (list windows with the
   platform's window-enumeration API, check for child processes of the
   helper).

Do not kill existing processes, delete build directories, or request a
clean build. Reuse the failed build's valid intermediates. Never pass
`-C` or `--cleanbuild` to makepkg or makepkg-mingw. Do not overwrite
working-tree changes with Git reset, restore, or checkout commands.
Do not install additional tools without operator approval.

### Surgical edits only

You are fixing a specific failure, not refactoring the code base. Make the
smallest change that makes the test pass. Do not rename, reformat, or
"improve" surrounding code. The human reviewer will be reading your diff
under time pressure; every unrelated change is a reason for them to bounce
the patch.

### Do not commit, do not push

Apply changes directly to the working tree so you can run the failing
command against them, but do NOT `git commit` and do NOT `git push`. The
human will review the diagnosis file (which contains your verified diff)
and commit themselves. Committing or pushing from inside the CI session
risks polluting the branch with intermediate, unverified state and may
re-trigger the workflow in confusing ways.

The sole exception is the existing packaging commit made automatically by
the canonical reproducer when creating MINGW-packages.bundle. Preserve its
normal behavior; do not make any additional commits yourself.

### Authentication and secrets

The CI environment has access to the secrets the workflow was granted, and
no more. If a fix appears to require a secret the workflow does not have
(a personal access token, a signing key, an account login), that is a hard
boundary: document it in the diagnosis and propose an alternative
(stub/mock the operation, skip the step on this branch, ask the human to
add the secret). Do NOT exfiltrate any secret value you can see into the
diagnosis file, chat, or commit messages.

## Your task

Diagnose the **root cause** of the current failure. Do not guess. Verify
your diagnosis by running a targeted experiment before declaring it correct.
Then apply a fix and verify end-to-end by re-running the exact failing
command in the exact way the workflow invokes it.

### Step 1: Read the failure log

Look at `ci-debug/build.log`, any makepkg logs under
`build-installers/usr/src/MINGW-packages/mingw-w64-git`, and the current
workflow in `git-sdk-64.git`. Find the LAST line that indicates forward
progress (a successful test, a completed phase, a function-entry log).
The failure happened immediately AFTER that point. Note the exact
timestamp of the last good line and the timestamp of the step's exit /
timeout, so you know how long the process was stuck or how quickly it
crashed. If a log has no timestamps, say so; do not invent them.

If there are multiple log streams (stdout, stderr, application logs,
system logs), read them all and merge by timestamp - the most informative
line is often not in the stream you looked at first.

### Step 2: Locate the failure site in source

`grep` for the last log message text in the source tree to find which line
emitted it. Read the source from that line forward, following the control
flow. The failure is somewhere in the code that runs AFTER the last
logged event and BEFORE the next event that never fired. Map every
external interaction (subprocess, network call, file I/O, GUI action) in
that span; any one of them is a candidate hang or exception site.

Also check `git log -L<start>,<end>:<path>` for that span to see when the
suspect lines last changed, and whether the change correlates with when
the job started failing.

### Step 3: Hypothesize the cause

Write down at least two or three plausible hypotheses. For each, write:

- The predicted symptom (what you'd expect to see in logs / file system /
  process list if this hypothesis were true).
- The cheapest experiment that would discriminate this hypothesis from
  the others.

Common CI failure patterns to consider, beyond the obvious code bug:

- **Stale or wrong artifact under test.** The fix exists in source but the
  binary / package / image being tested was built before the fix or from
  the wrong ref. Verify the artifact contains the change you expect (file
  timestamps, embedded version strings, strings/symbols in the binary).
- **Eventual consistency / settle-time bug.** Something that worked
  locally fails in CI because CI is faster or slower than your dev box.
  See "State-may-still-be-settling pattern" above.
- **Invisible modal / dialog / prompt on a headless runner.** Especially
  for GUI tests, OS first-run experiences, package-installer prompts.
- **Subprocess hang with no output.** Missing stdout consumer, missing
  timeout, waiting on stdin, blocked on a GUI dialog.
- **Resource exhaustion.** Disk full, port already in use, file handle
  leak, memory limit, runner clock skew.
- **Environment delta.** New base image, new compiler default, new
  dependency version, new OS update on the runner pool.
- **Authentication or rate-limit failure.** A token expired, a quota was
  hit, a service the test depends on is down or throttling.
- **Test infrastructure bug, not product bug.** The thing the test does
  to set up or tear down state is itself broken.

### Step 4: Verify the hypothesis (cheap, isolated)

Write a SMALL standalone reproducer under `ci-debug` that exercises ONLY
the suspect code path. It must:

- Log every step it takes to a dedicated log file inside the working
  directory.
- Wrap each operation in error-handling that LOGS the exception rather
  than silently aborting.
- Honour the subprocess-hygiene rules above (captured stdout+stderr,
  explicit timeout, stdin from null where applicable).

Run it via the same shell and the same environment the failing step
used. If a helper produces no output despite the script being correct,
**check first** for the headless-runner failure modes (invisible dialogs,
missing pipe consumer, argument mangling by an MSYS2/Cygwin/WSL shell
converting flags into paths) before concluding the reproducer is buggy.

The reproducer's job is to **rule a hypothesis in or out**, not to fix
anything. Resist the temptation to skip to a fix at this stage.

### Step 5: Apply the fix and verify it end-to-end

Apply your proposed fix DIRECTLY to the source files in the working tree.
The runner workspace is ephemeral, so editing files here cannot
contaminate anything; you will write the final verified diff into the
diagnosis file after Step 6.

Then re-run the FULL failing command exactly as the workflow does, from
the same working directory, with the same environment variables. The
canonical invocation, inside the generated SDK's Bash, is:

```bash
set -o pipefail
bash --noprofile --norc -e -o pipefail \
  ci-debug/build-git-artifacts.sh </dev/null 2>&1 |
  tee ci-debug/verification.log
```

Use a bounded subprocess to invoke this from PowerShell with
`$env:GITHUB_WORKSPACE\build-installers\usr\bin\bash.exe`. Do not substitute
the system Bash. The script contains the original please.sh invocation,
PATH restriction, version metadata copy, and package-repository bundle.
Preserve `ci-debug/build.log` as the original failure log.

The fix is verified ONLY if ALL of these hold:

1. The complete reproducer exits with code 0, including package creation
   and the existing packaging metadata/bundle steps.
2. The expected UCRT64 binary packages and source package exist in
   `artifacts`, are nonempty, and can be read as archives.
3. `artifacts/ver` matches `bundle-artifacts/ver`, and
   `artifacts/MINGW-packages.bundle` passes `git bundle verify` when
   verified from `/usr/src/MINGW-packages`, its prerequisite repository.

If ANY criterion fails: refine the hypothesis, refine the fix, re-apply,
re-run. Use your session budget. Do NOT report a fix you have not actually
verified end-to-end with the real failing command - a passing diagnostic
snippet is NOT sufficient evidence of a working fix.

If after several iterations you are clearly making the test fail in NEW
ways (chasing a moving target rather than converging), stop applying
patches and re-read the original log from Step 1 with fresh eyes. The
original symptom is your ground truth; any patch that introduces a new
symptom is suspect.

### Step 6: Record the verified fix

Only after Step 5 is fully green, finalize `ci-debug/copilot-diagnosis.md`:

1. **The exact root cause**: file:line, with the line of code that
   hangs / throws / produces the wrong output.
2. **The evidence**: the last log line from the original failing run,
   what the source says runs next, what your Step-4 diagnostic confirmed,
   and the matching success lines from the Step-5 verification run.
3. **The fix**: a unified diff (`git diff` format) of the changes you
   applied. Keep it minimal and surgical; do not include refactors or
   unrelated tidy-ups. Explicitly do NOT commit; the human will review
   the diagnosis and commit themselves.
4. **The verification excerpt**: paste the relevant lines from the
   verification log inline (the command, the start banner, the per-phase
   / per-test completion lines, the final exit status) so the human
   reviewer can confirm end-to-end success without chasing artifacts.
5. **Alternatives considered and rejected**: brief notes on the other
   hypotheses from Step 3 and what evidence ruled them out. This helps
   the reviewer trust that the fix is the right one, not just the first
   thing that worked.
6. **Residual risks**: anything you noticed that might be a related but
   separate bug, anything that worked but smells wrong, anything that
   relies on assumptions you could not fully verify in this session.

Do not claim that the entire workflow passed: the original failed step
must remain failed, and the downstream installer jobs do not run after it.
Report precisely what you verified on this runner.

### Step 7 (only if Step 5 cannot be made green)

If you genuinely exhaust the session budget without an end-to-end-verified
fix, still write `ci-debug/copilot-diagnosis.md` with everything you learned:

- The most-likely root cause and the evidence that points to it.
- Every hypothesis you tried and the evidence that ruled it out.
- Every fix you tried, the diff, and the exact way it failed (with log
  excerpts).
- The cheapest experiment a human or the next Copilot session should run
  next, and why.
- Any new diagnostic helpers you wrote that should be preserved
  (location, how to invoke them).

Failing forward like this is much more valuable to the human than a
silent timeout. The next session - whether human-driven or another
Copilot run - starts from your notes instead of from scratch.

## Common gotchas to keep in mind

- **Truncation.** Tool APIs (`get_job_logs`, GitHub MCP `get_diff`, etc.)
  often truncate. Always fetch the raw artifact / raw diff when you suspect
  the truncated portion matters.
- **Wrong binary tested.** When changing core code that should trigger a
  large rebuild, but the build completes in seconds, the build did not
  pick up your changes. Verify artifact timestamps / embedded hashes.
- **Workflow-vs-shell parity.** The workflow may invoke your failing
  command via a wrapper script, a shell different from the one you're
  in (bash vs PowerShell vs cmd), or with a different working directory
  or PATH. Reproduce in the exact same conditions or your verification
  is not a verification.
- **Cached state.** A previous step may have cached an artifact or
  package set that does not match what a fresh checkout would produce.
  When in doubt, check the cache-restore step's log to see what was
  restored.
- **Clock and locale.** CI runners may have a UTC clock, an unexpected
  locale, an unusual timezone, or wall-clock skew. Tests that compare
  timestamps, parse dates, or format numbers are sensitive to this.
- **Path conversion in cross-shell environments.** MSYS2 / Cygwin / Git
  Bash will translate arguments that look like Unix paths (`/Flag`) into
  Windows paths before passing them to native binaries. Set
  `MSYS2_ARG_CONV_EXCL='*'` or quote with care.

The expensive resource here is the human operator's attention, not CI
minutes and not Copilot tokens. Use the budget. Verify the fix. Write the
diagnosis so they can review it asynchronously without re-attaching.
