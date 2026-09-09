param(
    [switch]$SmokeTest,
    [string]$PromptFile = 'ci-debug/debug-git-artifacts.md'
)

$ErrorActionPreference = 'Stop'
$directory = if ($SmokeTest) { 'ci-debug\smoke' } else { 'ci-debug' }
New-Item -ItemType Directory -Force $directory | Out-Null

$token = $env:COPILOT_GITHUB_TOKEN
if ([string]::IsNullOrWhiteSpace($token)) {
    throw 'COPILOT_GITHUB_TOKEN is empty.'
}
@{
    trailingLineBreak = $token -match '[\r\n]$'
    surroundingWhitespace = $token -cne $token.Trim()
} | ConvertTo-Json | Tee-Object "$directory\token-shape.json"
$token = $token.Trim()
if ($token -match '[^\x21-\x7e]') {
    throw 'Token contains internal whitespace or non-ASCII characters.'
}
Write-Output "::add-mask::$token"
$env:COPILOT_GITHUB_TOKEN = $token

if ($SmokeTest) {
    $prompt = 'Use the shell tool to run git --version and write ' +
        'its output to ci-debug/smoke/git-version.txt. Then reply ' +
        'COPILOT_SMOKE_OK. Do not perform any other task.'
} else {
    $prompt = "Follow @$PromptFile exactly."
}

$null | & "$env:RUNNER_TEMP\copilot-cli\copilot.cmd" `
    -p $prompt --model gpt-6-astra --effort xhigh `
    --context long_context --yolo `
    --no-ask-user --no-auto-update --no-remote-export `
    --secret-env-vars=COPILOT_GITHUB_TOKEN `
    --log-dir "$directory\copilot-logs" `
    2>&1 | Tee-Object "$directory\copilot.log"
$code = $LASTEXITCODE
$code | Set-Content "$directory\copilot-exit-code.txt"
if ($code -ne 0) { exit $code }

if ($SmokeTest) {
    if (!(Select-String -Path "$directory\git-version.txt" `
        -Pattern '^git version \d' -Quiet)) {
        throw 'Copilot did not capture the Git version.'
    }
    if (!(Select-String -Path "$directory\copilot.log" `
        -SimpleMatch COPILOT_SMOKE_OK -Quiet)) {
        throw 'Copilot did not return the expected response.'
    }
}
exit 0
