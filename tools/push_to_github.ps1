$ErrorActionPreference = "Stop"
$repo = Split-Path $PSScriptRoot -Parent
Set-Location $repo
$out = Join-Path $PSScriptRoot "push_output.txt"

function Log($msg) { $msg | Tee-Object -FilePath $out -Append }

"" | Set-Content $out
Log "=== LibeRties push ==="
Log (Get-Location).Path

Log (git status -sb 2>&1 | Out-String)
Log (git log -1 --oneline 2>&1 | Out-String)

git add -A 2>&1 | Out-Null
if (git status --porcelain) {
  Log (git commit -m "Release version 0.4.0" 2>&1 | Out-String)
}

if ($LASTEXITCODE -ne 0 -and -not (git remote get-url origin 2>$null)) {
  git remote add origin https://github.com/svdijkman/LibeRties.git
}

Log (git fetch origin 2>&1 | Out-String)
Log ("Remote: " + (git ls-remote origin refs/heads/main 2>&1 | Out-String))

Log (git push -u origin main --force-with-lease 2>&1 | Out-String)
Log ("HEAD: " + (git log -1 --oneline 2>&1 | Out-String))
Log ("Files:" + (git ls-tree -r HEAD --name-only 2>&1 | Select-Object -First 15 | Out-String))
