# This script updates a Git for Windows SDK via Pacman.
#
# Its primary use is to update the `git-sdk-64` repository at
# https://github.com/git-for-windows/git-sdk-64, via its `sync`
# workflow.
#
# It can also be run locally. By default, PowerShell does not allow to run
# scripts. This can be temporarily allowed via
#
# Set-ExecutionPolicy -ExecutionPolicy Bypass -Scope Process

Set-PSDebug -Trace 1

function die {
  Param(
    [Parameter(Mandatory=$true,Position=0)] [String]$Message
  )
  [Console]::Error.WriteLine($Message)
  exit 1
}

# switch to the directory containing this script
Set-Location $PSScriptRoot
if (!$?) { die "Could not switch directory to $PSScriptRoot" }

$env:PATH = "$(Get-Location)\ucrt64\bin;$(Get-Location)\usr\bin;" + $env:PATH

# Set to MSYS mode
$env:MSYSTEM = "UCRT6464"
$env:MSYS2_PATH_TYPE = "minimal"

# Create /var/log/ so that pacman.log is written
if (!(Test-Path var\log -PathType Container)) {
  New-Item -ItemType Directory -Path var\log -Force
}

echo "Run Pacman (First Pass)"
bash -lc "pacman -Syyuu --overwrite=\* --noconfirm"
if (!$?) { exit 1 }

# Ensure that the Git for Windows keyring is registered
bash -lc @"
  set -x
  pacman-key --list-keys BB3AA74136C569BB >/dev/null ||
  pacman-key --populate git-for-windows
"@
if (!$?) { die "Could not re-populate git-for-windows-keyring" }

# If Pacman updated "core" packages, e.g. the MSYS2 runtime, it stops
# (because Pacman itself depends on the MSYS2 runtime, and continuing would
# result in crashes or hangs). In such a case, we simply need to upgrade
# *again*.
#
# To detect that, we look at Pacman's log and search for the needle
#
# 	[PACMAN] starting <upgrade-type> system upgrade
#
# If the last such line has the upgrade type `full`, we're fine, and do not
# need to run Pacman again. Otherwise we will have to run it again, letting
# it upgrade the non-core packages.

$type=(
  Get-Content -Tail 512 var\log\pacman.log |
  Select-String -AllMatches -Pattern "\[PACMAN\] starting .* system upgrade" |
  Select -Last 1
)
if ($type -Match "full system upgrade") {
  echo "No second pass needed"
} else {
  echo "Run Pacman again (Second Pass) to upgrade the remaining (non-core) packages"
  bash -lc "pacman -Suu --overwrite=\* --noconfirm"
  if (!$?) { exit 1 }

  # Ensure that the Git for Windows keyring is registered
  bash -lc @"
    set -x
    pacman-key --list-keys BB3AA74136C569BB >/dev/null ||
    pacman-key --populate git-for-windows
"@
  if (!$?) { die "Could not re-populate git-for-windows-keyring" }
}

# Pacman sometimes writes `.pacnew` files; We want to rename them and let
# the post-install script of the `git-extra` package edit them.
$latestSystemUpgrade = (
  Get-Content -Tail 512 var\log\pacman.log |
  Select-String -AllMatches -Pattern "\[PACMAN\] starting .* system upgrade" |
  Select -Last 1
)
$pacnew = (
  Get-Content -Tail (512-$latestSystemUpgrade.LineNumber) var\log\pacman.log |
  Select-String -AllMatches -Pattern "\.pacnew" |
  ForEach-Object -Process {
    if ($_.Line -Match "warning:.*installed as /(.*)\.pacnew$") { $Matches[1] }
  }
)
if ($pacnew.Length -gt 0) {
  $pacnew | ForEach-Object -Process {
    $newName = $_ -Replace '.*/', ''
    $path = $_ -Replace '/', '\'
    $pacnewPath = $path + ".pacnew"
    if (Test-Path $pacnewPath -PathType Leaf) {
      if ($newName -eq "pacman.conf") {
        $ignorePkg = (
          Get-Content $path |
          Select-String -Pattern "^IgnorePkg *="
        )
        if ($ignorePkg.length -eq 1) {
          $content = Get-Content $pacnewPath
          $line = $content | Select-String -Pattern "^ *#? *IgnorePkg *="
          $content[$line.LineNumber] = $ignorePkg[0]
          $content -join "`n" | Out-File -NoNewLine -FilePath $pacnewPath -Encoding ascii
        }
      }

      Remove-Item -Path $path
      Rename-Item -Path $pacnewPath -NewName $newName -Force
    }
  }
  bash -lc @"
    set -x &&
    . /var/lib/pacman/local/mingw-w64-*-git-extra-[0-9]*/install &&
    cd / &&
    post_upgrade
"@
}

if (!(Test-Path cmd\git.exe -PathType Leaf)) {
  # This installation does not yet have the split mingw-w64-git package
  bash -lc @'
    set -x
    for d in clangarm64 ucrt64 mingw64 mingw32
    do
      test -x /$d/bin/git.exe || continue
      export PATH=/$d/bin:$PATH
      case $d in
      clangarm64) carch=clang-aarch64;;
      ucrt64) carch=ucrt-x86_64;;
      mingw64) carch=x86_64;;
      mingw32) carch=i686;;
      esac

      # Install mingw-w64-git-for-windows-addons
      pacman -S --noconfirm mingw-w64-$carch-git-for-windows-addons || exit 1
    done
'@
  if (!$?) { die "Could not install mingw-w64-git-for-windows-addons" }
}

# Wrapping up: re-install mingw-w64-git-extra
bash -lc "pacman -S --overwrite=\* --noconfirm mingw-w64-x86_64-git-extra"
