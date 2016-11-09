@ECHO OFF
IF NOT "%~f0" == "~f0" GOTO :WinNT
@"\mingw32\bin\ruby.exe" "C:/repo/mingw-w64-ruby/pkg/mingw-w64-i686-ruby/mingw32/bin/rake" %1 %2 %3 %4 %5 %6 %7 %8 %9
GOTO :EOF
:WinNT
@"\mingw32\bin\ruby.exe" "%~dpn0" %*
