@ECHO OFF
IF NOT "%~f0" == "~f0" GOTO :WinNT
@"\mingw64\bin\ruby.exe" "C:/repo/mingw-w64-ruby/pkg/mingw-w64-x86_64-ruby/mingw64/bin/rake" %1 %2 %3 %4 %5 %6 %7 %8 %9
GOTO :EOF
:WinNT
@"\mingw64\bin\ruby.exe" "%~dpn0" %*
