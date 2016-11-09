@ECHO OFF
IF NOT "%~f0" == "~f0" GOTO :WinNT
@"C:\git-sdk-64-ci\mingw64\bin\ruby.exe" "C:/git-sdk-64-ci/mingw64/bin/asciidoctor-safe" %1 %2 %3 %4 %5 %6 %7 %8 %9
GOTO :EOF
:WinNT
@"C:\git-sdk-64-ci\mingw64\bin\ruby.exe" "%~dpn0" %*
