@ECHO OFF
IF NOT "%~f0" == "~f0" GOTO :WinNT
@"D:\test-git\git-sdk-64-ci\mingw64\bin\ruby.exe" "D:/test-git/git-sdk-64-ci/mingw64/bin/asciidoctor" %1 %2 %3 %4 %5 %6 %7 %8 %9
GOTO :EOF
:WinNT
@"D:\test-git\git-sdk-64-ci\mingw64\bin\ruby.exe" "%~dpn0" %*
