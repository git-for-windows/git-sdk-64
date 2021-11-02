# git-sdk-64
A Git repository mirroring the current 64-bit Git for Windows SDK

The Git-for-Windows SDK is created by an installer located in the 
https://github.com/git-for-windows/build-extra/blob/main/sdk-installer/
repository. 

The SDK has 32bit and 64bit versions, e.g. https://github.com/git-for-windows/git-sdk-64

The installer is a 7-zip self-extracting archive which auto-runs a
batch script to populate the relevant parts of th archive's repositories.
The batch script then delete's itself.

The SDK can be updated using the `update-via-pacman.bat` script, as detailed in
the Git-for-Windows Wiki page
https://github.com/git-for-windows/git/wiki/Updating-your-SDK
