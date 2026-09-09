#!/bin/bash

set -x

# Make sure makepkg-mingw can find Git inside the generated SDK.
printf '#!/bin/sh\n\nexec /ucrt64/bin/git.exe "$@"\n' >/usr/bin/git &&

# Restrict PATH to the same MSYS2 environment as the original build.
PATH="/ucrt64/bin:/usr/bin:/usr/bin/core_perl:/C/Windows/system32"

sh -x /usr/src/build-extra/please.sh build-mingw-w64-git \
	--only-ucrt64 --build-src-pkg -o artifacts HEAD &&
cp bundle-artifacts/ver artifacts/ &&

b=$PWD/artifacts &&
version=$(cat bundle-artifacts/next_version) &&
(cd /usr/src/MINGW-packages/mingw-w64-git &&
cp "PKGBUILD.$version" PKGBUILD &&
ident=$(git var GIT_COMMITTER_IDENT) &&
git commit --trailer "Signed-off-by: ${ident%>*}>" \
	-m "mingw-w64-git: new version ($version)" PKGBUILD &&
git bundle create "$b"/MINGW-packages.bundle origin/main..main)
