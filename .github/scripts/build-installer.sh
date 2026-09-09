#!/bin/bash

sh -x /usr/src/build-extra/please.sh make_installers_from_mingw_w64_git \
	--version="$(cat pkg-${ARCH_NAME}/ver)" \
	-o artifacts \
	--installer \
	$(ls pkg-$ARCH_NAME/mingw-w64-*.pkg.tar.* |
	  sed -e '/\.sig$/d;/archimport/d;/cvs/d;/p4/d;/gitweb/d;/doc-man/d' \
	      -e 's/^/--pkg=/')
