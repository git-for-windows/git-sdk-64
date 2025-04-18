# tdbcConfig.sh --
#
# This shell script (for sh) is generated automatically by TDBC's configure
# script. It will create shell variables for most of the configuration options
# discovered by the configure script. This script is intended to be included
# by the configure scripts for TDBC extensions so that they don't have to
# figure this all out for themselves.
#
# The information in this file is specific to a single platform.
#
# RCS: @(#) $Id$

# TDBC's version number
tdbc_VERSION=1.1.10
TDBC_VERSION=1.1.10

# Name of the TDBC library - may be either a static or shared library
tdbc_LIB_FILE=tdbc1110.dll
TDBC_LIB_FILE=tdbc1110.dll

# String to pass to the linker to pick up the TDBC library from its build dir
tdbc_BUILD_LIB_SPEC="-LD:/M/B/src/build-MINGW64/pkgs/tdbc1.1.10 -ltdbc1110"
TDBC_BUILD_LIB_SPEC="-LD:/M/B/src/build-MINGW64/pkgs/tdbc1.1.10 -ltdbc1110"

# String to pass to the linker to pick up the TDBC library from its installed
# dir.
tdbc_LIB_SPEC="-LD:/a/msys64/mingw64/lib/tdbc1.1.10 -ltdbc1110"
TDBC_LIB_SPEC="-LD:/a/msys64/mingw64/lib/tdbc1.1.10 -ltdbc1110"

# Name of the TBDC stub library
tdbc_STUB_LIB_FILE="libtdbcstub1110.a"
TDBC_STUB_LIB_FILE="libtdbcstub1110.a"

# String to pass to the linker to pick up the TDBC stub library from its
# build directory
tdbc_BUILD_STUB_LIB_SPEC="-LD:/M/B/src/build-MINGW64/pkgs/tdbc1.1.10 -ltdbcstub1110"
TDBC_BUILD_STUB_LIB_SPEC="-LD:/M/B/src/build-MINGW64/pkgs/tdbc1.1.10 -ltdbcstub1110"

# String to pass to the linker to pick up the TDBC stub library from its
# installed directory
tdbc_STUB_LIB_SPEC="-LD:/a/msys64/mingw64/lib/tdbc1.1.10 -ltdbcstub1110"
TDBC_STUB_LIB_SPEC="-LD:/a/msys64/mingw64/lib/tdbc1.1.10 -ltdbcstub1110"

# Path name of the TDBC stub library in its build directory
tdbc_BUILD_STUB_LIB_PATH="D:/M/B/src/build-MINGW64/pkgs/tdbc1.1.10/libtdbcstub1110.a"
TDBC_BUILD_STUB_LIB_PATH="D:/M/B/src/build-MINGW64/pkgs/tdbc1.1.10/libtdbcstub1110.a"

# Path name of the TDBC stub library in its installed directory
tdbc_STUB_LIB_PATH="D:/a/msys64/mingw64/lib/tdbc1.1.10/libtdbcstub1110.a"
TDBC_STUB_LIB_PATH="D:/a/msys64/mingw64/lib/tdbc1.1.10/libtdbcstub1110.a"

# Location of the top-level source directories from which TDBC was built.
# This is the directory that contains doc/, generic/ and so on.  If TDBC
# was compiled in a directory other than the source directory, this still
# points to the location of the sources, not the location where TDBC was
# compiled.
tdbc_SRC_DIR="D:/M/B/src/tcl8.6.16/pkgs/tdbc1.1.10"
TDBC_SRC_DIR="D:/M/B/src/tcl8.6.16/pkgs/tdbc1.1.10"

# String to pass to the compiler so that an extension can find installed TDBC
# headers
tdbc_INCLUDE_SPEC="-I/mingw64/include"
TDBC_INCLUDE_SPEC="-I/mingw64/include"

# String to pass to the compiler so that an extension can find TDBC headers
# in the source directory
tdbc_BUILD_INCLUDE_SPEC="-ID:/M/B/src/tcl8.6.16/pkgs/tdbc1.1.10/generic"
TDBC_BUILD_INCLUDE_SPEC="-ID:/M/B/src/tcl8.6.16/pkgs/tdbc1.1.10/generic"

# Path name where .tcl files in the tdbc package appear at run time.
tdbc_LIBRARY_PATH="/mingw64/lib/tdbc1.1.10"
TDBC_LIBRARY_PATH="/mingw64/lib/tdbc1.1.10"

# Path name where .tcl files in the tdbc package appear at build time.
tdbc_BUILD_LIBRARY_PATH="D:/M/B/src/tcl8.6.16/pkgs/tdbc1.1.10/library"
TDBC_BUILD_LIBRARY_PATH="D:/M/B/src/tcl8.6.16/pkgs/tdbc1.1.10/library"

# Additional flags that must be passed to the C compiler to use tdbc
tdbc_CFLAGS=
TDBC_CFLAGS=

