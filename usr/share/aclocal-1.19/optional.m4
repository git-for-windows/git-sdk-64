##                                                       -*- Autoconf -*-
## AM_OPTIONAL_AUTOMAKE -- skip `dist-XXX' formats whose tool is absent.

# Copyright (C) 2026 Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.


# AM_OPTIONAL_AUTOMAKE(OPTIONS)
# -----------------------------
# OPTIONS: whitespace-separated `dist-XXX' names (AM_INIT_AUTOMAKE
# spellings) for dist-bzip2/bzip3/xz/lzip/zstd/zip/shar/tarZ.  `make
# dist' builds each archive when its tool is present and skips it
# otherwise, without failing.  Unknown OPTION warns at autoreconf; a
# tool absent at configure appends no rule; a tool lost afterwards
# fails, which the recipe catches.  See bug#81040.
AC_DEFUN([AM_OPTIONAL_AUTOMAKE],
[m4_foreach_w([_am_opt_o], [$1],
   [_AM_OPTIONAL_AUTOMAKE_ONE(_m4_defn([_am_opt_o]))])dnl
])


# _AM_OPTIONAL_AUTOMAKE_ONE(OPT)
# ------------------------------
# Dispatch on OPT; unrecognized OPT warns and is ignored.
AC_DEFUN([_AM_OPTIONAL_AUTOMAKE_ONE],
[m4_case([$1],
  [dist-bzip2], [_AM_OPTIONAL_TAR([bzip2], [bzip2], [bz2], [-c])],
  [dist-bzip3], [_AM_OPTIONAL_TAR([bzip3], [bzip3], [bz3], [-c])],
  [dist-xz],    [_AM_OPTIONAL_TAR([xz],    [xz],    [xz],  [-c])],
  [dist-lzip],  [_AM_OPTIONAL_TAR([lzip],  [lzip],  [lz],  [-c])],
  [dist-zstd],  [_AM_OPTIONAL_TAR([zstd],  [zstd],  [zst], [-c -q])],
  [dist-tarZ],  [_AM_OPTIONAL_TAR([compress], [compress], [Z], [-c])],
  [dist-zip],   [_AM_OPTIONAL_ZIP()],
  [dist-shar],  [_AM_OPTIONAL_SHAR()],
  [m4_warn([syntax],
     [am-optional: unknown option `$1', ignoring it])])dnl
])


# _AM_OPTIONAL_RULE(TAG, VAR, RECIPE, ARTIFACTS)
# ----------------------------------------------
# AC_CHECK_PROG for TAG, then once append RECIPE to the top Makefile
# via AC_CONFIG_COMMANDS (survives config.status reruns) iff configure
# found the tool.  Hooked into dist/dist-all as an extra prerequisite.
# ARTIFACTS stay out of $(DIST_ARCHIVES), so distcleancheck must remove
# the copies that the in-tree `make dist' run by `make distcheck' leaves.
AC_DEFUN([_AM_OPTIONAL_RULE],
[m4_ifdef([_AM_OPTIONAL_DONE_$1], [],
 [m4_define([_AM_OPTIONAL_DONE_$1])dnl
AC_CHECK_PROG([am__optional_$2], [$1], [$1])
AC_SUBST([am__optional_$2])dnl
AC_CONFIG_COMMANDS([am--optional-rule-$1],
[am__optional_mf=
for am__optional_cand in Makefile GNUmakefile makefile; do
  if test -f "$am__optional_cand"; then
    am__optional_mf=$am__optional_cand
    break
  fi
done
if test -n "$am__optional_mf" && test -n "$am__optional_$2"; then
  cat >>"$am__optional_mf" <<'_am_opt_rule_eof_'

# Appended by am-optional for dist-$1.
dist dist-all: am--optional-dist-$1
distcleancheck: am--optional-rm-$1
.PHONY: am--optional-dist-$1 am--optional-rm-$1
$3
am--optional-rm-$1:
	-rm -f $4
_am_opt_rule_eof_
fi
],
[am__optional_$2="$am__optional_$2"
])])dnl
])


# _AM_OPTIONAL_TAR(TOOL, VAR-TAG, EXT, FLAGS)
# -------------------------------------------
# Compress the shared intermediate tarball, whose rule catches tar's own
# failures (bug#19614); only TOOL failing is tolerated here.
AC_DEFUN([_AM_OPTIONAL_TAR],
[_AM_OPTIONAL_RULE([$1], [$2],
[am--optional-dist-$1: am__distdir.tar
	@if test -n "$(am__optional_$2)"; then \
	  $(am__ensure_distdir_tar) || exit 1; \
	  "$(am__optional_$2)" $4 < am__distdir.tar > $(distdir).tar.$3 \
	  || { rm -f $(distdir).tar.$3; \
	       echo "am-optional: dist-$1 failed; archive not built" >&2; }; \
	else \
	  echo "am-optional: $1 unavailable, skipping $(distdir).tar.$3" >&2; \
	fi; \
	:], [$(distdir).tar.$3])dnl
])


# _AM_OPTIONAL_ZIP
# ----------------
AC_DEFUN([_AM_OPTIONAL_ZIP],
[_AM_OPTIONAL_RULE([zip], [zip],
[am--optional-dist-zip: distdir
	@if test -n "$(am__optional_zip)"; then \
	  rm -f $(distdir).zip; \
	  { "$(am__optional_zip)" -rq $(distdir).zip $(distdir); } \
	  || { rm -f $(distdir).zip; \
	       echo "am-optional: dist-zip failed; archive not built" >&2; }; \
	else \
	  echo "am-optional: zip unavailable, skipping $(distdir).zip" >&2; \
	fi; \
	:], [$(distdir).zip])dnl
])


# _AM_OPTIONAL_SHAR
# -----------------
# Only `shar' is optional (gzip is a hard automake prereq).  shar
# writes an intermediate file instead of piping into gzip: piped, gzip
# would succeed on empty input and leave a bogus .shar.gz when shar
# failed; the `&&' chain catches shar's exit instead.
AC_DEFUN([_AM_OPTIONAL_SHAR],
[_AM_OPTIONAL_RULE([shar], [shar],
[am--optional-dist-shar: distdir
	@if test -n "$(am__optional_shar)"; then \
	  rm -f $(distdir).shar $(distdir).shar.gz; \
	  { "$(am__optional_shar)" $(distdir) > $(distdir).shar \
	    && eval GZIP= gzip $(GZIP_ENV) -f $(distdir).shar; } \
	  || { rm -f $(distdir).shar $(distdir).shar.gz; \
	       echo "am-optional: dist-shar failed; archive not built" >&2; }; \
	else \
	  echo "am-optional: shar unavailable, skipping $(distdir).shar.gz" >&2; \
	fi; \
	:], [$(distdir).shar $(distdir).shar.gz])dnl
])
