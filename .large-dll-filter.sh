#!/bin/bash

# Keep large DLLs packed in Git, with optional unpacking in the worktree.

set -euo pipefail

usage () {
	printf '%s\n' \
		"Usage: $0 <clean|smudge>" \
		"       $0 setup [--no-refresh]" \
		"       $0 --help|-h|-?" \
		"Setup configures local filters and unpacks existing attributed DLLs." \
		"--no-refresh configures only; unpacking trades disk space for speed."
}

operation="${1-}"
case "$operation" in
--help|-h|'-?') usage; exit 0 ;;
setup)
	shift
	refresh=true
	for argument
	do
		case "$argument" in
		--no-refresh) refresh=false ;;
		--help|-h|'-?') usage; exit 0 ;;
		*) usage >&2; exit 1 ;;
		esac
	done
	cd "$(git rev-parse --show-toplevel)"
	;;
clean|smudge) test "$#" = 1 || { usage >&2; exit 1; } ;;
*) usage >&2; exit 1 ;;
esac

export PATH="$PWD/ucrt64/bin:$PWD/clangarm64/bin:$PATH"

if test "$operation" = setup
then
	git config --local filter.upx.clean 'bash ./.large-dll-filter.sh clean'
	git config --local filter.upx.smudge 'bash ./.large-dll-filter.sh smudge'
	git config --local filter.upx.required true
	test "$refresh" = true || exit 0
	git -c core.quotePath=false ls-files |
	git -c core.quotePath=false check-attr --stdin filter |
	sed -n 's/: filter: upx$//p' |
	while IFS= read -r path
	do
		test -f "$path" && ! test -L "$path" || continue
		if upx --no-env -t "$path" >/dev/null 2>&1
		then
			upx --no-env -d "$path" >&2
		else
			status=$?
			case "$status" in 1|2) ;; *) exit "$status" ;; esac
		fi
	done
	exit 0
fi

umask 077
directory="$(mktemp -d "${TMPDIR:-/tmp}/git-upx.XXXXXX")"
trap 'rm -f "$directory/input.dll" "$directory/output.dll"; rmdir "$directory"' 0
trap 'exit 1' HUP INT TERM

cat >"$directory/input.dll"

case "$operation" in
clean)
	# Leave already-packed input unchanged.
	if upx --no-env -t "$directory/input.dll" >/dev/null 2>&1
	then
		cat "$directory/input.dll"
		exit 0
	fi
	upx --no-env --exact -o "$directory/output.dll" \
		"$directory/input.dll" >&2
	;;
smudge)
	if ! upx --no-env -t "$directory/input.dll" >/dev/null 2>&1
	then
		# Validate unpacked input before passing it through.
		upx --no-env --exact -o "$directory/output.dll" \
			"$directory/input.dll" >&2
		cat "$directory/input.dll"
		exit 0
	fi
	upx --no-env -d -o "$directory/output.dll" \
		"$directory/input.dll" >&2
	;;
esac

cat "$directory/output.dll"
