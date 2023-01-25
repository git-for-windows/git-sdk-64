#!/bin/sh

# To help Continuous Integration, this script commits the files installed via
# MSYS2 packages into the Git for Windows SDK.

die () {
	echo "$*" >&2
	exit 1
}

summarize_commit () {
	test $# -le 1 ||
	die "summarize_commit: too many arguments ($*)"

	if test -z "$1"
	then
		git diff --cached -M50 --raw -- var/lib/pacman/local/\*/desc
	else
		git show "$1" --format=%H \
			-M15 --raw -- var/lib/pacman/local/\*/desc
	fi |
	sed -ne '/.* M\tvar\/lib\/pacman\/local\/\(mingw-w64-.*-\)\?git-extra-[1-9].*\/desc$/d' \
	 -e '/ R[0-9]*\t/{s/-\([0-9]\)/ (\1/;h;s|-\([0-9][^/]*\)/desc$|\t\1)|;s|.*\t| -> |;x;s|/desc\t.*||;s|.*\t[^\t]*/||;G;s|\n||g;p}' \
	 -e '/ A\t/{s|.*local/\([^/]*\)/desc|\1|;s|-\([0-9].*\)| (new: \1)|p}' \
	 -e '/ D\t/{s|.*local/\([^/]*\)/desc|\1|;s|-\([0-9].*\)| (removed)|p}'
}

git add -A . &&
if git diff-index --exit-code --cached HEAD -- \
	':(exclude)var/lib/pacman/sync/' \
	':(exclude)var/lib/pacman/local/git-extra-*/desc' \
	':(exclude)var/lib/pacman/local/mingw-w64-*-git-extra-*/desc' \
	':(exclude)etc/rebase.db*'
then
	# No changes, really, except maybe a new Pacman db
	exit 0
fi

summary="$(summarize_commit)"
count=$(echo "$summary" | wc -l) &&
if test $count -lt 2
then
	oneline="Update $count package"
else
	oneline="Update $count packages"
fi &&
git commit -q -s -m "$oneline" -m "$summary" ||
die "Could not commit changes"
