#! /usr/bin/gawk -f

# gensub2 --- Do what gensub() does, but also set NSUB0 to the
# number of substitutions made.  This is useful for prototyping
# and exploration.
#
# The implementation relies on a number of features specific to gawk.

# For production, the print statements and assignments to `target'
# should be commented out.

@namespace "gensub2"

BEGIN {
    target = "abc"
    print target, "--->", awk::gensub2("b+", "FOO", "g", target), ", NSUB0 =", awk::NSUB0, awk::gensub("b+", "FOO", "g", target), "NSUB =", PROCINFO["nsub"]
    print target, "--->", awk::gensub2("q", "FOO", "g", target), ", NSUB0 =", awk::NSUB0, awk::gensub("q", "FOO", "g", target), "NSUB =", PROCINFO["nsub"]
    print target, "--->", awk::gensub2("b", "X", 2, target), ", NSUB0 =", awk::NSUB0, awk::gensub("b", "X", 2, target), "NSUB =", PROCINFO["nsub"]
    print target, "--->", awk::gensub2("x*", "X", "g", target), ", NSUB0 =", awk::NSUB0, awk::gensub("x*", "X", "g", target), "NSUB =", PROCINFO["nsub"]
    print target, "--->", awk::gensub2("b*", "X", "g", target), ", NSUB0 =", awk::NSUB0, awk::gensub("b*", "X", "g", target), "NSUB =", PROCINFO["nsub"]
    print target, "--->", awk::gensub2("c", "X", "g", target), ", NSUB0 =", awk::NSUB0, awk::gensub("c", "X", "g", target), "NSUB =", PROCINFO["nsub"]
    print target, "--->", awk::gensub2("c+", "X", "g", target), ", NSUB0 =", awk::NSUB0, awk::gensub("c+", "X", "g", target), "NSUB =", PROCINFO["nsub"]
    print target, "--->", awk::gensub2("x*$", "X", "g", target), ", NSUB0 =", awk::NSUB0, awk::gensub("x*$", "X", "g", target), "NSUB =", PROCINFO["nsub"]
    print target, "--->", awk::gensub2("(a)(b)(c)", "& - \\3\\2\\1 - &", "g", target), ", NSUB0 =", awk::NSUB0, awk::gensub("(a)(b)(c)", "& - \\3\\2\\1 - &", "g", target), "NSUB =", PROCINFO["nsub"]
    $0 = target; print $0, "--->", awk::gensub2("(a)(b)(c)", "& - \\3\\2\\1 - &", "g"), ", NSUB0 =", awk::NSUB0, awk::gensub("(a)(b)(c)", "& - \\3\\2\\1 - &", "g"), "NSUB =", PROCINFO["nsub"]
    target = "abbbc"
    print target, "--->", awk::gensub2("b", "X", 2, target), ", NSUB0 =", awk::NSUB0, awk::gensub("b", "X", 2, target), "NSUB =", PROCINFO["nsub"]
}

function compute_replace(replace, matches,  result, chars, i, j, k)
{
    j = split(replace, chars, "")
    for (i = 1; i <= j; i++) {
        if (chars[i] == "\\") {
            if (index("0123456789", chars[i+1]) != 0) {
                k = chars[i+1] + 0
                result = result matches[k]
            } else {
                result = result chars[i+1]  # literal text
            }
            i++
        } else if (chars[i] == "&") {
            result = result matches[0]  # entire text
        } else {
            result = result chars[i]    # literal text
        }
    }
    return result
}

@namespace "awk"

function gensub2(regex, replace, how, target,       # parameters
                 newstr, eosflag, nonzeroflag, global,  # locals
                 which, count, str, matches, repl,
                 true, false)
{
    true = 1; false = 0
    NSUB0 = 0

    if (typeof(target) == "untyped")
        target = $0

    global = (how == "g" || how == "G")
    which = 1
    if (! global)
        which = how + 0
    if (which <= 0)
        which = 1

    eosflag = nonzeroflag = false
    str = target
    while (match(str, regex, matches)) {
        count++
        if (! global && count != which) {
            newstr = newstr substr(str, 1, RSTART + RLENGTH - 1)
            str = substr(str, RSTART + RLENGTH)
            continue
        }

        repl = gensub2::compute_replace(replace, matches)

        if (RLENGTH > 0) { # easy case
            nonzeroflag = true
            if (RSTART == 1) { # match at front of string
                newstr = newstr repl
            } else {
                newstr = newstr substr(str, 1, RSTART-1) repl
            }
            str = substr(str, RSTART+RLENGTH)
            NSUB0++
        } else if (nonzeroflag) {
            # last match was non-zero in length, and at the
            # current character, we get a zero length match,
            # which we don't really want, so skip over it
            newstr = newstr substr(str, 1, 1)
            str = substr(str, 2)
            nonzeroflag = false
        } else {
            # zero-length match
            NSUB0++
            if (RSTART == 1) {
                newstr = newstr repl substr(str, 1, 1)
                str = substr(str, 2)
            } else {
                return newstr str repl
            }
        }
        if (length(str) == 0) {
            if (eosflag)
                break
            else
                eosflag = true
        }
    }
    if (length(str) > 0)
        newstr = newstr str # rest of string

    return newstr
}
