# inplace --- load and invoke the inplace extension.

@load "inplace"

# Please set INPLACE_SUFFIX to make a backup copy.  For example, you may
# want to set INPLACE_SUFFIX to .bak on the command line or in a BEGIN rule.

# N.B. We call inplace_end() in the BEGINFILE and END rules so that any
# actions in an ENDFILE rule will be redirected as expected.

BEGINFILE {
    if (_inplace_filename != "")
        inplace_end(_inplace_filename, INPLACE_SUFFIX)
    inplace_begin(_inplace_filename = FILENAME, INPLACE_SUFFIX)
}

END {
    inplace_end(FILENAME, INPLACE_SUFFIX)
}
