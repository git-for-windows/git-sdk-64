#!/usr/bin/env python3
"""
NAME
    unwraplatex - Removes delimiters from LaTeX source text

SYNOPSIS
    latex2img STDIN

DESCRIPTION
    This filter reads LaTeX source text from STDIN and removes the
    surrounding \\[ and \\] delimiters.
"""

import re
import sys

sys.stdout.write(
    re.sub(
        r"(?s)\A(?:\\\[\s*)?(.*?)(?:\\\])?\Z", "\\1",
        sys.stdin.read().rstrip()
    )
)
# NOTE append endline in result to prevent 'no output from filter' warning
sys.stdout.write("\n")
