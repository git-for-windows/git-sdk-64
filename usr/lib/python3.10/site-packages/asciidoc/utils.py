import ast
import math
import os
import re
import time
from typing import List, Optional, Tuple, Union
import unicodedata


def userdir() -> Optional[str]:
    """
    Return user's home directory or None if it is not defined.
    """
    result = os.path.expanduser('~')
    if result == '~':
        result = None
    return result


def file_in(fname, directory) -> bool:
    """Return True if file fname resides inside directory."""
    assert os.path.isfile(fname)
    # Empty directory (not to be confused with None) is the current directory.
    if directory == '':
        directory = os.getcwd()
    else:
        assert os.path.isdir(directory)
        directory = os.path.realpath(directory)
    fname = os.path.realpath(fname)
    return os.path.commonprefix((directory, fname)) == directory


def assign(dst, src):
    """Assign all attributes from 'src' object to 'dst' object."""
    for a, v in list(src.__dict__.items()):
        setattr(dst, a, v)


def strip_quotes(s: str) -> str:
    """Trim white space and, if necessary, quote characters from s."""
    s = s.strip()
    # Strip quotation mark characters from quoted strings.
    if len(s) >= 3 and s[0] == '"' and s[-1] == '"':
        s = s[1:-1]
    return s


def is_re(s) -> bool:
    """Return True if s is a valid regular expression else return False."""
    try:
        re.compile(s)
        return True
    except BaseException:
        return False


def re_join(relist: List) -> List:
    """Join list of regular expressions re1,re2,... to single regular
    expression (re1)|(re2)|..."""
    if len(relist) == 0:
        return None
    result = []
    # Delete named groups to avoid ambiguity.
    for s in relist:
        result.append(re.sub(r'\?P<\S+?>', '', s))
    result = ')|('.join(result)
    result = '(' + result + ')'
    return result


def lstrip_list(s: Union[List, Tuple]) -> Union[List, Tuple]:
    """
    Return list with empty items from start of list removed.
    """
    for i in range(len(s)):
        if s[i]:
            break
    else:
        return []
    return s[i:]


def rstrip_list(s: Union[List, Tuple]) -> Union[List, Tuple]:
    """
    Return list with empty items from end of list removed.
    """
    for i in range(len(s) - 1, -1, -1):
        if s[i]:
            break
    else:
        return []
    return s[:i + 1]


def strip_list(s: Union[List, Tuple]) -> Union[List, Tuple]:
    """
    Return list with empty items from start and end of list removed.
    """
    s = lstrip_list(s)
    s = rstrip_list(s)
    return s


def is_array(obj) -> bool:
    """
    Return True if object is list or tuple type.
    """
    return isinstance(obj, list) or isinstance(obj, tuple)


def dovetail(lines1, lines2):
    """
    Append list or tuple of strings 'lines2' to list 'lines1'.  Join the last
    non-blank item in 'lines1' with the first non-blank item in 'lines2' into a
    single string.
    """
    assert is_array(lines1)
    assert is_array(lines2)
    lines1 = strip_list(lines1)
    lines2 = strip_list(lines2)
    if not lines1 or not lines2:
        return list(lines1) + list(lines2)
    result = list(lines1[:-1])
    result.append(lines1[-1] + lines2[0])
    result += list(lines2[1:])
    return result


def dovetail_tags(stag, content, etag):
    """Merge the end tag with the first content line and the last
    content line with the end tag. This ensures verbatim elements don't
    include extraneous opening and closing line breaks."""
    return dovetail(dovetail(stag, content), etag)


def py2round(n: Union[float, int], d: int = 0) -> int:
    """Utility function to get python2 rounding in python3. Python3 changed it such that
    given two equally close multiples, it'll round towards the even choice. For example,
    round(42.5) == 42 instead of the expected round(42.5) == 43). This function gives us
    back that functionality."""
    p = 10 ** d
    return float(math.floor((n * p) + math.copysign(0.5, n))) / p


east_asian_widths = {
    'W': 2,   # Wide
    'F': 2,   # Full-width (wide)
    'Na': 1,  # Narrow
    'H': 1,   # Half-width (narrow)
    'N': 1,   # Neutral (not East Asian, treated as narrow)
    'A': 1,   # Ambiguous (s/b wide in East Asian context, narrow otherwise, but that
              #   doesn't work)
}
"""Mapping of result codes from `unicodedata.east_asian_width()` to character
column widths."""


def column_width(s: str) -> int:
    width = 0
    for c in s:
        width += east_asian_widths[unicodedata.east_asian_width(c)]
    return width


def date_time_str(t: float) -> Tuple[str, str]:
    """Convert seconds since the Epoch to formatted local date and time strings."""
    source_date_epoch = os.environ.get('SOURCE_DATE_EPOCH')
    if source_date_epoch is not None:
        t = time.gmtime(min(t, int(source_date_epoch)))
    else:
        t = time.localtime(t)
    date_str = time.strftime('%Y-%m-%d', t)
    time_str = time.strftime('%H:%M:%S', t)
    if source_date_epoch is not None:
        time_str += ' UTC'
    elif time.daylight and t.tm_isdst == 1:
        time_str += ' ' + time.tzname[1]
    else:
        time_str += ' ' + time.tzname[0]
    return date_str, time_str


def get_args(val):
    d = {}
    args = ast.parse("d(" + val + ")", mode='eval').body.args
    i = 1
    for arg in args:
        if isinstance(arg, ast.Name):
            d[str(i)] = ast.literal_eval(arg.id)
        else:
            d[str(i)] = ast.literal_eval(arg)
        i += 1
    return d


def get_kwargs(val):
    d = {}
    args = ast.parse("d(" + val + ")", mode='eval').body.keywords
    for arg in args:
        d[arg.arg] = ast.literal_eval(arg.value)
    return d


def parse_to_list(val):
    values = ast.parse("[" + val + "]", mode='eval').body.elts
    return [ast.literal_eval(v) for v in values]
