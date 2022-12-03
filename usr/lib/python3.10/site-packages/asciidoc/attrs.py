import re
import typing

from . import get_compat_mode
from .utils import get_args, get_kwargs


def parse_attributes(attrs: str, output_dict: typing.Dict) -> None:
    """Update a dictionary with name/value attributes from the attrs string.
    The attrs string is a comma separated list of values and keyword name=value
    pairs. Values must precede keywords and are named '1','2'... The entire
    attributes list is named '0'. If keywords are specified string values must
    be quoted. Examples:

    attrs: ''
    output_dict: {}

    attrs: 'hello,world'
    output_dict: {'0': 'hello,world', '1': 'hello', '2': 'world',}

    attrs: '"hello", planet="earth"'
    output_dict: {'0': '"hello", planet="earth"', '1': 'hello' 'planet': 'earth', }
    """
    if not attrs:
        return
    output_dict['0'] = attrs
    # Replace line separators with spaces so line spanning works.
    s = re.sub(r'\s', ' ', attrs)
    d = legacy_parse(s) if get_compat_mode() == 1 else future_parse(s)
    output_dict.update(d)
    assert len(d) > 0


def future_parse(s: str) -> dict:
    d = {}
    key = ''
    value = ''
    count = 1
    quote = None
    in_quotes = False
    had_quotes = False

    def add_value():
        nonlocal count, d, key, value
        key = key.strip()
        value = value.strip()
        if had_quotes:
            value = value[1:-1]

        if not value and not had_quotes:
            value = None

        if key:
            d[key] = value if value else ''
            key = ''
        else:
            d["{}".format(count)] = value
        count += 1
        value = ''

    for i in range(len(s)):
        char = s[i]

        if char == ',' and not in_quotes:
            add_value()
            had_quotes = False
        elif value and char == '=' and not in_quotes:
            key = value
            value = ''
        elif not in_quotes and (char == '"' or char == "'") \
                and (i == 0 or s[i - 1] != '\\'):
            in_quotes = True
            quote = char
            value += char
        elif in_quotes and char == quote and (i == 0 or s[i - 1] != '\\'):
            in_quotes = False
            had_quotes = True
            quote = None
            value += char
        elif char == '\\' and i < len(s) - 1 and (s[i + 1] == '"' or s[i + 1] == "'"):
            pass
        else:
            value += char

    if key and key[0] == '=' and not value:
        value = key + "="
        key = ""

    if not value and s.rstrip()[-1] == ',':
        value = ' '

    if had_quotes or value or key:
        add_value()
    return d


def legacy_parse(s: str) -> dict:
    d = {}
    try:
        d.update(get_args(s))
        d.update(get_kwargs(s))
        for v in list(d.values()):
            if not (isinstance(v, str)
                    or isinstance(v, int) or isinstance(v, float) or v is None):
                raise Exception
    except Exception:
        s = s.replace('"', '\\"')
        s = s.split(',')
        s = ['"' + x.strip() + '"' for x in s]
        s = ','.join(s)
        try:
            d = {}
            d.update(get_args(s))
            d.update(get_kwargs(s))
        except Exception:
            return  # If there's a syntax error leave with {0}=attrs.
        for k in list(d.keys()):  # Drop any empty positional arguments.
            if d[k] == '':
                del d[k]
    return d
