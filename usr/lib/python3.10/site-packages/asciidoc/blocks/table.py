import copy
import re
from typing import Optional, Tuple

ALIGN = {'<': 'left', '>': 'right', '^': 'center'}
VALIGN = {'<': 'top', '>': 'bottom', '^': 'middle'}


def parse_align_spec(align_spec: Optional[str]) -> Tuple[Optional[str], Optional[str]]:
    """
    Parse AsciiDoc cell alignment specifier and return 2-tuple with
    horizontal and vertical alignment names. Unspecified alignments
    set to None.
    """
    result = (None, None)
    if align_spec:
        mo = re.match(r'^([<\^>])?(\.([<\^>]))?$', align_spec)
        if mo:
            result = (
                ALIGN.get(mo.group(1)),
                VALIGN.get(mo.group(3)),
            )
    return result


# TODO: remove _table_ from name once Table class has been moved into this file
def parse_table_span_spec(span_spec: Optional[str]) -> Tuple[int, int]:
    """
    Parse AsciiDoc cell span specifier and return 2-tuple with horizontal
    and vertical span counts. Set default values (1,1) if not
    specified.
    """
    result = (None, None)
    if span_spec:
        mo = re.match(r'^(\d+)?(\.(\d+))?$', span_spec)
        if mo:
            result = (
                mo.group(1) and int(mo.group(1)),
                mo.group(3) and int(mo.group(3)),
            )
    return (result[0] or 1, result[1] or 1)


class Column:
    """Table column."""
    def __init__(self, width=None, align_spec=None, style=None):
        self.width = width or '1'
        self.halign, self.valign = parse_align_spec(align_spec)
        self.style = style      # Style name or None.
        # Calculated attribute values.
        self.abswidth = None    # 1..   (page units).
        self.pcwidth = None     # 1..99 (percentage).


class Cell:
    def __init__(self, data, span_spec=None, align_spec=None, style=None):
        self.data = data
        self.span, self.vspan = parse_table_span_spec(span_spec)
        self.halign, self.valign = parse_align_spec(align_spec)
        self.style = style
        self.reserved = False

    def __repr__(self):
        return '<Cell: %d.%d %s.%s %s "%s">' % (
            self.span, self.vspan,
            self.halign, self.valign,
            self.style or '',
            self.data)

    def clone_reserve(self):
        """Return a clone of self to reserve vertically spanned cell."""
        result = copy.copy(self)
        result.vspan = 1
        result.reserved = True
        return result
