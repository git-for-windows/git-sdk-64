"""asciidoc module"""

import sys
from .__metadata__ import VERSION, __version__

__all__ = [
    'VERSION',
    '__version__',
    'set_legacy_compat',
    'set_future_compat',
    'set_compat_mode',
    'get_compat_mode',
]

COMPAT_MODE = 1


def set_legacy_compat() -> None:
    set_compat_mode(1)


def set_future_compat() -> None:
    set_compat_mode(2)


def set_compat_mode(mode: int) -> None:
    if mode < 1 or mode > 2:
        raise ValueError('compat mode must be 1 <= mode <= 2')

    global COMPAT_MODE
    COMPAT_MODE = mode


def get_compat_mode() -> int:
    return COMPAT_MODE


# If running as a script, we avoid these imports to avoid a circular
# RuntimeWarning, which is fine as we don't use them in that case.
if "-m" not in sys.argv:
    from .api import AsciiDocAPI
    from .asciidoc import execute, cli
    __all__ += ['AsciiDocAPI', 'execute', 'cli']
