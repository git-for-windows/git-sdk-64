from . import asciidoc
from .exceptions import AsciiDocError


class Options(object):
    """
    Stores asciidoc(1) command options.
    """
    def __init__(self, values=[]):
        self.values = values[:]

    def __call__(self, name, value=None):
        """Shortcut for append method."""
        self.append(name, value)

    def append(self, name, value=None):
        if type(value) in (int, float):
            value = str(value)
        self.values.append((name, value))


class AsciiDocAPI(object):
    """
    AsciiDoc API class.
    """
    def __init__(self, asciidoc_py=None):
        """
        Locate and import asciidoc.py.
        Initialize instance attributes.
        """
        self.options = Options()
        self.attributes = {}
        self.messages = []
        self.cmd = 'asciidoc'

    def execute(self, infile, outfile=None, backend=None):
        """
        Compile infile to outfile using backend format.
        infile can outfile can be file path strings or file like objects.
        """
        self.messages = []
        opts = Options(self.options.values)
        if outfile is not None:
            opts('--out-file', outfile)
        if backend is not None:
            opts('--backend', backend)
        for k, v in self.attributes.items():
            if v == '' or k[-1] in '!@':
                s = k
            elif v is None:  # A None value undefines the attribute.
                s = k + '!'
            else:
                s = '%s=%s' % (k, v)
            opts('--attribute', s)
        args = [infile]
        try:
            try:
                asciidoc.execute(self.cmd, opts.values, args)
            finally:
                self.messages = asciidoc.messages[:]
        except SystemExit as e:
            if e.code:
                raise AsciiDocError(self.messages[-1])
