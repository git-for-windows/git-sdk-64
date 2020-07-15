#!/usr/bin/env python3

'''
NAME
    graphviz2png - Converts textual graphviz notation to PNG file

SYNOPSIS
    graphviz2png [options] INFILE

DESCRIPTION
    This filter reads Graphviz notation text from the input file
    INFILE (or stdin if INFILE is -), converts it to a PNG image file.


OPTIONS
    -o OUTFILE, --outfile=OUTFILE
        The file name of the output file. If not specified the output file is
        named like INFILE but with a .png file name extension.

    -L LAYOUT, --layout=LAYOUT
        Graphviz layout: dot, neato, twopi, circo, fdp
        Default is 'dot'.

    -F FORMAT, --format=FORMAT
        Graphviz output format: png, svg, or any other format Graphviz
        supports. Run dot -T? to get the full list.
        Default is 'png'.

    -v, --verbose
        Verbosely print processing information to stderr.

    -h, --help
        Print this documentation.

    -V, --version
        Print program version number.

SEE ALSO
    graphviz(1)

AUTHOR
    Written by Gouichi Iisaka, <iisaka51@gmail.com>
    Format support added by Elmo Todurov, <todurov@gmail.com>

THANKS
    Stuart Rackham, <srackham@gmail.com>
    This script was inspired by his music2png.py and AsciiDoc

LICENSE
    Copyright (C) 2008-2009 Gouichi Iisaka.
    Free use of this software is granted under the terms of
    the GNU General Public License (GPL).
'''

import os
import sys
import subprocess
import argparse

__AUTHOR__ = "Gouichi Iisaka <iisaka51@gmail.com>"
__VERSION__ = '1.1.5'


class EApp(Exception):
    '''Application specific exception.'''
    pass


class Application():
    def __init__(self, argv=None):
        # Run dot, get the list of supported formats. It's prefixed by some junk.
        format_output = subprocess.Popen(
            ["dot", "-T?"],
            stderr=subprocess.PIPE,
            stdout=subprocess.PIPE
        ).communicate()[1].decode('utf-8')
        # The junk contains : and ends with :. So we split it, then strip the
        # final endline, then split the list for future usage.
        supported_formats = format_output.split(": ")[2][:-1].split(" ")

        if not argv:
            argv = sys.argv

        self.usage = '%(prog)s [options] infile'
        self.version = 'Version: %s\n' % __VERSION__
        self.version += 'Copyright(c) 2008-2009: %s\n' % __AUTHOR__

        self.parser = argparse.ArgumentParser(usage=self.usage)
        self.parser.add_argument(
            "-o", "--outfile", action="store", dest="outfile", help="Output file"
        )
        self.parser.add_argument(
            "-L", "--layout", action="store", dest="layout", default="dot",
            choices=['dot', 'neato', 'twopi', 'circo', 'fdp'], help="Layout type"
        )
        self.parser.add_argument(
            "-F", "--format", action="store", dest="format", default="png",
            choices=supported_formats, help="Format type"
        )
        self.parser.add_argument(
            "--debug", action="store_true", dest="do_debug", help=argparse.SUPPRESS
        )
        self.parser.add_argument(
            "-v", "--verbose",
            action="store_true", dest="do_verbose", default=False,
            help="verbose output"
        )
        self.parser.add_argument("infile", action="store", help="Input file")
        self.parser.add_argument('--version', action='version', version=self.version)
        self.options = self.parser.parse_args()

    def systemcmd(self, cmd):
        if self.options.do_verbose:
            msg = 'Execute: %s' % cmd
            sys.stderr.write(msg + os.linesep)
        else:
            cmd += ' 2>%s' % os.devnull
        if os.system(cmd):
            raise EApp('failed command: %s' % cmd)

    def graphviz2png(self, infile, outfile):
        '''Convert Graphviz notation in file infile to
           PNG file named outfile.'''

        outfile = os.path.abspath(outfile)
        outdir = os.path.dirname(outfile)

        if not os.path.isdir(outdir):
            raise EApp('directory does not exist: %s' % outdir)

        saved_cwd = os.getcwd()
        os.chdir(outdir)
        try:
            cmd = '%s -T%s "%s" > "%s"' % (
                self.options.layout,
                self.options.format,
                infile,
                outfile
            )
            self.systemcmd(cmd)
        finally:
            os.chdir(saved_cwd)

        if not self.options.do_debug:
            os.unlink(infile)

    def run(self):
        if self.options.format == '':
            self.options.format = 'png'

        infile = self.options.infile
        if self.options.infile == '-':
            if self.options.outfile is None:
                sys.stderr.write('OUTFILE must be specified')
                sys.exit(1)
            infile = os.path.splitext(self.options.outfile)[0] + '.txt'
            lines = sys.stdin.readlines()
            open(infile, 'w').writelines(lines)

        if not os.path.isfile(infile):
            raise EApp('input file does not exist: %s' % infile)

        if self.options.outfile is None:
            outfile = os.path.splitext(infile)[0] + '.png'
        else:
            outfile = self.options.outfile

        self.graphviz2png(infile, outfile)

        # To suppress asciidoc 'no output from filter' warnings.
        if self.options.infile == '-':
            sys.stdout.write(' ')


if __name__ == "__main__":
    app = Application()
    app.run()
