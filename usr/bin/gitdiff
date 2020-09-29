#!/usr/bin/python3
# -*- coding: utf-8 -*-
#
# Copyright (C) 2015-2019 Sérgio Basto <sergio@serjux.com>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
#

import os
import sys
import argparse
from subprocess import Popen, PIPE

enviro = os.environ
workdir = '.'

parser = argparse.ArgumentParser()
parser.add_argument('-v', '--debug',
                    help='writes the commands that will be executed',
                    action='store_true')
parser.add_argument('git_args', nargs='*', default=[])
parser.add_argument('patchview_args', nargs=argparse.REMAINDER)
args, unknown = parser.parse_known_args()
largs = vars(args).get("git_args")
rargs = vars(args).get("patchview_args")

if args.debug:
    print("%s | %s" % (" ".join(['git', 'diff'] + largs),
          " ".join(['patchview'] + rargs + unknown)))

p1 = Popen(['git', 'diff'] + largs, stdout=PIPE, env=enviro, cwd=workdir)
p2 = Popen(['patchview'] + rargs + unknown, stdin=p1.stdout, stdout=PIPE,
           env=enviro, cwd=workdir)
p1.stdout.close()  # Allow p1 to receive a SIGPIPE if p2 exits.
sys.stdout.buffer.write(p2.communicate()[0])
