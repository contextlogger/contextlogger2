#!/usr/bin/env python
# -*- coding: utf-8 -*-

##############################################################################
# Copyright 2006, 2007, 2008, 2009 Jussi Ylänen
#
# This file is part of Ensymble developer utilities for Symbian OS(TM).
#
# Ensymble is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# Ensymble is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Ensymble; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
##############################################################################
"""
actions - Commands used by the ensymble tool
"""
__all__ = ["altere32", "genuid", "infoe32", "mergesis", "py2sis", "signsis",
           "simplesis"]

cmddict = {}
for _ in __all__:
    cmddict[_] = __import__(_, globals(), locals(), [])

from .. import __version__

class Version:
    shorthelp = "Print Ensymble version"
    longhelp  = """version

Print Ensymble version"""

    def run(self, pgmname, argv):
        print __version__

cmddict['version'] = Version()


__all__.append('cmddict')

