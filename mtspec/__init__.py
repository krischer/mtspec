#! /usr/bin/env python
# -*- coding: utf-8 -*-
"""
A Python wrapper for the multitaper library of German A. Prieto (see link_).

.. _link: http://wwwprof.uniandes.edu.co/~gprieto/software/mwlib.html.

:copyright:
    Lion Krischer (krischer@geophysik.uni-muenchen.de) and
    Moritz Beyreuther, 2010-2016
:license:
    GNU General Public License, Version 3
    (http://www.gnu.org/copyleft/gpl.html)
"""
import inspect
import os

from .multitaper import mtspec, sine_psd, dpss  # NOQA
from .multitaper import wigner_ville_spectrum, mt_coherence  # NOQA
from .multitaper import mt_deconv  # NOQA

path = os.path.abspath(os.path.dirname(
    inspect.getfile(inspect.currentframe())))
with open(os.path.join(path, "VERSION.txt"), "rt") as fh:
    __version__ = fh.read().strip()
