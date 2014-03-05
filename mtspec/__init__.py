# -*- coding: utf-8 -*-
"""
A Python wrapper for the multitaper library of German A. Prieto (see link_).

:copyright: Moritz Beyreuther and Lion Krischer
:license: GNU General Public License (GPLv2)

.. _link: http://wwwprof.uniandes.edu.co/~gprieto/software/mwlib.html.
"""

from .multitaper import mtspec, sine_psd, dpss, wigner_ville_spectrum, mt_coherence
