#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Simple test runner so the tests can be run with

python -m mtspec.tests

:copyright:
    Lion Krischer (krischer@geophysik.uni-muenchen.de), 2012-2015
:license:
    GNU General Public License, Version 3
    (http://www.gnu.org/copyleft/gpl.html)
"""
from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

from mtspec.tests import test_multitaper, test_code_formatting
import unittest


def suite():
    suite = unittest.TestSuite()
    suite.addTest(test_multitaper.suite())
    suite.addTest(test_code_formatting.suite())
    return suite


if __name__ == '__main__':
    unittest.main(defaultTest='suite')
