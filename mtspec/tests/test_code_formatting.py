#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Tests all Python files of the project with flake8. This ensure PEP8 conformance
and some other sanity checks as well.

:copyright:
    Lion Krischer (krischer@geophysik.uni-muenchen.de), 2015
:license:
    GNU General Public License, Version 3
    (http://www.gnu.org/copyleft/gpl.html)
"""
from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

import flake8
import flake8.engine
import flake8.main
import inspect
import os
import unittest
import warnings


class CodeFormattingTestCase(unittest.TestCase):
    """
    Test suite enforcing the code formatting.
    """
    def test_flake8(self):
        if flake8.__version__ <= "2":
            msg = ("Module was designed to be tested with flake8 >= 2.0. "
                   "Please update.")
            warnings.warn(msg)
        test_dir = os.path.dirname(os.path.abspath(inspect.getfile(
            inspect.currentframe())))
        mtspec_dir = os.path.dirname(test_dir)

        # Ignore automatically generated files.
        ignore_files = [os.path.join("gui", "qt_window.py")]
        ignore_files = [os.path.join(mtspec_dir, _i) for _i in ignore_files]
        files = []
        for dirpath, _, filenames in os.walk(mtspec_dir):
            filenames = [_i for _i in filenames if
                         os.path.splitext(_i)[-1] == os.path.extsep + "py"]
            if not filenames:
                continue
            for py_file in filenames:
                full_path = os.path.join(dirpath, py_file)
                if full_path in ignore_files:
                    continue
                files.append(full_path)

        # Get the style checker with the default style.
        flake8_style = flake8.engine.get_style_guide(
            parse_argv=False, config_file=flake8.main.DEFAULT_CONFIG)

        report = flake8_style.check_files(files)

        # Make sure at least 8 files are tested. This happens to be the number
        # of python files at the time of writing this test case.
        self.assertTrue(report.counters["files"] >= 8)
        # And no errors occured.
        self.assertEqual(report.get_count(), 0)


def suite():
    return unittest.makeSuite(CodeFormattingTestCase, 'test')


if __name__ == '__main__':
    unittest.main(defaultTest='suite')
