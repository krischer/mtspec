# -*- coding: utf-8 -*-

from mtspec.tests import test_multitaper, test_recreatepaperfigures
import unittest

def suite():
    suite = unittest.TestSuite()
    suite.addTest(test_multitaper.suite())
    suite.addTest(test_recreatepaperfigures.suite())
    return suite

if __name__ == '__main__':
    unittest.main(defaultTest='suite')
