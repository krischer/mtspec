# -*- coding: utf-8 -*-

from mtspec import mtspec, sine_psd
import numpy as np
import os
import unittest


class MtSpecTestCase(unittest.TestCase):
    """
    Test suite for mtspec.
    """

    def test_singleDPSSTaperSpectrum(self):
        """
        Test for single DPSS taper spectrum. The result is compared to the output of
        test_recreatePaperFigures.py in the same directory. This is assumed to
        be correct because they are identical to the figures in the paper on
        the machine that created these.
        """
        datafile = os.path.join('data', 'PASC.dat')
        data = np.loadtxt(datafile)
        # Calculate the spectra.
        spec, freq = mtspec(data, 1.0, 1.5, number_of_tapers = 1)
        # Load the good data.
        datafile = os.path.join('data', 'single_taper.npz')
        files = np.load(datafile)
        spec2 = files['spec']
        freq2 = files['freq']
        # Compare.
        np.testing.assert_almost_equal(freq, freq2)
        np.testing.assert_almost_equal(spec, spec2)

    def test_multitaperSpectrum(self):
        """
        Test for mtspec. The result is compared to the output of
        test_recreatePaperFigures.py in the same directory. This is assumed to
        be correct because they are identical to the figures in the paper on
        the machine that created these.
        """
        datafile = os.path.join('data', 'PASC.dat')
        data = np.loadtxt(datafile)
        # Calculate the spectra.
        spec, freq = mtspec(data, 1.0, 4.5, number_of_tapers = 5)
        # Load the good data.
        datafile = os.path.join('data', 'multitaper.npz')
        files = np.load(datafile)
        spec2 = files['spec']
        freq2 = files['freq']
        # Compare.
        np.testing.assert_almost_equal(freq, freq2)
        np.testing.assert_almost_equal(spec, spec2)

    def test_quadraticMultitaperSpectrum(self):
        """
        Test for quadratic mtspec. The result is compared to the output of
        test_recreatePaperFigures.py in the same directory. This is assumed to
        be correct because they are identical to the figures in the paper on
        the machine that created these.
        """
        datafile = os.path.join('data', 'PASC.dat')
        data = np.loadtxt(datafile)
        # Calculate the spectra.
        spec, freq = mtspec(data, 1.0, 4.5, number_of_tapers = 5,
                            quadratic = True)
        # Load the good data.
        datafile = os.path.join('data', 'quadratic_multitaper.npz')
        files = np.load(datafile)
        spec2 = files['spec']
        freq2 = files['freq']
        # Compare.
        np.testing.assert_almost_equal(freq, freq2)
        np.testing.assert_almost_equal(spec, spec2)

    def test_sinePSD(self):
        """
        Test for the sine_psd spectra. The result is compared to the output of
        test_recreatePaperFigures.py in the same directory. This is assumed to
        be correct because they are identical to the figures in the paper on
        the machine that created these.
        """
        datafile = os.path.join('data', 'PASC.dat')
        data = np.loadtxt(datafile)
        # Calculate the spectra.
        spec, freq = sine_psd(data, 1.0)
        # Load the good data.
        datafile = os.path.join('data', 'sine_psd.npz')
        files = np.load(datafile)
        spec2 = files['spec']
        freq2 = files['freq']
        # Compare.
        np.testing.assert_almost_equal(freq, freq2)
        np.testing.assert_almost_equal(spec, spec2)

    def test_quadraticMultitaperIsDifferent(self):
        """
        The quadratic and the normal multitaper spectra look quite similar.
        Check that they are different.
        """
        datafile = os.path.join('data', 'PASC.dat')
        data = np.loadtxt(datafile)
        # Calculate the spectra.
        spec, freq = mtspec(data, 1.0, 4.5, number_of_tapers = 2)
        spec2, freq2 = mtspec(data, 1.0, 4.5, number_of_tapers = 2,
                              quadratic = True)
        # Test that these are not equal.
        self.assertRaises(AssertionError, np.testing.assert_almost_equal,
                          spec, spec2)


def suite():
    return unittest.makeSuite(MtSpecTestCase, 'test')


if __name__ == '__main__':
    unittest.main(defaultTest='suite')
