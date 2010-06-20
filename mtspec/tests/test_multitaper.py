# -*- coding: utf-8 -*-

from mtspec import mtspec, sine_psd, load_mtdata, dpss
import numpy as np
import os
import unittest


class MtSpecTestCase(unittest.TestCase):
    """
    Test suite for mtspec.
    """

    def test_singleDPSSTaperSpectrum(self):
        """
        Test for single DPSS taper spectrum. The result is compared to the
        output of test_recreatePaperFigures.py in the same directory. This is
        assumed to be correct because they are identical to the figures in the
        paper on the machine that created these.
        """
        data = load_mtdata('PASC.dat.gz')
        # Calculate the spectra.
        spec, freq = mtspec(data, 1.0, 1.5, number_of_tapers=1)
        # Load the good data.
        datafile = os.path.join(os.path.dirname(__file__), 'data',
                                'single_taper.npz')
        spec2 = np.load(datafile)['spec']
        freq2 = np.arange(43201) * 1.15740741e-05
        # Compare, normalize for subdigit comparision
        np.testing.assert_almost_equal(freq, freq2)
        np.testing.assert_almost_equal(spec / spec, spec2 / spec, 5)

    def test_multitaperSpectrum(self):
        """
        Test for mtspec. The result is compared to the output of
        test_recreatePaperFigures.py in the same directory. This is assumed to
        be correct because they are identical to the figures in the paper on
        the machine that created these.
        """
        data = load_mtdata('PASC.dat.gz')
        # Calculate the spectra.
        spec, freq = mtspec(data, 1.0, 4.5, number_of_tapers=5)
        # Load the good data.
        datafile = os.path.join(os.path.dirname(__file__), 'data',
                                'multitaper.npz')
        spec2 = np.load(datafile)['spec']
        freq2 = np.arange(43201) * 1.15740741e-05
        # Compare, normalize for subdigit comparision
        np.testing.assert_almost_equal(freq, freq2)
        np.testing.assert_almost_equal(spec / spec, spec2 / spec, 5)

    def test_multitaperSpectrumOptionalOutput(self):
        """
        Test for mtspec. The result is compared to the output of
        test_recreatePaperFigures.py in the same directory. This is assumed to
        be correct because they are identical to the figures in the paper on
        the machine that created these.
        """
        data = load_mtdata('PASC.dat.gz')
        # Calculate the spectra.
        spec, freq, eigspec, eigcoef, weights = \
                mtspec(data, 1.0, 4.5, number_of_tapers=5,
                optional_output=True)
        #XXX: Verify if this is correct, if so savez the data
        #import matplotlib.pyplot as plt
        #plt.plot(np.abs(eigcoef[:,0]))
        #plt.show()
        #import ipdb; ipdb.set_trace()
        #np.savez('data/multitaper.npz', spec=spec.astype('float32'),
        #         eigspec=eigspec.astype('float32'),
        #         eigcoef=eigcoef.astype('float32'),
        #         weights=weights.astype('float32'))
        # Load the good data.
        datafile = os.path.join(os.path.dirname(__file__), 'data',
                                'multitaper.npz')
        record = np.load(datafile)
        spec2 = record['spec']
        #eigspec2 = record['eigspec']
        #eigcoef2 = record['eigcoef']
        #weights2 = record['weights']
        freq2 = np.arange(43201) * 1.15740741e-05
        # Compare, normalize for subdigit comparision
        np.testing.assert_almost_equal(freq, freq2)
        np.testing.assert_almost_equal(spec / spec, spec2 / spec, 5)
        #np.testing.assert_almost_equal(eigspec/eigspec, eigspec2/eigspec, 5)
        #np.testing.assert_almost_equal(eigcoef/eigcoef, eigcoef2/eigcoef, 5)
        #np.testing.assert_almost_equal(weights/weights, weights2/weights, 5)

    def test_paddedMultitaperSpectrumWithErrors(self):
        """
        Test for mtspec_pad with jackknife interval errors. The result is
        compared to the output of test_recreatePaperFigures.py in the same
        directory. This is assumed to be correct because they are identical to
        the figures in the paper on the machine that created these.
        """
        data = load_mtdata('v22_174_series.dat.gz')
        # Calculate the spectra.
        spec, freq, jackknife, _, _ = mtspec(data, 4930., 3.5, nfft=312,
                                      number_of_tapers=5, statistics=True)
        # Load the good data.
        datafile = os.path.join(os.path.dirname(__file__), 'data',
                                'mtspec_pad_with_errors.npz')
        record = np.load(datafile)
        spec2 = record['spec']
        jackknife2 = record['jackknife']
        freq2 = np.arange(157) * 6.50127447e-07
        # Compare.
        np.testing.assert_almost_equal(freq, freq2)
        np.testing.assert_almost_equal(spec / spec, spec2 / spec, 6)
        np.testing.assert_almost_equal(jackknife / jackknife,
                                       jackknife2 / jackknife, 6)

    def test_quadraticMultitaperSpectrum(self):
        """
        Test for quadratic mtspec. The result is compared to the output of
        test_recreatePaperFigures.py in the same directory. This is assumed to
        be correct because they are identical to the figures in the paper on
        the machine that created these.
        """
        data = load_mtdata('PASC.dat.gz')
        # Calculate the spectra.
        spec, freq = mtspec(data, 1.0, 4.5, number_of_tapers=5,
                            quadratic=True)
        # Load the good data.
        datafile = os.path.join(os.path.dirname(__file__), 'data',
                                'quadratic_multitaper.npz')
        spec2 = np.load(datafile)['spec']
        freq2 = np.arange(43201) * 1.15740741e-05
        # Compare.
        np.testing.assert_almost_equal(freq, freq2)
        np.testing.assert_almost_equal(spec / spec, spec2 / spec, 5)

    def test_fstatisticsAndReshapedSpectrum(self):
        """
        Test for mtspec_pad with jackknife interval errors. The result is
        compared to the output of test_recreatePaperFigures.py in the same
        directory. This is assumed to be correct because they are identical to
        the figures in the paper on the machine that created these.
        """
        data = load_mtdata('v22_174_series.dat.gz')
        # Calculate the spectra.
        spec, freq, jackknife, fstatistics, _ = mtspec(data, 4930., 3.5,
                           nfft=312, number_of_tapers=5, statistics=True,
                           rshape=0, fcrit=0.9)
        # Load the good data.
        datafile = os.path.join(os.path.dirname(__file__), 'data',
                                'fstatistics.npz')
        record = np.load(datafile)
        spec2 = record['spec']
        jackknife2 = record['jackknife']
        fstatistics2 = record['fstatistics']
        freq2 = np.arange(157) * 6.50127447e-07
        # Compare.
        np.testing.assert_almost_equal(freq, freq2)
        np.testing.assert_almost_equal(spec / spec, spec2 / spec)
        np.testing.assert_almost_equal(jackknife / jackknife,
                                       jackknife2 / jackknife, 5)
        np.testing.assert_almost_equal(fstatistics / fstatistics,
                                       fstatistics2 / fstatistics, 5)

    def test_sinePSD(self):
        """
        Test for the sine_psd spectra. The result is compared to the output of
        test_recreatePaperFigures.py in the same directory. This is assumed to
        be correct because they are identical to the figures in the paper on
        the machine that created these.
        """
        data = load_mtdata('PASC.dat.gz')
        # Calculate the spectra.
        spec, freq = sine_psd(data, 1.0)
        # Load the good data.
        datafile = os.path.join(os.path.dirname(__file__), 'data',
                                'sine_psd.npz')
        spec2 = np.load(datafile)['spec']
        freq2 = np.arange(43201) * 1.15740741e-05
        # Compare.
        np.testing.assert_almost_equal(freq, freq2)
        np.testing.assert_almost_equal(spec / spec, spec2 / spec, 2)

    def test_sinePSDStatistics(self):
        """
        Test for the sine_psd spectra with optional output. The result is
        compared to the output of test_recreatePaperFigures.py in the same
        directory. This is assumed to be correct because they are identical
        to the figures in the paper on the machine that created these.
        """
        data = load_mtdata('PASC.dat.gz')
        # Calculate the spectra.
        spec, freq, errors, tapers = sine_psd(data, 1.0, statistics=True)
        #XXX: assert for errors and tapers is missing
        # Load the good data.
        datafile = os.path.join(os.path.dirname(__file__), 'data',
                                'sine_psd.npz')
        spec2 = np.load(datafile)['spec']
        freq2 = np.arange(43201) * 1.15740741e-05
        # Compare #XXX really bad precision for spec (linux 64bit)
        np.testing.assert_almost_equal(freq, freq2)
        np.testing.assert_almost_equal(spec / spec, spec2 / spec, 2)

    def test_quadraticMultitaperIsDifferent(self):
        """
        The quadratic and the normal multitaper spectra look quite similar.
        Check that they are different.
        """
        data = load_mtdata('v22_174_series.dat.gz')
        # Calculate the spectra.
        spec, freq = mtspec(data, 1.0, 4.5, number_of_tapers=2)
        spec2, freq2 = mtspec(data, 1.0, 4.5, number_of_tapers=2,
                              quadratic=True)
        # Test that these are not equal.
        self.assertRaises(AssertionError, np.testing.assert_almost_equal,
                          spec, spec2)
        # Do the same with the mtspec_pad method.
        spec, freq = mtspec(data, 1.0, 4.5, nfft=312, number_of_tapers=2)
        spec2, freq2 = mtspec(data, 1.0, 4.5, nfft=312, number_of_tapers=2,
                                  quadratic=True)
        # Test that these are not equal.
        self.assertRaises(AssertionError, np.testing.assert_almost_equal,
                          spec / spec, spec2 / spec)

    def test_dpss(self):
        """
        Tests case for dpss. The resulting v tapers are tested. There is
        no test for theta or lambda.
        """
        v = np.load

        datafile = os.path.join(os.path.dirname(__file__), 'data', 'dpss.npz')
        v = np.load(datafile)['v']

        v2, lamb, theta = dpss(512, 2.5, 2)
        # Taper 1, normalize for precision
        np.testing.assert_almost_equal(v2[:,0] / v[:,0], v[:,0] / v[:,0])
        # Taper 2, normalize for precision
        np.testing.assert_almost_equal(v2[:,1] / v[:,1], v[:,1] / v[:,1])

        # Do the same but with spline interpolation.
        v3, lamb2, thetha2 = dpss(512, 2.5, 2, nmax=256)
        # Test both tapers. They are not exactly equal therefore only two
        # digits are compared.
        np.testing.assert_almost_equal(v3 / v3.max(), v2 / v2.max(), 2)


def suite():
    return unittest.makeSuite(MtSpecTestCase, 'test')


if __name__ == '__main__':
    unittest.main(defaultTest='suite')
