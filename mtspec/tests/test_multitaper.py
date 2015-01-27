#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
mtspec test suite.

:copyright:
    Lion Krischer (krischer@geophysik.uni-muenchen.de) and
    Moritz Beyreuther, 2010-2015
:license:
    GNU General Public License, Version 3
    (http://www.gnu.org/copyleft/gpl.html)
"""
from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

import numpy as np
import os
import unittest

from ..multitaper import mtspec, sine_psd, dpss, wigner_ville_spectrum, \
    mt_coherence
from ..util import signal_bursts, load_mtdata


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
        # No NaNs are supposed to be in the output.
        self.assertEqual(np.isnan(spec).any(), False)
        self.assertEqual(np.isnan(spec).any(), False)
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
        # No NaNs are supposed to be in the output.
        self.assertEqual(np.isnan(spec).any(), False)
        self.assertEqual(np.isnan(spec).any(), False)
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
            mtspec(data, 1.0, 4.5, number_of_tapers=5, optional_output=True)
        # No NaNs are supposed to be in the output.
        self.assertEqual(np.isnan(spec).any(), False)
        self.assertEqual(np.isnan(spec).any(), False)
        self.assertEqual(np.isnan(eigspec).any(), False)
        self.assertEqual(np.isnan(eigcoef).any(), False)
        self.assertEqual(np.isnan(weights).any(), False)
        # XXX: Verify if this is correct, if so savez the data
        # import matplotlib.pyplot as plt
        # plt.plot(np.abs(eigcoef[:,0]))
        # plt.show()
        # import ipdb; ipdb.set_trace()
        # np.savez('data/multitaper.npz', spec=spec.astype('float32'),
        #         eigspec=eigspec.astype('float32'),
        #         eigcoef=eigcoef.astype('float32'),
        #         weights=weights.astype('float32'))
        # Load the good data.
        datafile = os.path.join(os.path.dirname(__file__), 'data',
                                'multitaper.npz')
        record = np.load(datafile)
        spec2 = record['spec']
        # eigspec2 = record['eigspec']
        # eigcoef2 = record['eigcoef']
        # weights2 = record['weights']
        freq2 = np.arange(43201) * 1.15740741e-05
        # Compare, normalize for subdigit comparision
        np.testing.assert_almost_equal(freq, freq2)
        np.testing.assert_almost_equal(spec / spec, spec2 / spec, 5)
        # np.testing.assert_almost_equal(eigspec/eigspec, eigspec2/eigspec, 5)
        # np.testing.assert_almost_equal(eigcoef/eigcoef, eigcoef2/eigcoef, 5)

    def test_eigenspectraOutput(self):
        """
        Tests the eigenspectra output using a nonadaptive spectra. This also at
        least somewhat tests the weights.
        """
        data = load_mtdata('PASC.dat.gz')
        # Calculate the spectra.
        spec, freq, eigspec, eigcoef, weights = \
            mtspec(data, 1.0, 4.5, number_of_tapers=5, adaptive=False,
                   optional_output=True)
        # No NaNs are supposed to be in the output.
        self.assertEqual(np.isnan(spec).any(), False)
        self.assertEqual(np.isnan(spec).any(), False)
        self.assertEqual(np.isnan(eigspec).any(), False)
        self.assertEqual(np.isnan(eigcoef).any(), False)
        self.assertEqual(np.isnan(weights).any(), False)
        # The weights should all be one for the nonadaptive spectrum.
        np.testing.assert_almost_equal(weights, np.ones((43201, 5),
                                                        'float64'))
        # Sum over the eigenspectra to get the nonadaptive spectrum.
        new_spec = eigspec.sum(axis=1) / float(eigspec.shape[1])
        new_spec[1:] *= 2.0
        # Compare the output and the newly calculated spectrum. Normalize with
        # the maximum values to avoid scaling issues.
        np.testing.assert_almost_equal(spec[:10] / spec.max(),
                                       new_spec[:10] / new_spec.max())

    def test_paddedMultitaperSpectrumWithErrors(self):
        """
        Test for mtspec_pad with jackknife interval errors. The result is
        compared to the output of test_recreatePaperFigures.py in the same
        directory. This is assumed to be correct because they are identical to
        the figures in the paper on the machine that created these.
        """
        data = load_mtdata('v22_174_series.dat.gz')
        # Calculate the spectra.
        spec, freq, jackknife, _, _ = mtspec(
            data, 4930., 3.5, nfft=312, number_of_tapers=5, statistics=True)
        # No NaNs are supposed to be in the output.
        self.assertEqual(np.isnan(spec).any(), False)
        self.assertEqual(np.isnan(spec).any(), False)
        self.assertEqual(np.isnan(jackknife).any(), False)
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
        # No NaNs are supposed to be in the output.
        self.assertEqual(np.isnan(spec).any(), False)
        self.assertEqual(np.isnan(spec).any(), False)
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
        spec, freq, jackknife, fstatistics, _ = mtspec(
            data, 4930., 3.5, nfft=312, number_of_tapers=5, statistics=True,
            rshape=0, fcrit=0.9)
        # No NaNs are supposed to be in the output.
        self.assertEqual(np.isnan(spec).any(), False)
        self.assertEqual(np.isnan(freq).any(), False)
        self.assertEqual(np.isnan(jackknife).any(), False)
        self.assertEqual(np.isnan(fstatistics).any(), False)
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
        # No NaNs are supposed to be in the output.
        self.assertEqual(np.isnan(spec).any(), False)
        self.assertEqual(np.isnan(freq).any(), False)
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
        # No NaNs are supposed to be in the output.
        self.assertEqual(np.isnan(spec).any(), False)
        self.assertEqual(np.isnan(freq).any(), False)
        self.assertEqual(np.isnan(errors).any(), False)
        self.assertEqual(np.isnan(tapers).any(), False)
        # XXX: assert for errors and tapers is missing
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
        # No NaNs are supposed to be in the output.
        self.assertEqual(np.isnan(spec).any(), False)
        self.assertEqual(np.isnan(freq).any(), False)
        spec2, freq2 = mtspec(data, 1.0, 4.5, number_of_tapers=2,
                              quadratic=True)
        # No NaNs are supposed to be in the output.
        self.assertEqual(np.isnan(spec2).any(), False)
        self.assertEqual(np.isnan(freq2).any(), False)
        # Test that these are not equal.
        self.assertRaises(AssertionError, np.testing.assert_almost_equal,
                          spec, spec2)

    def test_dpss(self):
        """
        Tests case for dpss. The resulting v tapers are tested. There is
        no test for theta or lambda.
        """
        v = np.load

        datafile = os.path.join(os.path.dirname(__file__), 'data', 'dpss.npz')
        v = np.load(datafile)['v']

        v2, lamb, theta = dpss(512, 2.5, 2)
        # No NaNs are supposed to be in the output.
        self.assertEqual(np.isnan(v2).any(), False)
        self.assertEqual(np.isnan(lamb).any(), False)
        self.assertEqual(np.isnan(theta).any(), False)
        # Taper 1, normalize for precision
        np.testing.assert_almost_equal(v2[:, 0] / v[:, 0], v[:, 0] / v[:, 0])
        # Taper 2, normalize for precision
        np.testing.assert_almost_equal(v2[:, 1] / v[:, 1], v[:, 1] / v[:, 1])

        # Do the same but with spline interpolation.
        v3, lamb2, thetha2 = dpss(512, 2.5, 2, nmax=400)
        # Test both tapers. They are not exactly equal therefore only two
        # digits are compared.
        np.testing.assert_almost_equal(v3 / v3, v2 / v3, 2)

    def test_wignerVille(self):
        """
        Test for wigner_ville_spectrum. Test uses only a fraction of the
        whole spectrum due to space consumtions.
        """
        datafile = os.path.join(os.path.dirname(__file__), 'data', 'wv.npz')
        rec = np.load(datafile)
        wv = abs(wigner_ville_spectrum(
            signal_bursts(), 10, 3.5, smoothing_filter='gauss',
            verbose=False))
        # No NaNs are supposed to be in the output.
        self.assertEqual(np.isnan(wv).any(), False)
        rms1 = rms(rec['wv_250_500_25'], wv[250:500:25])
        rms2 = rms(rec['wv_750_1000_25'], wv[750:1000:25])
        self.assertEqual(True, rms1 < 1e-6)
        self.assertEqual(True, rms2 < 1e-6)

    def test_mtCoherence(self):
        """
        Coherence test case.
        """
        datafile = os.path.join(
            os.path.dirname(__file__), 'data', 'mt_cohe.npz')
        np.random.seed(815)
        npts = 256
        sampling_rate = 10.0
        # 1Hz: one sine wave in one second (sampling_rate samples)
        one_hz_sin = np.sin(np.arange(0, sampling_rate, dtype='float32') /
                            sampling_rate * 2 * np.pi)
        # repeat this until npts is reached
        one_hz_sin = np.tile(one_hz_sin, int(npts // sampling_rate + 1))[:npts]
        xi = np.random.randn(npts) + one_hz_sin * .5
        xj = np.random.randn(npts) + one_hz_sin * .5
        dt, tbp, kspec, nf, p = 1.0/sampling_rate, 3.5, 5, npts // 2, .90
        out = mt_coherence(dt, xi, xj, tbp, kspec, nf, p, freq=True,
                           cohe=True, iadapt=1)
        freq = np.linspace(0, sampling_rate/2, npts/2).astype('float32')
        cohe = np.load(datafile)['cohe']
        # No NaNs are supposed to be in the output.
        self.assertEqual(np.isnan(cohe).any(), False)
        np.testing.assert_almost_equal(freq, out['freq'], 5)
        np.testing.assert_almost_equal(cohe/cohe, out['cohe']/cohe, 4)


def rms(x, y):
    """
    Normalized RMS
    """
    return np.sqrt(((x - y)**2).mean() / (x**2).mean())


def suite():
    return unittest.makeSuite(MtSpecTestCase, 'test')


if __name__ == '__main__':
    unittest.main(defaultTest='suite')
