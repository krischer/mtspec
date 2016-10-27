Tutorial
========

This tutorial shows source code on how to recreate the first three figures
and the sixth figure from the multitaper paper of Prieto. Simple examples for
the DPSS method, the Wigner Ville spectrum, and multitaper coherency
calculations are included as well. More information can be found in the Prieto
paper (reference_).

.. _reference: http://dx.doi.org/10.1016/j.cageo.2008.06.007

.. contents::
    :local:


Fig. 1: Multitaper Spectrum Estimate with Confidence Intervals
--------------------------------------------------------------

This example shows data at the top and a multitaper spectrum estimate on the
bottom with 95% confidence intervals shaded in red.
See the documentation of the :func:`mtspec.multitaper.mtspec` function for
more details.

.. plot:: snippets/figure_1.py

.. literalinclude:: snippets/figure_1.py
   :language: python


Fig. 2: F Statistics
--------------------

The top shows the F statistics and the bottom a spectrum reshaped with these.
Function documentation: :func:`mtspec.multitaper.mtspec`.

.. plot:: snippets/figure_2.py

.. literalinclude:: snippets/figure_2.py
    :language: python


Fig. 3: Various Multitaper Variants
-----------------------------------

Comparison of different multitaper spectral estimates and various settings.
Top shows data, bottom 4 plots are spectral estimations. Function
documentations: :func:`mtspec.multitaper.mtspec` and
:func:`mtspec.multitaper.sine_psd`.

.. plot:: snippets/figure_3.py

.. literalinclude:: snippets/figure_3.py
    :language: python


DPSS Example
------------

A simple example showing how to calculate and plot a couple DPSS. Function
documentation: :func:`mtspec.multitaper.dpss`.

.. plot:: snippets/dpss_example.py

.. literalinclude:: snippets/dpss_example.py
    :language: python


Wigner-Ville Spectrum
---------------------

Time frequency like spectral estimation using the Wigner-Ville method.
Function documentation: :func:`mtspec.multitaper.wigner_ville_spectrum`.

.. plot:: snippets/wigner_ville_spectrum_example.py

.. literalinclude:: snippets/wigner_ville_spectrum_example.py
    :language: python


Wigner-Ville Smoothing
++++++++++++++++++++++

One of the main disadvantages of the Wigner-Ville method is occurrence of
interference terms. This can be somewhat alleviated by smoothing it at the
cost of the clarity of the distribution.

The following plot shows an example of this behaviour. The signal consists of
a linear chirp and an exponential chirp. The left figure is with smoothing and
the right one without it.

.. plot:: snippets/wigner_ville_smoothing.py

.. literalinclude:: snippets/wigner_ville_smoothing.py
    :language: python


Multitaper coherence example
----------------------------

Calculate the coherency of two signals using multitapers. Function
documentation: :func:`mtspec.multitaper.mt_coherence`.

.. plot:: snippets/multitaper_coherency_example.py

.. literalinclude:: snippets/multitaper_coherency_example.py
    :language: python


Fig. 6: Deconvolution
---------------------

Demonstrates the deconvolution of two time series. See the
:func:`mtspec.multitaper.mt_deconvolve` function for details.

.. plot:: snippets/figure_6.py

.. literalinclude:: snippets/figure_6.py
    :language: python


Multitaper deconvolution for earthquake source studies
------------------------------------------------------

This example shows how to compute the relative source time function of a target
event using the empirical Green's function approach with the
:func:`~mtspec.multitaper.mt_deconvolve` function.

It utilizes the `ObsPy library <https://obspy.org>`_ to read and filter the
seismological example data but it is not required for using ``mtspec``.

.. plot:: snippets/empirical_green_function.py

.. literalinclude:: snippets/empirical_green_function.py
    :language: python
