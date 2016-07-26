Tutorial
========

This tutorial shows source code on how to recreate the first three figures from
the multitaper paper of Prieto. Simple examples for the DPSS method, the
Wigner Ville spectrum, and multitaper coherency calculations are included as
well. More information can be found in the Prieto paper (reference_).

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


Recreate Fig. 2
---------------

.. plot:: snippets/figure_2.py

.. literalinclude:: snippets/figure_2.py
    :language: python


Recreate Fig. 3
---------------

.. plot:: snippets/figure_3.py

.. literalinclude:: snippets/figure_3.py
    :language: python


DPSS Example
------------

.. plot:: snippets/dpss_example.py

.. literalinclude:: snippets/dpss_example.py
    :language: python


Wigner-Ville Spectrum
---------------------

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

.. plot:: snippets/multitaper_coherency_example.py

.. literalinclude:: snippets/multitaper_coherency_example.py
    :language: python
