mtspec
======

Multitaper Spectral Estimation
------------------------------

**mtspec** is a Python wrapper for the
`Multitaper Spectrum Estimation Library <http://wwwprof.uniandes.edu.co/~gprieto/software/mwlib.html>`_
by Germán Prieto. The relevant publication is ::

    Prieto, G. A., R. L. Parker, F. L. Vernon. (2009),
    A Fortran 90 library for multitaper spectrum analysis,
    Computers and Geosciences, 35, pp. 1701-1710.
    doi:10.1016/ j.cageo.2008.06.007

It enables you to calculate Slepian windows, perform multitaper spectral
estimations with various options, calculate Wigner-Ville time-frequency
distributions, and construct coherence spectra with multitapers.

It currently wraps version 3.1 of the library. This is mainly due to later
versions using FFTW which would introduce an additional dependency.


Getting Started and Documentation
---------------------------------

To get started, please have a look at the tutorial.

.. toctree::
  :maxdepth: 1

  tutorial.rst


Installation
------------

Requirements
^^^^^^^^^^^^

**mtspec** currently runs on Linux and Mac OS X. Adding support for Windows is
mainly a question of compiling the shared Fortran libraries - pull requests are
welcome.

* ``gfortran >= 4.7``
* ``Python 2.6, 2.7, 3.3, or 3.4``
* ``NumPy >= 1.7``
* ``matplotlib``
* ``flake8``

Fortran Compiler
~~~~~~~~~~~~~~~~

If you don't have ``gfortran``, please install it (on Linux) with

.. code-block:: bash

    $ sudo apt-get install gfortran

or the equivalent of your distribution. On OSX we recommend to install
`Homebrew <http://brew.sh/>`_ and then use it to install ``gfortran``:

.. code-block:: bash

    $ brew install gcc

Python and Dependencies
~~~~~~~~~~~~~~~~~~~~~~~

If you know what you are doing, just make sure the aforementioned
dependencies are installed. Otherwise do yourself a favor and download the
`Anaconda <https://store.continuum.io/cshop/anaconda/>`_ Python distribution.
It is a free scientific Python distribution bundling almost all necessary
modules with a convenient installer (does not require root access!).
Once installed assert that ``pip`` and ``conda`` point to the Anaconda
installation folder (you may need to open a new terminal after installing
Anaconda). Then install **mtspec**'s dependencies with

.. code-block:: bash

    $ conda install numpy matplotlib flake8 pip

A possible complication arises **if you are running on a server without a
display**. In that case please edit (on Linux)
``~/.config/matplotlib/matplotlibrc`` (create if it does not exist) and make
sure the following line is part of it:

.. code-block:: bash

    backend: agg


Installing mtspec
^^^^^^^^^^^^^^^^^

For normal usage just install **mtspec** with

User Installation
~~~~~~~~~~~~~~~~~

.. code-block:: bash

    $ pip install mtspec

Developer Installation
~~~~~~~~~~~~~~~~~~~~~~

If you want the bleeding edge version or intend to edit **mtspec**'s code,
clone the git repository and install in an editable fashion:

.. code-block:: bash

    $ git clone https://github.com/krischer/mtspec.git
    $ cd mtspec
    $ pip install -v -e .


Testing
^^^^^^^

To assert that your installation is working properly, execute

.. code-block:: bash

    $ python -m mtspec.tests

and make sure all tests pass. Otherwise please contact the developers.

Build the Documentation
^^^^^^^^^^^^^^^^^^^^^^^

The documentation requires ``sphinx`` and the Alabaster theme. Install both
with

.. code-block:: bash

    $ pip install sphinx alabaster

Build the doc with

.. code-block:: bash

    $ cd doc
    $ make html

Finally open the ``doc/html_doc/index.html`` file with the browser of your
choice.

Additional Notes
^^^^^^^^^^^^^^^^

In case you get the error message ``"Internal Error: printf is broken"``, you
can use the following workaround:


.. code-block:: bash

    export LC_ALL=C
    python script.py

This is a known ``gfortran`` bug, see
`here <http://projects.scipy.org/scipy/ticket/696>`_
for an explanation.


API Doc
-------

.. toctree::

    multitaper_dpss
    multitaper_mtspec
    multitaper_sine_psd
    multitaper_mt_coherence
    multitaper_wigner_ville_spectrum
    util
