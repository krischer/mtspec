#! /usr/bin/env python
# -*- coding: utf-8 -*-
"""
mtspecPy installer.

:copyright:
    Moritz Beyreuther, Lion Krischer and German A. Prieto
:license:
    GNU General Public License (GPL)

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
    02110-1301, USA.
"""

from distutils.ccompiler import CCompiler
from distutils.errors import DistutilsExecError, CompileError
from setuptools import find_packages, setup
from setuptools.extension import Extension
import os
import platform
import sys

VERSION = open(os.path.join("mtspec", "VERSION.txt")).read().strip()

macros = []
extra_link_args = []
extra_compile_args = []
# XXX: Linking happens with gcc and if gfortran was installed seperately it
# might not be able to find the gfortran library. In that case but the path to
# libgfortan.a here ("locate libgfortran.a").
# Maybe just do the linking also with gfortran?
library_dirs = []

# Monkey patch CCompiler for Unix, Linux and Windows
# We pretend that .f90 is a C extension and overwrite
# the corresponding compilation calls
CCompiler.language_map['.f90'] = "c"

try:
    from distutils.unixccompiler import UnixCCompiler, _darwin_compiler_fixup
except ImportError:
    from distutils.unixccompiler import UnixCCompiler #we assume we will not need to use _darwin_compiler_fixup


# Monkey patch UnixCCompiler for Unix, Linux and darwin
UnixCCompiler.src_extensions.append(".f90")


def _compile(self, obj, src, ext, cc_args, extra_postargs, pp_opts):
        compiler_so = self.compiler_so
        if sys.platform == 'darwin':
            try:
                compiler_so = _darwin_compiler_fixup(compiler_so, cc_args + extra_postargs)
            except AttributeError:  # In this case we leave the compiler_so sring as it is. Hoping it will work
                pass

        if ext == ".f90":
            if sys.platform == 'darwin' or sys.platform == 'linux2':
                compiler_so = ["gfortran"]
                cc_args = ["-O", "-fPIC", "-c", "-ffree-form"]
        try:
            self.spawn(compiler_so + cc_args + [src, '-o', obj] +
                       extra_postargs)
        except DistutilsExecError as msg:
            raise CompileError(msg)
UnixCCompiler._compile = _compile
# set library dir for mac and linux
libs = ['gfortran']

if platform.system() == "Windows":
    # Monkey patch MSVCCompiler for Windows
    # XXX: Only work if the msvc compiler is default
    # We only do this on windows, importing msvccompiler on linux produces the
    # following warning:"
    #      Warning: Can't read registry to find the necessary compiler setting
    #      Make sure that Python modules _winreg, win32api or win32con are
    #      installed.
    from distutils.msvccompiler import MSVCCompiler
    MSVCCompiler._c_extensions.append(".f90")

    def compile(self, sources, output_dir=None, macros=None, include_dirs=None,
            debug=0, extra_preargs=None, extra_postargs=None,
            depends=None):
        if output_dir:
            try:
                os.makedirs(output_dir)
            except OSError:
                pass
        objects = []
        for src in sources:
            file, ext = os.path.splitext(src)
            if output_dir:
                obj = os.path.join(output_dir, os.path.basename(file) + ".o")
            else:
                obj = file + ".o"
            if ext == ".f90":
                self.compiler_so = ["gfortran"]
                #cc_args = ["-O", "-c", "-ffree-form"]
                cc_args = ["-c"]
                extra_postargs = []
            try:
                self.spawn(self.compiler_so + cc_args + [src, '-o', obj] +
                           extra_postargs)
            except DistutilsExecError as msg:
                raise CompileError(msg)
            objects.append(obj)
        return objects

    def link(self, target_desc, objects, output_filename, *args, **kwargs):
        os.mkdir(os.path.dirname(output_filename))
        self.spawn(self.compiler_so + ["-shared"] + objects +
                   ["-o", output_filename])

    MSVCCompiler.compile = compile
    MSVCCompiler.link = link
    # do not include any libraries for windows
    libs = []
    # include macros
    macros = []
    extra_link_args = []
    extra_compile_args = []
    macros.append(('WIN32', '1'))
    # Workaround Win32 + MinGW + Python 2.6
    # :see: http://bugs.python.org/issue3308
    macros.append(('time_t', '__int64'))
    macros.append(('localtime', '_localtime64'))
    macros.append(('gmtime', '_gmtime64'))


### Hack to prevent build_ext from trying to append "init" to the export
### symbols
class finallist(list):
    def append(self, object):
        return


class MyExtension(Extension):
    def __init__(self, *args, **kwargs):
        Extension.__init__(self, *args, **kwargs)
        self.export_symbols = finallist(self.export_symbols)

src = os.path.join('mtspec', 'src', 'src') + os.sep

lib = MyExtension('mtspec',
                  define_macros=macros,
                  library_dirs=library_dirs,
                  libraries=libs,
                  extra_link_args=extra_link_args,
                  extra_compile_args=extra_compile_args,
                  #export_symbols=symbols,
                  sources=[src + 'spectra.f90', src + 'adaptspec.f90',
                        src + 'atanh2.f90',
                        src + 'df_spec.f90', src + 'dpss.f90',
                        src + 'dpss_ev.f90', src + 'dpss_spline.f90',
                        src + 'eigenft.f90', src + 'fdis.f90', src + 'fft.f90',
                        src + 'ftest.f90', src + 'ifft.f90',
                        src + 'jackspec.f90', src + 'mt_cohe.f90',
                        src + 'mt_deconv.f90', src + 'mt_transfer.f90',
                        src + 'mtspec.f90', src + 'nearn.f90',
                        src + 'nnls.f90', src + 'nsinv.f90',
                        src + 'nsqi.f90', src + 'oct_spec.f90',
                        src + 'psd_reshape.f90', src + 'pythag.f90',
                        src + 'qiinv.f90', src + 'qrfac.f90',
                        src + 'qsnorm.f90', src + 'qtdis.f90',
                        src + 'rsm_eig.f90', src + 'set_xint.f90',
                        src + 'sft.f90', src + 'sine_cohe.f90',
                        src + 'sine_psd.f90',
                        src + 'spline.f90', src + 'sym_fft.f90',
                        src + 'tinvit.f90', src + 'trbak1.f90',
                        src + 'tred1.f90', src + 'tridib.f90',
                        src + 'wv_spec.f90', src + 'wv_spec_to_array.f90',
                        src + 'xint.f90', src + 'yule.f90',
                        src + 'zqrfac.f90'])

setup(
    name='mtspec',
    version=VERSION,
    description="Python Bindings for multitaper `mtspec` f90 Library",
    long_description="""

    Python-ctypes wrapper for multitaper `mtspec` f90 Library of German A.
    Prieto::

        Prieto, G. A., R. L. Parker, F. L. Vernon. (2009),
        A Fortran 90 library for multitaper spectrum analysis,
        Computers and Geosciences, 35, pp. 1701-1710.

    For more information see:
    https://github.com/krischer/mtspec

    ::

        Main Changes in 0.2.6
        ---------------------
        * Bugfix #6, access violation on windows
        * Refactoring parts of the underlying fortran code, now memchecked
        * Bugfix for NaNs in spectra
        * Bugfix RMS calculation in tests
    """,
    url='https://github.com/krischer/mtspec',
    author='Moritz Beyreuther, Lion Krischer and German A. Prieto',
    author_email='beyreuth@geophysik.uni-muenchen.de',
    license='GNU General Public License (GPL)',
    platforms='OS Independent',
    classifiers=[
        'Development Status :: 4 - Beta',
        'Environment :: Console',
        'Intended Audience :: Science/Research',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: GNU General Public License (GPL)',
        'Operating System :: OS Independent',
        'Programming Language :: Python',
        'Topic :: Scientific/Engineering',
        'Topic :: Scientific/Engineering :: Physics',
    ],
    keywords=['mtspec', 'multitaper', 'seismology', 'waveform',
              'signal', 'processing', 'taper', 'wigner', 'ville'],
    packages=find_packages(),
    #namespace_packages=['mtspec'],
    zip_safe=False,
    install_requires=[
        'numpy',
    ],
    download_url="https://svn.geophysik.uni-muenchen.de" + \
        "/svn/mtspecpy/trunk#egg=mtspec-dev",
    ext_package='mtspec.lib',
    ext_modules=[lib],
    include_package_data=True,
    test_suite="mtspec.tests.suite"
)

# Remove mod files.
for file in ['mvspectra.mod', 'spectra.mod']:
    try:
        os.remove(file)
    except:
        pass
