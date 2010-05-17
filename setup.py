#! /usr/bin/env python
# -*- coding: utf-8 -*-
"""
mtspecPy installer

:copyright:
    Lion Krischer, Moritz Beyreuther and German A. Prieto
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

from distutils.ccompiler import get_default_compiler
from distutils.unixccompiler import UnixCCompiler
from distutils.errors import DistutilsExecError
from setuptools import find_packages, setup
from setuptools.extension import Extension
#from numpy.distutils.ccompiler import get_default_compiler
#from setuptools import find_packages
#from numpy.distutils.core import setup
#from numpy.distutils.extension import Extension
import os, re
import platform
import sys


VERSION = open(os.path.join("VERSION.txt")).read()

# Monkey patch Compiler for Unix, Linux and darwin
UnixCCompiler.src_extensions.append(".f90")
def _compile(self, obj, src, ext, cc_args, extra_postargs, pp_opts):
        compiler_so = self.compiler_so
        if sys.platform == 'darwin':
            compiler_so = _darwin_compiler_fixup(compiler_so, cc_args + extra_postargs)
        if ext == ".f90":
            if sys.platform == 'darwin' or sys.platform == 'linux2':
                compiler_so = ["gfortran"]
                cc_args = ["-O", "-fPIC", "-c"]
        try:
            self.spawn(compiler_so + cc_args + [src, '-o', obj] +
                       extra_postargs)
        except DistutilsExecError, msg:
            raise CompileError, msg
UnixCCompiler._compile = _compile

# hack to prevent build_ext from trying to append "init" to the export symbols
class finallist(list):
    def append(self, object):
        return

#fortran = re.compile(r'.*[.](f90|f95|f77|for|ftn|f|pyf)\Z',re.I).match
class MyExtension(Extension):
    def __init__(self, *args, **kwargs):
        Extension.__init__(self, *args, **kwargs)
        self.export_symbols = finallist(self.export_symbols)
    #def has_fortran(self):
    #    for source in self.sources:
    #        if fortran(source):
    #            return True

macros = []
extra_link_args = []
extra_compile_args = []
if platform.system() == "Windows":
    macros.append(('WIN32', '1'))
    # disable some warnings for MSVC
    macros.append(('_CRT_SECURE_NO_WARNINGS', '1'))
    if 'mingw32' in sys.argv or \
        ('-c' not in sys.argv and get_default_compiler() == 'mingw32'):
        # Workaround Win32 + MinGW + Python 2.6 
        # :see: http://bugs.python.org/issue3308
        macros.append(('time_t', '__int64'))
        macros.append(('localtime', '_localtime64'))
        macros.append(('gmtime', '_gmtime64'))
    elif 'msvc' in sys.argv or \
        ('-c' not in sys.argv and get_default_compiler() == 'msvc'):
        if platform.architecture()[0] == '32bit':
            # Workaround Win32 and MSVC - see issue #64 
            extra_compile_args.append("/fp:strict")


src = os.path.join('src', 'mtspec', 'src') + os.sep
gp_src = os.path.join('src', 'gplot', 'src') + os.sep
sp_src = os.path.join('src', 'splines', 'src') + os.sep
#symbols = [s.strip() for s in open(src + 'mtspec.def', 'r').readlines()[2:]
#           if s.strip() != '']
lib = MyExtension('mtspec',
                  define_macros=macros,
                  #library_dirs=['/usr/bin'],
                  libraries=['lapack', 'fftw3'],
                  extra_link_args=extra_link_args,
                  extra_compile_args=extra_compile_args,
                  #export_symbols=symbols,
                  sources=[gp_src + 'gplot.f90', gp_src + 'gnuplot.f90',
                           sp_src + 'spline_cubic.f90',
                           src + 'spectra.f90', src + 'mtspec.f90',
                           src + 'fft.f90', src + 'ifft.f90',
                           src + 'adaptspec.f90', src + 'dpss.f90',
                           src + 'dpss_ev.f90', src + 'eigenft.f90',
                           src + 'pythag.f90', src + 'sym_fft.f90',
                           src + 'oct_spec.f90', src + 'set_xint.f90',
                           src + 'sft.f90', src + 'xint.f90',
                           src + 'qtdis.f90', src + 'qsnorm.f90',
                           src + 'yule.f90', src + 'wv_spec.f90',
                           src + 'jackspec.f90', src + 'df_spec.f90',
                           src + 'atanh2.f90', src + 'nearn.f90',
                           src + 'sine_psd.f90', src + 'sine_cohe.f90',
                           src + 'dpss_spline.f90', src + 'mt_cohe.f90',
                           src + 'nsqi.f90', src + 'qrfac.f90',
                           src + 'rsm_eig.f90', src + 'rst_eig.f90',
                           src + 'nsinv.f90', src + 'ftest.f90',
                           src + 'fdis.f90', src + 'psd_reshape.f90',
                           src + 'qiinv.f90', src + 'zqrfac.f90',
                           src + 'mt_deconv.f90', src + 'mt_transfer.f90',
                           src + 'qi_nsqi.f90', src + 'cumtrapz.f90',
                           src + 'spec_gram.f90', src + 'spec_gram2.f90',
                           src + 'qi_nsqi2.f90', src + 'qi_nsqi3.f90'])


setup(
    name='mtspec',
    version=VERSION,
    description="Python Bindings for multitaper `mtspec` f90 Library",
    long_description="""Python (ctypes) Bindings for multitaper mtspec f90
    Library

    For more information see:
    https://svn.geophysik.uni-muenchen.de/trac/mtspecpy/wiki
    """,
    url='https://svn.geophysik.uni-muenchen.de/trac/mtspecpy/wiki',
    author='Lion Krischer, Moritz Beyreuther, German A. Prieto',
    author_email='krischer@geophysik.uni-muenchen.de, beyreuth@geophysik.uni-muenchen.de',
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
    keywords=['mtspecpy', 'multitaper', 'python', 'seismology', 'waveform',
             'signal', 'processing'],
    packages=find_packages(),
    namespace_packages=['mtspec'],
    zip_safe=False,
    install_requires=[
        'numpy',
    ],
    download_url="https://svn.geophysik.uni-muenchen.de" + \
        "/svn/mtspecpy/trunk#egg=mtspecpy-dev",
    ext_package='lib',
    ext_modules=[lib],
    #include_package_data=True,
    #test_suite="mtspecpy.tests.suite"
)
