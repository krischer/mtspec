#!/usr/bin/env python
from numpy.distutils.core import setup
from numpy.distutils.misc_util import Configuration

import glob
import inspect
import os


def configuration(parent_package='', top_path=None):
    config = Configuration('mtspec', parent_package, top_path)

    # Get a list of all source code files
    setup_directory = os.path.dirname(os.path.abspath(inspect.getfile(
        inspect.currentframe())))
    fortran_files = glob.glob(os.path.join(setup_directory, "mtspec", "src",
        "src", "*.f90"))
    # Interface file
    interface_file = os.path.join("mtspec", "src", "_mtspec_lib.pyf")
    fortran_files.insert(0, interface_file)

    config.add_extension('_mtspec_lib', fortran_files)
    return config

if __name__ == "__main__":
    setup(configuration=configuration)
