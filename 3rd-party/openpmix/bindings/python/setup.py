#!/usr/bin/env python3
#
# Copyright (c) 2022      Nanook Consulting. All rights reserved

# Note: as of Dec 2022, we still use setup.py with the Python
# setuptools package, even though setuptools docs now recommend
# upgrading to the setup.cfg or pyproject.toml methods.  Perhaps
# someday we'll migrate to the newer methods, but until then, setup.py
# seems to be working.

import os, sys
import warnings
import setuptools

from setuptools import Extension, setup
from Cython.Build import cythonize

from Cython.Compiler.Main import default_options
default_options['emit_linenums'] = True

def get_include():
    dirs = []
    for key in ['BUILDDIR', 'SRCDIR']:
        name = f'PMIX_BINDINGS_TOP_{key}'
        if name in os.environ:
            dirs.append(f'{os.environ[name]}/include')

    return dirs

def getVersion():
    dir = os.path.dirname(__file__)

    vers_path = os.path.join(dir, '../../include', 'pmix_version.h')
    if not os.path.exists(vers_path):
        include_dirs = get_include()
        vers_path = None
        for dir in include_dirs:
            tmp_path = os.path.join(dir, 'pmix_version.h')
            if os.path.exists(tmp_path):
                vers_path = tmp_path
                break
        if vers_path is None:
            print("Error: pmix_version.h does not exist at path: ",vers_path)
            sys.exit(1)

    with open(vers_path) as verFile:
        lines = verFile.readlines()
        for l in lines:
            if 'MAJOR' in l:
                major = l.split()[2]
                major = major[:-1]
            elif 'MINOR' in l:
                minor = l.split()[2]
                minor = minor[:-1]
            elif 'RELEASE' in l:
                release = l.split()[2]
                release = release[:-1]
        vers = [major, minor, release]
        version = ".".join(vers)
        return version

def package_setup(package_name, package_vers):
    '''
    Package setup routine.
    '''

    # Find the Cython source file.  Note that we always look in the
    # build tree.  The Makefile.am will ensure to always set this env
    # variable, but if someone invokes this script without setting
    # that env variable, just assume that pmix.pyx is in the same
    # directory as this script.
    key = 'PMIX_BINDINGS_TOP_BUILDDIR'
    if key in os.environ:
        dir = os.environ[key]
        src_filename = os.path.join(dir, "bindings", "python", "pmix.pyx")
    else:
        dir = os.path.dirname(sys.argv[0])
        src_filename = os.path.join(dir, "pmix.pyx")

    # Per comment at the top of this file, we know that setup.py is
    # deprecated/discouraged.  Until we switch away from it, remove
    # the setuptools "deprecated" warnings that are emitted to stderr
    # so that we do not scare the users.
    if hasattr(setuptools, "SetuptoolsDeprecationWarning"):
        warnings.filterwarnings("ignore",
                                category=setuptools.SetuptoolsDeprecationWarning)

    setup(
        name = 'pypmix',
        version = getVersion(),
        url = 'https://pmix.org',
        license = '3-clause BSD',
        author = 'Ralph H. Castain',
        author_email = 'rhc@pmix.org',
        description = 'Python bindings for PMIx',
        classifiers = [
                'Development Status :: 5 - Production/Stable',
                'Intended Audience :: Developers',
                'Topic :: HPC :: Parallel Programming :: System Management',
                'License :: OSI Approved :: BSD License',
                'Programming Language :: Python :: 3.4',
                'Programming Language :: Python :: 3.5',
                'Programming Language :: Python :: 3.6',
                'Programming Language :: Python :: 3.7',
                'Programming Language :: Python :: 3.8',
                'Programming Language :: Python :: 3.9',
                'Programming Language :: Python :: 3.10',
                'Programming Language :: Python :: 3.11',
                'Programming Language :: Python :: 3.12'],
        keywords = ['PMI', 'PMIx', 'HPC', 'MPI', 'SHMEM' ],
        platforms = 'any',
        install_requires = ["cython"],
        zip_safe = False,
        ext_modules = cythonize([Extension("pmix",
                                           [src_filename],
                                           libraries=["pmix"],
                                           depends=[], )],
                                compiler_directives={'language_level': 3}),
        include_dirs = get_include()
    )

def main():
    '''
    The main entry point for this program.
    '''
    package_name = 'pypmix'
    package_vers = getVersion()

    package_setup(package_name, package_vers)

    return os.EX_OK


if __name__ == '__main__':
    sys.exit(main())
