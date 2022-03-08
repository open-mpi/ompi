# Configuration file for the Sphinx documentation builder.
#
# This file only contains a selection of the most common options. For a full
# list see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
# import os
# import sys
# sys.path.insert(0, os.path.abspath('.'))

# -- Project information -----------------------------------------------------

import datetime
year = datetime.datetime.now().year

project = 'Open MPI'
copyright = f'2003-{year}, The Open MPI Community'
author = 'The Open MPI Community'

# The full version, including alpha/beta/rc tags
# Read the Open MPI version from the VERSION file
with open("../VERSION") as fp:
    ompi_lines = fp.readlines()

ompi_data = dict()
for ompi_line in ompi_lines:
    if '#' in ompi_line:
        ompi_line, _ = ompi_line.split("#")
    ompi_line = ompi_line.strip()

    if '=' not in ompi_line:
        continue

    ompi_key, ompi_val = ompi_line.split("=")
    ompi_data[ompi_key.strip()] = ompi_val.strip()

# "release" is a sphinx config variable -- assign it to the computed
# Open MPI version number.
series = f"{ompi_data['major']}.{ompi_data['minor']}.x"
release = f"{ompi_data['major']}.{ompi_data['minor']}.{ompi_data['release']}{ompi_data['greek']}"


# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
import sphinx_rtd_theme
extensions = ['recommonmark', "sphinx_rtd_theme"]

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store', 'venv', 'py*/**']


# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
html_theme = 'sphinx_rtd_theme'

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
#html_static_path = ['_static']

# -- Options for MAN output -------------------------------------------------

import os
import re

# Dynamically find all the man pages and build the appropriate list of
# tuples so that we don't have to manually maintain it.

def find_man_pages_top():
    def _doit(topdir):
        for root, dirs, files in os.walk(topdir):
            for filename in files:
                # Parse filenames of the format a "foo.X.rst"
                parts = re.search("^([^/]+?)\.([0-9]+)\.rst$", filename)

                # Skip files that do not match that format (e.g.,
                # "index.rst")
                if parts is None:
                    continue

                base_name = parts.group(1)
                section   = int(parts.group(2))

                full_filename_without_rst = f'{root}/{base_name}.{section}'

                # Append a tuple: (filename, name, description, authors, section)
                # We leave description blank.
                # We also leave authors blank, because "The Open MPI community"
                # already shows up in the copyright section.
                tuple = (full_filename_without_rst, base_name, '', '', section)
                tuples.append(tuple)

    tuples = list()
    _doit("man-openmpi")
    _doit("man-openshmem")

    return tuples

man_pages = find_man_pages_top()

# -- Open MPI-specific options -----------------------------------------------

# This prolog is included in every file.  Put common stuff here.

rst_prolog = f"""
.. |mdash|  unicode:: U+02014 .. Em dash
.. |rarrow| unicode:: U+02192 .. Right arrow

.. |year| replace:: {year}
.. |ompi_ver| replace:: v{release}
.. |ompi_series| replace:: v{series}
"""
