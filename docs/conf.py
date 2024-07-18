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
import os
# import sys
# sys.path.insert(0, os.path.abspath('.'))

# -- Project information -----------------------------------------------------

import datetime
year = datetime.datetime.now().year

project = 'Open MPI'
copyright = f'2003-{year}, The Open MPI Community'
author = 'The Open MPI Community'

with open("../VERSION") as fp:
    ompi_lines = fp.readlines()

ompi_data = dict()
for ompi_line in ompi_lines:
    if '#' in ompi_line:
        parts = ompi_line.split("#")
        ompi_line = parts[0]
    ompi_line = ompi_line.strip()

    if '=' not in ompi_line:
        continue

    ompi_key, ompi_val = ompi_line.split("=")
    ompi_data[ompi_key.strip()] = ompi_val.strip()

# "release" is a sphinx config variable -- assign it to the computed
# Open MPI version number.
series = f"{ompi_data['major']}.{ompi_data['minor']}.x"
release = f"{ompi_data['major']}.{ompi_data['minor']}.{ompi_data['release']}{ompi_data['greek']}"

# If we are building in a ReadTheDocs.io environment, there will be
# READTHEDOCS environment variables.
#
# Relevant RTD env variables (documented
# https://docs.readthedocs.io/en/stable/builds.html#build-environment):
key = 'READTHEDOCS'
if key in os.environ and os.environ[key] == 'True':
    print("OMPI: found ReadTheDocs build environment")

    # Tell Jinja2 templates the build is running on Read the Docs
    if "html_context" not in globals():
        html_context = {}
    html_context["READTHEDOCS"] = True

    # Define the canonical URL if you are using a custom domain on
    # Read the Docs
    html_baseurl = os.environ.get("READTHEDOCS_CANONICAL_URL", "")

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

# -- Open MPI-specific options -----------------------------------------------

# This prolog is included in every file.  Put common stuff here.

rst_prolog = f"""
.. |mdash|  unicode:: U+02014 .. Em dash
.. |rarrow| unicode:: U+02192 .. Right arrow

.. |year| replace:: {year}
.. |ompi_ver| replace:: v{release}
.. |ompi_series| replace:: v{series}
"""
