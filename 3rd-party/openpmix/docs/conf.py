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
import os
import time

mydate = datetime.datetime.utcfromtimestamp(
    int(os.environ.get('SOURCE_DATE_EPOCH', time.time()))
)
year = mydate.year
month = mydate.strftime("%B")

project = 'OpenPMIx'
copyright = f'2014-{year}, The OpenPMIx Community'
author = 'The OpenPMIx Community'

# The full version, including alpha/beta/rc tags
# Read the OpenPMIx version from the VERSION file
with open("../VERSION") as fp:
    opmix_lines = fp.readlines()

opmix_data = dict()
for opmix_line in opmix_lines:
    if '#' in opmix_line:
        parts = opmix_line.split("#")
        opmix_line = parts[0]
    opmix_line = opmix_line.strip()

    if '=' not in opmix_line:
        continue

    opmix_key, opmix_val = opmix_line.split("=")
    opmix_data[opmix_key.strip()] = opmix_val.strip()

opmix_series = f"v{opmix_data['major']}.{opmix_data['minor']}.x"
opmix_ver = f"v{opmix_data['major']}.{opmix_data['minor']}.{opmix_data['release']}{opmix_data['greek']}"
std_ver = f"{opmix_data['std_major']}.{opmix_data['std_minor']}"

hwloc_min_version = f"{opmix_data['hwloc_min_version']}"
event_min_version = f"{opmix_data['event_min_version']}"
automake_min_version = f"{opmix_data['automake_min_version']}"
autoconf_min_version = f"{opmix_data['autoconf_min_version']}"
libtool_min_version = f"{opmix_data['libtool_min_version']}"
flex_min_version = f"{opmix_data['flex_min_version']}"

# "release" is a sphinx config variable: assign it to the computed
# OpenPMIx version number.  The opmix_ver string begins with a "v"; the
# Sphinx release variable should not include this prefix "v".
release = opmix_ver[1:]

# If we are building in a ReadTheDocs.io environment, there will be a
# READTHEDOCS environment variables that tell us what version to use.
# Relevant RTD env variables (documented
# https://docs.readthedocs.io/en/stable/builds.html#build-environment):
#
# - READTHEDOCS: will be "True"
# - READTHEDOCS_VERSION: The RTD slug of the version which is being
#   built (e.g., "latest")
# - READTHEDOCS_VERSION_NAME: Corresponding version name as displayed
#   in RTD's version switch menu (e.g., "stable")
# - READTHEDOCS_VERSION_TYPE: Type of the event triggering the build
#   (e.g., "branch", "tag", "external" (for PRs), or "unknown").
#
# If we're building in an RTD environment for a tag or external (i.e.,
# PR), use the RTD version -- not what we just read from the VERSIONS
# file.
import os
key = 'READTHEDOCS'
if key in os.environ and os.environ[key] == 'True':
    print("OpenPMIx: found ReadTheDocs build environment")

    rtd_v = os.environ['READTHEDOCS_VERSION']
    if os.environ['READTHEDOCS_VERSION_TYPE'] == 'external':
        # Make "release" be shorter than the full "opmix_ver" value.
        release = f'PR #{rtd_v}'
        opmix_ver += f' (Github PR #{rtd_v})'
    else:
        opmix_ver = rtd_v

        # The "release" Sphinx variable is supposed to be expressed as
        # a simple value, such as "A.B.C[rcX]" (without a leading
        # "v").  The opmix_ver value will be one of two things:
        #
        # - a git branch name (of the form "vA.B.x")
        # - a git tag (of the form "A.B.C[rcX]")
        #
        # If there's a leading "v", we need to strip it.
        release = opmix_ver
        if opmix_ver[0] == 'v':
            release = opmix_ver[1:]

    # Override the branch names "master" and "main" (that would have
    # come from the ReadTheDocs version slug) to be "head of
    # development".
    if release == 'main' or release == 'master':
        opmix_ver = 'head of development'

    print(f"OpenPMIx: release = {release}")
    print(f"OpenPMIx: opmix_ver = {opmix_ver}")

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

man_pages = list()
for root, dirs, files in os.walk("man"):
    for filename in files:
        # Parse filenames of the format a "foo.X.rst"
        parts = re.search(r"^([^/]+?).([0-9]+).rst$", filename)

        # Skip files that do not match that format (e.g.,
        # "index.rst")
        if parts is None:
            continue

        base_name = parts.group(1)
        section   = int(parts.group(2))

        full_filename_without_rst = f'{root}/{base_name}.{section}'

        # Append a tuple: (filename, name, description, authors, section)
        # We leave description blank.
        # We also leave authors blank, because "The OpenPMIx community"
        # already shows up in the copyright section.
        tuple = (full_filename_without_rst, base_name, '', '', section)
        man_pages.append(tuple)

# -- OpenPMIx-specific options -----------------------------------------------

# This prolog is included in every file.  Put common stuff here.

rst_prolog = f"""
.. |mdash|  unicode:: U+02014 .. Em dash
.. |rarrow| unicode:: U+02192 .. Right arrow

.. |year| replace:: {year}
.. |date| replace:: {month}-{year}
.. |opmix_ver| replace:: {opmix_ver}
.. |opmix_series| replace:: {opmix_series}
.. |std_ver| replace:: {std_ver}
.. |hwloc_min_version| replace:: {hwloc_min_version}
.. |event_min_version| replace:: {event_min_version}
.. |automake_min_version| replace:: {automake_min_version}
.. |autoconf_min_version| replace:: {autoconf_min_version}
.. |libtool_min_version| replace:: {libtool_min_version}
.. |flex_min_version| replace:: {flex_min_version}

"""

# The sphinx_rtd_theme does not properly handle wrapping long lines in
# table cells when rendering to HTML due to a CSS issue (see
# https://github.com/readthedocs/sphinx_rtd_theme/issues/1505).  Until
# the issue is fixed upstream in sphinx_rtd_theme, we can simply
# override the CSS here.
rst_prolog += """
.. raw:: html

   <style>
   .wy-table-responsive table td,.wy-table-responsive table th{white-space:normal}
   </style>
"""
