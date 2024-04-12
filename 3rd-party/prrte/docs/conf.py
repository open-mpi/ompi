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
import sys

# -- Project information -----------------------------------------------------

needs_sphinx = '4.2'

import datetime
year = datetime.datetime.now().year

project = 'PMIx Reference Run Time Environment'
copyright = f'2003-{year}, The PRRTE Community'
author = 'The PRRTE Community'

# The full version, including alpha/beta/rc tags
# Read the PRRTE version from the VERSION file
# Note: this conf file lives in 2 different directories, so find the
# VERSION file relative to where we're running right now.
prte_lines = None
for dir in ['..', '../../..']:
    file = f'{dir}/VERSION'
    if os.path.exists(file):
        with open(file) as fp:
            prte_lines = fp.readlines()
            break

prte_data = dict()
for prte_line in prte_lines:
    if '#' in prte_line:
        parts = prte_line.split("#")
        prte_line = parts[0]
    prte_line = prte_line.strip()

    if '=' not in prte_line:
        continue

    prte_key, prte_val = prte_line.split("=")
    prte_data[prte_key.strip()] = prte_val.strip()

prte_series = f"v{prte_data['major']}.{prte_data['minor']}.x"
prte_ver = f"v{prte_data['major']}.{prte_data['minor']}.{prte_data['release']}{prte_data['greek']}"

pmix_min_version = f"{prte_data['pmix_min_version']}"
hwloc_min_version = f"{prte_data['hwloc_min_version']}"
event_min_version = f"{prte_data['event_min_version']}"
automake_min_version = f"{prte_data['automake_min_version']}"
autoconf_min_version = f"{prte_data['autoconf_min_version']}"
libtool_min_version = f"{prte_data['libtool_min_version']}"
flex_min_version = f"{prte_data['flex_min_version']}"

# "release" is a sphinx config variable: assign it to the computed
# PRRTE version number.  The prte_ver string begins with a "v"; the
# Sphinx release variable should not include this prefix "v".
release = prte_ver[1:]

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
    print("PRRTE: found ReadTheDocs build environment")

    rtd_v = os.environ['READTHEDOCS_VERSION']
    if os.environ['READTHEDOCS_VERSION_TYPE'] == 'external':
        # Make "release" be shorter than the full "prte_ver" value.
        release = f'PR #{rtd_v}'
        prte_ver += f' (Github PR #{rtd_v})'
    else:
        prte_ver = rtd_v

        # The "release" Sphinx variable is supposed to be expressed as
        # a simple value, such as "A.B.C[rcX]" (without a leading
        # "v").  The prte_ver value will be one of two things:
        #
        # - a git branch name (of the form "vA.B.x")
        # - a git tag (of the form "A.B.C[rcX]")
        #
        # If there's a leading "v", we need to strip it.
        release = prte_ver
        if prte_ver[0] == 'v':
            release = prte_ver[1:]

    # Override the branch names "master" and "main" (that would have
    # come from the ReadTheDocs version slug) to be "head of
    # development".
    if release == 'main' or release == 'master':
        prte_ver = 'head of development'

    print(f"PRRTE: release = {release}")
    print(f"PRRTE: prte_ver = {prte_ver}")

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
#
# Note that we exclude the entire prrte-rst-content/ directory.
# Here's why:
#
# * By default, Sphinx will warn about any .rst file that it finds in
#   the doc tree that is not referenced via either the "include"
#   directive or via a TOC.
# * The prrte-rst-content/ directory contains files that we *do* use
#   here in the PRRTE docs, but it also contains files that we do
#   *not* use here in the PRRTE docs (e.g., they're for downstream
#   PRRTE packagers).
# * Consequently, we explicitly ".. include:: <filename>" the specific
#   files that we want from the prrte-rst-content/ directory.  And we
#   specifically do *not* include at least some files in the
#   prrte-rst-content directory.
# * Listing files/patterns in exclude_patterns:
#   * Will prevent Sphinx from searching for/finding new .rst files
#     that match those patterns.
#   * Will not prevent explicitly ".. include:"'ing a file with a name
#     that matches a pattern in exclude_patterns.
#
# Hence, listing prrte-rst-content in exclude_patterns means that
# Sphinx won't complain about the .rst files in that tree that we are
# not referencing from here in the PRRTE docs.
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store', 'venv', 'py*/**',
                    'prrte-rst-content' ]


# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
html_theme = 'sphinx_rtd_theme'

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
#html_static_path = ['_static']

html_additional_pages = {
    'configurator' : 'configurator.html',
}

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
        # We also leave authors blank, because "The PRRTE community"
        # already shows up in the copyright section.
        tuple = (full_filename_without_rst, base_name, '', '', section)
        man_pages.append(tuple)

# -- Options for TEXT output ------------------------------------------------

text_sectionchars = '=-$#@!`'

# -- PRRTE-specific options -----------------------------------------------

# This prolog is included in every file.  Put common stuff here.

rst_prolog = f"""
.. |mdash|  unicode:: U+02014 .. Em dash
.. |rarrow| unicode:: U+02192 .. Right arrow

.. |year| replace:: {year}
.. |prte_ver| replace:: {prte_ver}
.. |prte_series| replace:: {prte_series}
.. |pmix_min_version| replace:: {pmix_min_version}
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
