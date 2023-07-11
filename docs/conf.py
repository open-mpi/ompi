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

# -- Project information -----------------------------------------------------

import datetime
year = datetime.datetime.now().year

project = 'Open MPI'
copyright = f'2003-{year}, The Open MPI Community'
author = 'The Open MPI Community'

# The full version, including alpha/beta/rc tags
# Read the Open MPI version from the VERSION file in the source tree
# The docs/Makefile.am will set the env var OMPI_VERSION_FILE, because
# we might be doing a VPATH build.
filename = None
if 'OMPI_VERSION_FILE' in os.environ:
    filename = os.environ['OMPI_VERSION_FILE']
elif os.path.exists("../VERSION"):
    filename = '../VERSION'

if filename is None:
    print("ERROR: Could not find Open MPI source tree VERSION file")
    exit(1)

with open(filename) as fp:
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

ompi_series = f"v{ompi_data['major']}.{ompi_data['minor']}.x"
ompi_ver = f"v{ompi_data['major']}.{ompi_data['minor']}.{ompi_data['release']}{ompi_data['greek']}"

pmix_min_version = f"{ompi_data['pmix_min_version']}"
prte_min_version = f"{ompi_data['prte_min_version']}"
hwloc_min_version = f"{ompi_data['hwloc_min_version']}"
event_min_version = f"{ompi_data['event_min_version']}"
automake_min_version = f"{ompi_data['automake_min_version']}"
autoconf_min_version = f"{ompi_data['autoconf_min_version']}"
libtool_min_version = f"{ompi_data['libtool_min_version']}"
flex_min_version = f"{ompi_data['flex_min_version']}"
mpi_standard_major_version = f"{ompi_data['mpi_standard_version']}"
mpi_standard_minor_version = f"{ompi_data['mpi_standard_subversion']}"

# "release" is a sphinx config variable: assign it to the computed
# Open MPI version number.  The ompi_ver string begins with a "v"; the
# Sphinx release variable should not include this prefix "v".
release = ompi_ver[1:]

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
    print("OMPI: found ReadTheDocs build environment")

    rtd_v = os.environ['READTHEDOCS_VERSION']
    if os.environ['READTHEDOCS_VERSION_TYPE'] == 'external':
        # Make "release" be shorter than the full "ompi_ver" value.
        release = f'PR #{rtd_v}'
        ompi_ver += f' (Github PR #{rtd_v})'
    else:
        ompi_ver = rtd_v

        # The "release" Sphinx variable is supposed to be expressed as
        # a simple value, such as "A.B.C[rcX]" (without a leading
        # "v").  The ompi_ver value will be one of two things:
        #
        # - a git branch name (of the form "vA.B.x")
        # - a git tag (of the form "A.B.C[rcX]")
        #
        # If there's a leading "v", we need to strip it.
        release = ompi_ver
        if ompi_ver[0] == 'v':
            release = ompi_ver[1:]

    # Override the branch names "master" and "main" (that would have
    # come from the ReadTheDocs version slug) to be "head of
    # development".
    if release == 'main' or release == 'master':
        ompi_ver = 'head of development'

    print(f"OMPI: release = {release}")
    print(f"OMPI: ompi_ver = {ompi_ver}")

# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
import sphinx_rtd_theme
extensions = [
    'recommonmark',
    "sphinx_rtd_theme",
    "sphinx.ext.extlinks",
]

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store', 'venv', 'py*/**']


# Clarify the language for verbatim blocks (::)
# Never change this to anything else. Lots of verbatim code blocks
# exists across the documentation pages.
# The default value is actually "default" which is something like Python3
# and on fail it reverts to "none". We just go directly to none for clarity.
highlight_language = "none"



# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
html_theme = 'sphinx_rtd_theme'

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
#html_static_path = ['_static']


# Short hand external links
# Allows smoother transitioning for commonly used articles and sites
extlinks = {
    'doi': ('https://doi.org/%s', '%s'),
}


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
.. |ompi_ver| replace:: {ompi_ver}
.. |ompi_series| replace:: {ompi_series}
.. |pmix_min_version| replace:: {pmix_min_version}
.. |prte_min_version| replace:: {prte_min_version}
.. |hwloc_min_version| replace:: {hwloc_min_version}
.. |event_min_version| replace:: {event_min_version}
.. |automake_min_version| replace:: {automake_min_version}
.. |autoconf_min_version| replace:: {autoconf_min_version}
.. |libtool_min_version| replace:: {libtool_min_version}
.. |flex_min_version| replace:: {flex_min_version}
.. |mpi_standard_version| replace:: {mpi_standard_major_version}.{mpi_standard_minor_version}
.. |mpi_standard_major_version| replace:: {mpi_standard_major_version}
.. |mpi_standard_minor_version| replace:: {mpi_standard_minor_version}
.. |deprecated_favor| replace:: this routine is deprecated in favor of

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
