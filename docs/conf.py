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

import os
import re
import datetime
import sphobjinv as soi

year = datetime.datetime.now().year

project = 'Open MPI'
copyright = f'2003-{year}, The Open MPI Community'
author = 'The Open MPI Community'

# ---------------------------

# The docs/Makefile.am will set the env var OMPI_TOP_SRCDIR, because
# we might be doing a VPATH build.
ompi_top_srcdir = os.environ.get('OMPI_TOP_SRCDIR', '..')

# Read an Open MPI-style VERSION file
def read_version_file(path):
    if not os.path.exists(path):
        print(f"ERROR: Unable to find file {path}")
        exit(1)

    with open(path) as fp:
        version_lines = fp.readlines()

    data = dict()
    for line in version_lines:
        if '#' in line:
            parts = line.split("#")
            line = parts[0]
        line = line.strip()

        if '=' not in line:
            continue

        key, val = line.split("=")
        data[key.strip()] = val.strip()

    return data

# Look for a version string via a regular expresion of a filename in a
# given directory
def get_tarball_version(path, expr):
    if not os.path.exists(path):
        print(f"ERROR: Unable to find path {path}")
        exit(1)

    for file in os.listdir(path):
        m = re.match(expr, file)
        if not m:
            continue
        return m.group(1)

    return ""

# Read all the various versions from the source tree

ompi_data = read_version_file(f"{ompi_top_srcdir}/VERSION")
pmix_data = read_version_file(f"{ompi_top_srcdir}/3rd-party/openpmix/VERSION")
prte_data = read_version_file(f"{ompi_top_srcdir}/3rd-party/prrte/VERSION")

hwloc_embedded_version = get_tarball_version(f"{ompi_top_srcdir}/3rd-party/",
                                             r"hwloc-([^-]+).*\.tar")
event_embedded_version = get_tarball_version(f"{ompi_top_srcdir}/3rd-party/",
                                             r"libevent-([^-]+).*\.tar")

# ---------------------------

# Assemble several different combinations of version strings

ompi_series = f"v{ompi_data['major']}.{ompi_data['minor']}.x"
ompi_ver = f"v{ompi_data['major']}.{ompi_data['minor']}.{ompi_data['release']}{ompi_data['greek']}"

pmix_embedded_version = f"v{pmix_data['major']}.{pmix_data['minor']}.{pmix_data['release']}{pmix_data['greek']}"
prte_embedded_version = f"v{prte_data['major']}.{prte_data['minor']}.{prte_data['release']}{prte_data['greek']}"
prte_embedded_series = f"v{prte_data['major']}.{prte_data['minor']}"

pmix_min_version = f"{ompi_data['pmix_min_version']}"
prte_min_version = f"{ompi_data['prte_min_version']}"
hwloc_min_version = f"{ompi_data['hwloc_min_version']}"
event_min_version = f"{ompi_data['event_min_version']}"
automake_min_version = f"{ompi_data['automake_min_version']}"
autoconf_min_version = f"{ompi_data['autoconf_min_version']}"
libtool_min_version = f"{ompi_data['libtool_min_version']}"
flex_min_version = f"{ompi_data['flex_min_version']}"
python_min_version = f"{ompi_data['python_min_version']}"
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
if os.environ.get('READTHEDOCS') == 'True':
    print("OMPI: found ReadTheDocs build environment")

    # Tell Jinja2 templates the build is running on Read the Docs
    if "html_context" not in globals():
        html_context = {}
    html_context["READTHEDOCS"] = True

    # Define the canonical URL if you are using a custom domain on
    # Read the Docs
    html_baseurl = os.environ.get("READTHEDOCS_CANONICAL_URL", "")

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
    "sphinx.ext.intersphinx",
]

##########################################################################

# Map to external documentation: PMIx and PRRTE

def _make_intersphinx_mapping(project, name, fallback_base, entries):
    # If there is no PROJECT_NAME_DOCS_URL_BASE (e.g., in a ReadTheDocs
    # build), then use the fallback_base.
    key = f'{project}_{name}_DOCS_URL_BASE'.upper()
    docs_url_base = os.environ.get(key, fallback_base).strip()
    key = f'{project}_USING_INTERNAL_{name}'.upper()
    using_internal = os.environ.get(key, '0').strip()

    if using_internal == '0':
        # In this case, we're using some external URL base -- either on the
        # filesystem or via https.  Just use that directly.
        inv_filename = None
    else:
        # In this case, we're using the internal (embedded) version of
        # the project (e.g., PMIX or PRRTE).  Two things:
        #
        # 1. The internal PMIX / PRRTE docs have not yet been installed.
        #    Hence, we have to build our own objects.inv file to use during
        #    this Sphinx build.
        # 2. We have to use relative links because these links must work
        #    - in the installed tree
        #    - in a pre-built tarball of the docs (where we don't know
        #      the install prefix before building)
        #    NOTE: We specifically realize that these relative links won't
        #          work in the build tree (because the install tree and build
        #          tree will likely have different relative paths).  We have
        #          decided that this is acceptable -- the installed tree is
        #          the more important case to get right.

        # Use the official Sphinx Object Inventory library to build
        # an objects.inv file on the fly.  We know we only need a few specific
        # labels from the internal PMIX / PRRTE docs, so we will just
        # build those specific entries.
        #
        # 1. Initialize an empty inventory.  Since this is a fake inventory,
        # the name/version values don't matter.
        inv = soi.Inventory()
        inv.project = f'Open MPI {project}-{name} documentation'
        inv.version = '1.2.3'

        # 2. Make the single link that we need
        # - name: the unique ID for the link
        # - domain: either 'py' or 'std'; we want "std" for a label
        # - role: 'class', 'func', 'doc', 'label', etc.
        # - uri: the relative path to the HTML page
        # - dispname: what shows up in the link text ('-' means same as name)
        for label, uri in entries.items():
            inv.objects.append(soi.DataObjStr(
                name=label,
                domain='std',
                role='label',
                priority='-1',
                uri=uri,
                dispname='-'
            ))

        # 3. Export to a compressed objects.inv file
        text_data = inv.data_file(contract=True)
        zlib_data = soi.compress(text_data)
        inv_filename = f'{project}-{name}-objects.inv'
        soi.writebytes(inv_filename, zlib_data)

        # 4. Finally, figure out:
        #    - the docdir/html for where OMPI html docs will be installed
        #    - the docdir/html for where this project_name HTML docs will be installed
        #    Then compute the relative path between them.  This will be
        #    the URL base that we will use for intersphinx mapping.

        # OMPI HTML docdir
        docdir = os.environ.get('OMPI_DOCDIR', '.')
        docdir_html = os.path.join(docdir, 'html')

        # This project_name's HTML docdir
        docdir_parent = os.path.dirname(docdir)
        docdir_name_html = os.path.join(docdir_parent, name, 'html')

        # Compute the relative path between them
        docs_url_base = os.path.relpath(docdir_name_html, start=docdir_html)

    global intersphinx_mapping
    intersphinx_mapping[name] = (docs_url_base, inv_filename)

intersphinx_mapping = {}
_make_intersphinx_mapping("opal", "pmix", "https://docs.openpmix.org/en/latest/", {
    'man1-pmix_info' : 'man/man1/pmix_info.1.html#pmix-info',
    })
_make_intersphinx_mapping("ompi", "prrte", "https://docs.prrte.org/en/latest/", {
    'man1-prte_info' : 'man/man1/ompi-prte_info.1.html#man1-prte-info',
    })

# Sphinx defaults to automatically resolve *unresolved* labels using all your Intersphinx mappings.
# This behavior has unintended side-effects, namely that documentations local references can
# suddenly resolve to an external location.
# See also:
# https://www.sphinx-doc.org/en/master/usage/extensions/intersphinx.html#confval-intersphinx_disabled_reftypes
intersphinx_disabled_reftypes = ["*"]

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
#   here in the OMPI docs, but it also contains files that we do
#   *not* use here in the OMPI docs.
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
# not referencing from here in the OMPI docs.
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store', 'venv', 'py*/**',
                    'prrte-rst-content' ]


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

# Put a "Last updated on:" timestamp at the bottom of each page.
html_last_updated_fmt = '%Y-%m-%d %H:%M:%S %Z'

# Short hand external links
# Allows smoother transitioning for commonly used articles and sites
extlinks = {
    'doi': ('https://doi.org/%s', '%s'),
}


# -- Options for MAN output -------------------------------------------------

# Dynamically find all the man pages and build the appropriate list of
# tuples so that we don't have to manually maintain it.

def find_man_pages_top():
    def _doit(topdir):
        for root, dirs, files in os.walk(topdir):
            for filename in files:
                # Parse filenames of the format a "foo.X.rst"
                parts = re.search(r"^([^/]+?)\.([0-9]+)\.rst$", filename)

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
.. |pmix_embedded_version| replace:: {pmix_embedded_version}
.. |prte_min_version| replace:: {prte_min_version}
.. |prte_embedded_version| replace:: {prte_embedded_version}
.. |prte_embedded_series| replace:: {prte_embedded_series}
.. |hwloc_min_version| replace:: {hwloc_min_version}
.. |hwloc_embedded_version| replace:: {hwloc_embedded_version}
.. |event_min_version| replace:: {event_min_version}
.. |event_embedded_version| replace:: {event_embedded_version}
.. |automake_min_version| replace:: {automake_min_version}
.. |autoconf_min_version| replace:: {autoconf_min_version}
.. |libtool_min_version| replace:: {libtool_min_version}
.. |flex_min_version| replace:: {flex_min_version}
.. |python_min_version| replace:: {python_min_version}
.. |mpi_standard_version| replace:: {mpi_standard_major_version}.{mpi_standard_minor_version}
.. |mpi_standard_major_version| replace:: {mpi_standard_major_version}
.. |mpi_standard_minor_version| replace:: {mpi_standard_minor_version}
.. |deprecated_favor| replace:: this routine is deprecated in favor of

.. |br| raw:: html

   <br />

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
