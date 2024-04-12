#
# Copyright (c) 2022-2023 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.
# Copyright (c) 2023-2024 Nanook Consulting  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# We need this Makefile to be executed serially.  Below, we list all
# the man pages as the targets of the rule that invokes Sphinx for
# dependency/generation reasons.  But a *single* execution of the make
# target will generate *all* of the man pages and HTML files.  Hence,
# when "make" determines that none of the man page files exist, it
# should execute the Sphinx-invocation rule once, and then it will
# realize that all the man pages files exist.  More specifically: if
# someone invokes "make -j N", we need make to not execute the
# Sphinx-invocation rule multiple times simultaneously.  Both GNU Make
# and BSD Make will honor the .NOTPARALLEL target to disable all
# parallel invocation in this Makefile[.am].
#
# Note that even though we explicitly disable make's parallelism,
# we'll use Sphinx's internal parallelism via "-j auto" -- see
# SPHINX_OPTS.
.NOTPARALLEL:

OUTDIR         = _build
SPHINX_CONFIG  = conf.py
SPHINX_OPTS   ?= -W --keep-going -j auto

# All RST source files, including those that are not installed
RST_SOURCE_FILES = \
        $(srcdir)/prrte-rst-content/*.rst \
        $(srcdir)/index.rst \
        $(srcdir)/help-prte-info.rst \
        $(srcdir)/help-prte.rst \
        $(srcdir)/help-prted.rst \
        $(srcdir)/help-prterun.rst \
        $(srcdir)/help-prun.rst \
        $(srcdir)/help-pterm.rst \
        $(srcdir)/help-cli.rst \
        $(srcdir)/help-dash-host.rst \
        $(srcdir)/help-hostfile.rst \
        $(srcdir)/help-prte-hwloc-base.rst \
        $(srcdir)/help-prte-runtime.rst \
        $(srcdir)/help-prte-util.rst

TXT_OUTDIR = $(OUTDIR)/text

# These files are the ones that are built that we care about (Sphinx
# will emit lots of files in the TXT_OUTDIR; these are the only ones
# that we care about).
ALL_TXT_BUILT = \
        $(TXT_OUTDIR)/help-prte-info.txt \
        $(TXT_OUTDIR)/help-prte.txt \
        $(TXT_OUTDIR)/help-prted.txt \
        $(TXT_OUTDIR)/help-prterun.txt \
        $(TXT_OUTDIR)/help-prun.txt \
        $(TXT_OUTDIR)/help-pterm.txt \
        $(TXT_OUTDIR)/help-cli.txt \
        $(TXT_OUTDIR)/help-dash-host.txt \
        $(TXT_OUTDIR)/help-hostfile.txt \
        $(TXT_OUTDIR)/help-prte-hwloc-base.txt \
        $(TXT_OUTDIR)/help-prte-runtime.txt \
        $(TXT_OUTDIR)/help-prte-util.txt

#-------------------

EXTRA_DIST = \
        requirements.txt \
        $(SPHINX_CONFIG) \
        $(RST_SOURCE_FILES) \
        $(TXT_OUTDIR)

###########################################################################

include $(top_srcdir)/Makefile.prte-rules

# We do not specifically compute or list the RST file dependencies.
# Instead, because Sphinx will do this computation by itself, we just
# say "all Sphinx output is dependent upon all RST input files" here
# in the Makefile.  If any of the input RST files are edited, this
# will cause "make" to invoke Sphinx and let it re-generate whatever
# output files need to be re-generated.
#
# Hence, we use a single sentinel file here in the Makefile to
# represent all Sphinx output files.
TXT_SENTINEL_OUTPUT = $(TXT_OUTDIR)/index.txt
$(ALL_TXT_BUILT): $(TXT_SENTINEL_OUTPUT)

$(TXT_SENTINEL_OUTPUT): $(RST_SOURCE_FILES)
$(TXT_SENTINEL_OUTPUT): $(SPHINX_CONFIG)

# Technically, we always build the docs files if they're missing
# (e.g., in a git clone).  The "PRTE_BUILD_DOCS" conditional really
# refers to whether we have Sphinx or not.
#
# If we have Sphinx, use it to build.
#
# If we do not have Sphinx, then build dummy help files that say "you
# don't have Sphinx, so you didn't get proper help files" files, just
# so that it's obvious as to why you don't have proper help files.
$(TXT_SENTINEL_OUTPUT):
if PRTE_BUILD_DOCS
	$(PRTE_V_TXT) $(SPHINX_BUILD) -M text "$(srcdir)" "$(OUTDIR)" $(SPHINX_OPTS)
else !PRTE_BUILD_DOCS
	$(PRTE_V_TXT) \
	$(PYTHON) $(srcdir)/build-dummy-ini-files.py $(TXT_OUTDIR) $(ALL_TXT_BUILT); \
	touch $(TXT_SENTINEL_OUTPUT)
endif !PRTE_BUILD_DOCS

maintainer-clean-local:
	$(SPHINX_BUILD) -M clean "$(srcdir)" "$(OUTDIR)" $(SPHINX_OPTS)

###########################################################################

# Install the generated text files
dist_prtedata_DATA = $(ALL_TXT_BUILT)
