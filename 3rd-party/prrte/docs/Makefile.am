#
# Copyright (c) 2022-2023 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.
#
# Copyright (c) 2023-2024 Nanook Consulting  All rights reserved.
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

OUTDIR             = _build
SPHINX_CONFIG      = conf.py
SPHINX_OPTS       ?= -W --keep-going -j auto

# Note: it is significantly more convenient to list all the source
# files here using wildcards (vs. listing every single .rst file).
# However, it is necessary to list $(srcdir) when using wildcards.
RST_SOURCE_FILES = \
        $(srcdir)/*.rst \
        $(srcdir)/prrte-rst-content/*.rst \
        $(srcdir)/placement/*.rst \
        $(srcdir)/hosts/*.rst \
        $(srcdir)/how-things-work/*.rst \
        $(srcdir)/developers/*.rst \
        $(srcdir)/man/*.rst \
        $(srcdir)/man/man1/*.rst \
        $(srcdir)/man/man5/*.rst \
        $(srcdir)/news/*.rst

EXTRA_DIST = \
        requirements.txt \
        _templates/configurator.html \
        $(SPHINX_CONFIG) \
        $(RST_SOURCE_FILES)

###########################################################################

# Note: we list all the man pages explicitly (as opposed to using
# wildcards, like we do to list all the RST source files in
# EXTRA_DIST) because these files get installed by Automake.  There
# are less complications and portability issues (between GNU and BSD
# make(1), for example) if all the files to be installed are
# explicitly listed.

PRTE_MAN1 = \
        prte.1 \
        prte_info.1 \
        prted.1 \
        prterun.1 \
        prun.1 \
        pterm.1

PRTE_MAN5 = \
        prte.5

MAN_OUTDIR = $(OUTDIR)/man

PRTE_MAN1_RST = $(PRTE_MAN1:%.1=man/man1/%.1.rst)
PRTE_MAN1_BUILT = $(PRTE_MAN1:%.1=$(MAN_OUTDIR)/%.1)

PRTE_MAN5_RST = $(PRTE_MAN5:%.5=man/man5/%.5.rst)
PRTE_MAN5_BUILT = $(PRTE_MAN5:%.5=$(MAN_OUTDIR)/%.5)

#-------------------

EXTRA_DIST += \
        $(PRTE_MAN1_BUILT) \
        $(PRTE_MAN5_BUILT)

###########################################################################
if PRTE_BUILD_DOCS

include $(top_srcdir)/Makefile.prte-rules

# Have to not list these targets in EXTRA_DIST outside of the
# PRTE_BUILD_DOCS conditional because "make dist" will fail due to
# these missing targets (and therefore not run the "dist-hook" target
# in the top-level Makefile, which prints a pretty message about why
# "make dist" failed).
#
# We list the entire directory trees (html and man) to grab all
# generated files in them.
EXTRA_DIST += \
        $(OUTDIR)/html \
        $(OUTDIR)/man

ALL_MAN_BUILT = \
        $(PRTE_MAN1_BUILT) $(PRTE_MAN5_BUILT)
$(ALL_MAN_BUILT): $(RST_SOURCE_FILES) $(IMAGE_SOURCE_FILES)
$(ALL_MAN_BUILT): $(TEXT_SOURCE_FILES) $(SPHINX_CONFIG)

# List both commands (HTML and man) in a single rule because they
# really need to be run in serial.  Specifically, if they were two
# different rules and someone ran "make -j", then both of them could
# be writing to $(OUTDIR)/doctrees simultaneously, which would be Bad.
# Use one of the man pages as a sentinel file to indicate whether all
# the HTML docs and man pages have been built.
$(ALL_MAN_BUILT):
	$(PRTE_V_SPHINX_HTML) $(SPHINX_BUILD) -M html "$(srcdir)" "$(OUTDIR)" $(SPHINX_OPTS)
	$(PRTE_V_SPHINX_MAN) $(SPHINX_BUILD) -M man "$(srcdir)" "$(OUTDIR)" $(SPHINX_OPTS)

# A useful rule to invoke manually to ensure that all of the external
# HTML links we have are valid.  Running this rule requires
# connectivity to the general internet.
linkcheck:
	$(SPHINX_BUILD) -M linkcheck "$(srcdir)" "$(OUTDIR)" $(SPHINX_OPTS)

.PHONY: linkcheck

maintainer-clean-local:
	$(SPHINX_BUILD) -M clean "$(srcdir)" "$(OUTDIR)" $(SPHINX_OPTS)

# List all the built man pages here in the Automake BUILT_SOURCES
# macro.  This hooks into the normal Automake build mechanisms, and
# will ultimately cause the invocation of the above rule that runs
# Sphinx to build the HTML and man pages.
BUILT_SOURCES = $(ALL_MAN_BUILT)

endif PRTE_BUILD_DOCS

###########################################################################
if PRTE_INSTALL_DOCS

man1_MANS = $(PRTE_MAN1_BUILT)
man5_MANS = $(PRTE_MAN5_BUILT)

# We do not know the names of all the generated HTML files: we only
# know that the generated tree will of files be in _build/html/.
# Unfortunately, Automake's installation process only handles
# individual files -- not entire trees of files.  Hence, we use a
# "find ..." to copy the entire generated tree to the target
# directory.
#
# Note: we do not use "cp -r ..." because if the source directory is
# write-protected (e.g., during "make distcheck"), then the cp -r will
# also write-protect the target directory.  Instead, use the
# Automake-provided install macros to set desirable permissions on the
# target directories and files.
#
# Since this might be a VPATH build, first check to see if _build/html
# exists in the source tree.  If not, do the find+install from the
# build tree.
install-data-hook:
	$(MKDIR_P) $(DESTDIR)$(docdir)
	if test -d $(srcdir)/_build/html; then \
	    topdir=$(srcdir)/_build; \
	else \
	    topdir=_build; \
	fi; \
	cd $$topdir; \
	find html -type d -exec $(mkinstalldirs) $(DESTDIR)$(docdir)/{} \; ; \
	find html -type f -exec $(INSTALL_DATA) {} $(DESTDIR)$(docdir)/{} \;

uninstall-hook:
	rm -rf $(DESTDIR)$(docdir)

endif PRTE_INSTALL_DOCS
