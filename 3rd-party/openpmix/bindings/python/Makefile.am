#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2010 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012-2013 Los Alamos National Security, Inc.  All rights reserved.
# Copyright (c) 2013-2019 Intel, Inc.  All rights reserved.
# Copyright (c) 2018      Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# Copyright (c) 2021      IBM Corporation.  All rights reserved.
# Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# We do NOT want picky compilers down here
CFLAGS = $(PMIX_CFLAGS_BEFORE_PICKY)

helpers = \
        setup.py \
        construct.py

src_files = \
        pmix.pxi \
        pmix.pyx

EXTRA_DIST = $(helpers) $(src_files)

if WANT_PYTHON_BINDINGS

harvested_headers = \
        $(top_srcdir)/include/pmix.h \
        $(top_builddir)/include/pmix_common.h \
        $(top_srcdir)/include/pmix_deprecated.h \
        $(top_srcdir)/include/pmix_server.h \
        $(top_srcdir)/include/pmix_tool.h

constants_files = \
        pmix_constants.pxd \
        pmix_constants.pxi

# Create pmix_constants.pxd and pmix_constants.pxi by harvesting a
# bunch of PMIx header files These will be generated in the PMIx build
# dir.
$(constants_files): construct.py $(harvested_headers)
	$(PYTHON) $(top_srcdir)/bindings/python/construct.py --src="$(top_builddir)/include" --include-dir="$(top_srcdir)/include"

# Cython does not natively understand VPATH builds.  Specifically,
# Cython will build things in the same tree where it finds
# pmix.[pxi|pyx] (which is the PMIx source tree).
#
# Cython's "setup.py build_ext
# --build-lib=SOMEWHERE_IN_THE_BUILD_TREE" *almost* does what we want.
# But even though it makes the build/install trees in the directory we
# tell it (vs. the source tree), it still writes pmix.c in the source
# tree, which is definitely not what we want.
#
# We want to properly support VPATH builds, so we fake out Cython and
# force it to build in the PMIx build tree by making sym links for
# pmix.[pxi|pyx] from the source tree to the build tree.  Cython can
# then find these files in the build tree, and build everything there
# (including pmix.c).
#
# This is a little gross, but it works. :-/
$(PMIX_TOP_BUILDDIR)/bindings/python/pmix.pxi:
	$(LN_S) $(PMIX_TOP_SRCDIR)/bindings/python/pmix.pxi $(PMIX_TOP_BUILDDIR)/bindings/python/pmix.pxi
$(PMIX_TOP_BUILDDIR)/bindings/python/pmix.pyx:
	$(LN_S) $(PMIX_TOP_SRCDIR)/bindings/python/pmix.pyx $(PMIX_TOP_BUILDDIR)/bindings/python/pmix.pyx

# When we run "setup.pu build_ext ...", several files/directories are
# created.  The exact files / directory names may be system-dependent,
# so we don't try to check for most of them.  Instead, we just check
# for one sentinel file that is always created during this step:
# pmix.c, and use that as a proxy for all the other things that were
# created during this step.
SENTINEL_FILE = pmix.c

# Create $(SENTINEL_FILE) from its sources and the constants we
# harvested from various PMIx header files.  Ensure to depend on the
# pmix.[pxi|pyx] in the build tree so that those rules get triggered
# and we get sym links over to the source tree, if necessary.
$(SENTINEL_FILE): $(PMIX_TOP_BUILDDIR)/bindings/python/pmix.pxi
$(SENTINEL_FILE): $(PMIX_TOP_BUILDDIR)/bindings/python/pmix.pyx
$(SENTINEL_FILE): $(constants_files)
	PMIX_BINDINGS_TOP_SRCDIR=$(PMIX_TOP_SRCDIR) PMIX_BINDINGS_TOP_BUILDDIR=$(PMIX_TOP_BUILDDIR) \
		$(PYTHON) $(top_srcdir)/bindings/python/setup.py build_ext --library-dirs="$(PMIX_TOP_BUILDDIR)/src/.libs" --user

BUILT_SOURCES = $(constants_files)

# We don't put $(SENTINEL_FILE) BUILT_SOURCES because then "make dist"
# will fail if it can't be created (e.g., if no suitable Python +
# Cython + etc. is available) -- because the files in BUILT_SOURCES
# are dependencies of "distdir".
#
# More specifically: $(SENTINEL_FILE) isn't shipped in the tarball, so
# it doesn't seem right to require it to be able to be built for "make
# dist" to succeed.  Hence, we (slightly) abuse Automake's "noinst"
# and "DATA" to force $(SENTINEL_FILE) to be built during "make all"
# when WANT_PYTHON_BINDINGS is true.
noinst_DATA = $(SENTINEL_FILE)

install-exec-local:
	$(MKDIR_P) $(DESTDIR)$(pythondir)
	PYTHONPATH=$$PYTHONPATH:$(DESTDIR)$(pythondir):$(DESTDIR)$(pyexecdir) \
		PMIX_BINDINGS_TOP_SRCDIR=$(PMIX_TOP_SRCDIR) PMIX_BINDINGS_TOP_BUILDDIR=$(PMIX_TOP_BUILDDIR) \
		$(PYTHON) $(top_srcdir)/bindings/python/setup.py install --quiet --prefix="$(DESTDIR)$(prefix)"

# Be careful with easy-install.pth -- we may not be the only entity
# that put things in there.  Therefore, carefully remove the pmix
# entry from it, and then remove the file altogether if is then
# otherwise empty.  Finally, remove the installed pypmix Python egg.
uninstall-hook:
	-@file=$(pythondir)/easy-install.pth; \
	tmpfile=$(pythondir)/easy-install.pth.$$; \
	if test -r $$file; then \
	    grep -v /pypmix- $$file > $$tmpfile; \
	    diff -q $$file $$tmpfile 2>&1 >/dev/null; \
	    if test $$? -ne 0; then \
	        cp $$tmpfile $$file; \
	    fi; \
	    rm -f $$tmpfile; \
	    if test ! -s $$file; then \
	        rm -f $$file; \
	    fi; \
	fi
	rm -rf $(pythondir)/pypmix*

CLEANFILES += $(constants_files)

# Also delete the other files and dirs that Cython creates during its
# "setup.py build_ext ..." step.  We need to use clean-local because
# the names in CLEANFILE are not "rm -rf"'ed.
clean-local:
	rm -rf $(SENTINEL_FILE) build dist pypmix.egg-info

endif # WANT_PYTHON_BINDINGS
