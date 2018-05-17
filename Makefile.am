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
# Copyright (c) 2006-2016 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012-2015 Los Alamos National Security, Inc.  All rights reserved.
# Copyright (c) 2014      Intel, Inc. All rights reserved.
# Copyright (c) 2017-2018 Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

SUBDIRS = config contrib $(MCA_PROJECT_SUBDIRS) test
DIST_SUBDIRS = config contrib $(MCA_PROJECT_DIST_SUBDIRS) test
EXTRA_DIST = README INSTALL VERSION Doxyfile LICENSE autogen.pl README.JAVA.txt AUTHORS

include examples/Makefile.include

dist-hook:
	env LS_COLORS= sh "$(top_srcdir)/config/distscript.sh" "$(top_srcdir)" "$(distdir)" "$(OMPI_REPO_REV)"
	@if test ! -s $(distdir)/AUTHORS ; then \
		echo "AUTHORS file is empty; aborting distribution"; \
		exit 1; \
	fi

# Check for common symbols.  Use a "-hook" to increase the odds that a
# developer will see it at the end of their installation process.
install-exec-hook:
	-@if test -d "$(top_srcdir)/.git"; then \
	    $(top_srcdir)/config/find_common_syms \
	        --brief \
	        --top_builddir=$(top_builddir) \
	        --top_srcdir=$(top_srcdir) \
	        --objext=$(OBJEXT); \
	fi

ACLOCAL_AMFLAGS = -I config

# Use EXTRA_DIST and an explicit target (with a FORCE hack so that
# it's always run) rather than a dist-hook because there's some magic
# extra logic in Automake that will add AUTHORS to EXTRA_DIST if the
# file exists when Automake is run.  Once we're explicit (to avoid odd
# copy behavior), it's easier to always build AUTHORS here, rather
# than trying to handle the EXTRA_DIST dependency from a clean repo
# (no AUTHORS file present) and use dist-hook to run every time.
AUTHORS: FORCE
	$(PERL) "$(top_srcdir)/contrib/dist/make-authors.pl" --skip-ok --quiet --srcdir="$(top_srcdir)"

FORCE:
