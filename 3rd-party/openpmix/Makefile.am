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
# Copyright (c) 2012-2013 Los Alamos National Security, Inc.  All rights reserved.
# Copyright (c) 2013-2022 Intel, Inc.  All rights reserved.
# Copyright (c) 2019      Amazon.com, Inc. or its affiliates.  All Rights
#                         reserved.
# Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# Note that the -I directory must *exactly* match what was specified
# via AC_CONFIG_MACRO_DIR in configure.ac.
ACLOCAL_AMFLAGS = -I ./config

SUBDIRS = config contrib include src etc bindings docs

AM_DISTCHECK_CONFIGURE_FLAGS = --disable-dlopen

headers =
sources =
nodist_headers =
EXTRA_DIST = AUTHORS README.md VERSION LICENSE autogen.pl

# Only install the valgrind suppressions file and man pages
# if we're building in standalone mode
dist_pmixdata_DATA =
dist_pmixdata_DATA += contrib/pmix-valgrind.supp

if PMIX_TESTS_EXAMPLES
SUBDIRS += . test/test_v2 test examples
endif

pmixdir = $(pmixincludedir)/$(subdir)
nobase_pmix_HEADERS = $(headers)

dist-hook:
	env LS_COLORS= sh "$(top_srcdir)/config/distscript.sh" "$(top_srcdir)" "$(distdir)" "$(PMIX_VERSION)" "$(PMIX_REPO_REV)"

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = maint/pmix.pc
