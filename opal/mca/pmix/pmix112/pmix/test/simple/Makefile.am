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
# Copyright (c) 2013-2015 Intel, Inc. All rights reserved
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = -I$(top_builddir)/src -I$(top_builddir)/src/include -I$(top_builddir)/include -I$(top_builddir)/include/pmix

check_PROGRAMS = simptest simpclient simppub simpdyn simpft simpdmodex

# TESTS = simptest

simptest_SOURCES = \
        simptest.c
simptest_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
simptest_LDADD = \
	$(top_builddir)/libpmix.la

simpclient_SOURCES = \
        simpclient.c
simpclient_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
simpclient_LDADD = \
	$(top_builddir)/libpmix.la

simppub_SOURCES = \
        simppub.c
simppub_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
simppub_LDADD = \
	$(top_builddir)/libpmix.la

simpdmodex_SOURCES = \
        simpdmodex.c
simpdmodex_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
simpdmodex_LDADD = \
	$(top_builddir)/libpmix.la

simpft_SOURCES = \
        simpft.c
simpft_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
simpft_LDADD = \
	$(top_builddir)/libpmix.la

simpdyn_SOURCES = \
        simpdyn.c
simpdyn_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
simpdyn_LDADD = \
	$(top_builddir)/libpmix.la
