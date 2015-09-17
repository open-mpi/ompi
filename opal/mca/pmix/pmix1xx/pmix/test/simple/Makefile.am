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

bin_PROGRAMS = simptest simpclient simppub simpdmodex simpft simpdyn

simptest_SOURCES = \
        simptest.c

simptest_LDADD = \
	$(top_builddir)/libpmix.la

simpclient_SOURCES = \
        simpclient.c

simpclient_LDADD = \
	$(top_builddir)/libpmix.la

simppub_SOURCES = \
        simppub.c

simppub_LDADD = \
	$(top_builddir)/libpmix.la

simpdmodex_SOURCES = \
        simpdmodex.c

simpdmodex_LDADD = \
	$(top_builddir)/libpmix.la

simpft_SOURCES = \
        simpft.c

simpft_LDADD = \
	$(top_builddir)/libpmix.la

simpdyn_SOURCES = \
        simpdyn.c

simpdyn_LDADD = \
	$(top_builddir)/libpmix.la
