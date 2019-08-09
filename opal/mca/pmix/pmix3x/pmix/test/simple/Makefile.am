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
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = -I$(top_builddir)/src -I$(top_builddir)/src/include -I$(top_builddir)/include -I$(top_builddir)/include/pmix

headers = simptest.h

noinst_PROGRAMS = simptest simpclient simppub simpdyn simpft simpdmodex \
                  test_pmix simptool simpdie simplegacy simptimeout \
                  gwtest gwclient stability quietclient simpjctrl

simptest_SOURCES = $(headers) \
        simptest.c
simptest_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
simptest_LDADD = \
    $(top_builddir)/src/libpmix.la

simpclient_SOURCES = $(headers) \
        simpclient.c
simpclient_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
simpclient_LDADD = \
    $(top_builddir)/src/libpmix.la

simppub_SOURCES = $(headers) \
        simppub.c
simppub_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
simppub_LDADD = \
    $(top_builddir)/src/libpmix.la

simpdmodex_SOURCES = $(headers) \
        simpdmodex.c
simpdmodex_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
simpdmodex_LDADD = \
    $(top_builddir)/src/libpmix.la

simpft_SOURCES = $(headers) \
        simpft.c
simpft_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
simpft_LDADD = \
    $(top_builddir)/src/libpmix.la

simpdyn_SOURCES = $(headers) \
        simpdyn.c
simpdyn_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
simpdyn_LDADD = \
    $(top_builddir)/src/libpmix.la

test_pmix_SOURCES = $(headers) \
        test_pmix.c
test_pmix_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
test_pmix_LDADD = \
    $(top_builddir)/src/libpmix.la

simptool_SOURCES = $(headers) \
        simptool.c
simptool_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
simptool_LDADD = \
    $(top_builddir)/src/libpmix.la

simpdie_SOURCES = $(headers) \
        simpdie.c
simpdie_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
simpdie_LDADD = \
    $(top_builddir)/src/libpmix.la

simplegacy_SOURCES = $(headers) \
        simplegacy.c
simplegacy_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
simplegacy_LDADD = \
    $(top_builddir)/src/libpmi.la

simptimeout_SOURCES = $(headers) \
        simptimeout.c
simptimeout_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
simptimeout_LDADD = \
    $(top_builddir)/src/libpmix.la

gwtest_SOURCES = $(headers) \
        gwtest.c
gwtest_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
gwtest_LDADD = \
    $(top_builddir)/src/libpmix.la

gwclient_SOURCES = $(headers) \
        gwclient.c
gwclient_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
gwclient_LDADD = \
    $(top_builddir)/src/libpmix.la

stability_SOURCES = $(headers) \
        stability.c
stability_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
stability_LDADD = \
    $(top_builddir)/src/libpmix.la

quietclient_SOURCES = $(headers) \
        quietclient.c
quietclient_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
quietclient_LDADD = \
    $(top_builddir)/src/libpmix.la

simpjctrl_SOURCES = \
        simpjctrl.c
simpjctrl_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
simpjctrl_LDADD = \
    $(top_builddir)/src/libpmix.la
