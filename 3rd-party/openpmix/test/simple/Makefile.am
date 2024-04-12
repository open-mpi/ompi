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
# Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = -I$(top_builddir)/src -I$(top_builddir)/src/include -I$(top_builddir)/include -I$(top_builddir)/include/pmix
# we do NOT want picky compilers down here

headers = simptest.h

noinst_PROGRAMS = simptest simpclient simppub simpdyn simpft simpdmodex \
                  test_pmix simptool simpdie simptimeout \
                  gwtest gwclient stability quietclient simpjctrl simpio simpsched \
                  simpcoord simpcycle doubleget simpfabric get_put_example simpvni \
                  hybrid simpqual legacy

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

simpjctrl_SOURCES = $(headers) \
        simpjctrl.c
simpjctrl_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
simpjctrl_LDADD = \
    $(top_builddir)/src/libpmix.la

simpio_SOURCES = $(headers) \
        simpio.c
simpio_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
simpio_LDADD = \
    $(top_builddir)/src/libpmix.la

simpsched_SOURCES = $(headers) \
        simpsched.c
simpsched_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
simpsched_LDADD = \
    $(top_builddir)/src/libpmix.la

simpcoord_SOURCES = $(headers) \
        simpcoord.c
simpcoord_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
simpcoord_LDADD = \
    $(top_builddir)/src/libpmix.la

simpcycle_SOURCES = $(headers) \
        simpcycle.c
simpcycle_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
simpcycle_LDADD = \
    $(top_builddir)/src/libpmix.la

doubleget_SOURCES = $(headers) \
        doubleget.c
doubleget_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
doubleget_LDADD = \
    $(top_builddir)/src/libpmix.la

simpfabric_SOURCES = $(headers) \
        simpfabric.c
simpfabric_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
simpfabric_LDADD = \
    $(top_builddir)/src/libpmix.la

get_put_example_SOURCES = $(headers) \
        get_put_example.c
get_put_example_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
get_put_example_LDADD = \
    $(top_builddir)/src/libpmix.la

simpvni_SOURCES = $(headers) \
        simpvni.c
simpvni_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
simpvni_LDADD = \
    $(top_builddir)/src/libpmix.la

hybrid_SOURCES = $(headers) \
        hybrid.c
hybrid_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
hybrid_LDADD = \
    $(top_builddir)/src/libpmix.la

simpqual_SOURCES = $(headers) \
        simpqual.c
simpqual_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
simpqual_LDADD = \
    $(top_builddir)/src/libpmix.la

legacy_SOURCES = $(headers) \
        legacy.c
legacy_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
legacy_LDADD = \
    $(top_builddir)/src/libpmix.la
