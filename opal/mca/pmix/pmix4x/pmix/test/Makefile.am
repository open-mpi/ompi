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
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

if !WANT_HIDDEN
# these tests use internal symbols
# use --disable-visibility
SUBDIRS = simple
endif

headers = test_common.h cli_stages.h server_callbacks.h utils.h test_fence.h \
        test_publish.h test_spawn.h test_cd.h test_resolve_peers.h test_error.h \
        test_replace.h test_internal.h test_server.h

AM_CPPFLAGS = -I$(top_builddir)/src -I$(top_builddir)/src/include -I$(top_builddir)/src/api

noinst_SCRIPTS = pmix_client_otheruser.sh \
	run_tests00.pl \
	run_tests01.pl \
	run_tests02.pl \
	run_tests03.pl \
	run_tests04.pl \
	run_tests05.pl \
	run_tests06.pl \
	run_tests07.pl \
	run_tests08.pl \
	run_tests09.pl \
	run_tests10.pl \
	run_tests11.pl \
	run_tests12.pl \
	run_tests13.pl \
	run_tests14.pl \
	run_tests15.pl

noinst_PROGRAMS =

#########################
# Support for "make check"

check_PROGRAMS = \
    pmix_test \
    pmix_client \
    pmix_regex

if WANT_PMI_BACKWARD
check_PROGRAMS += \
    pmi_client \
    pmi2_client
endif

TESTS = \
	run_tests00.pl \
	run_tests01.pl \
	run_tests02.pl \
	run_tests03.pl \
	run_tests04.pl \
	run_tests05.pl \
	run_tests06.pl \
	run_tests07.pl \
	run_tests08.pl \
	run_tests09.pl \
	run_tests10.pl \
	run_tests11.pl \
	run_tests12.pl \
	run_tests13.pl \
	run_tests14.pl \
	run_tests15.pl


##########################

if WANT_PMI_BACKWARD
noinst_PROGRAMS += pmi_client pmi2_client
endif

noinst_PROGRAMS += pmix_test pmix_client pmix_regex

pmix_test_SOURCES = $(headers) \
        pmix_test.c test_common.c cli_stages.c server_callbacks.c test_server.c utils.c
pmix_test_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
pmix_test_LDADD = \
    $(top_builddir)/src/libpmix.la

if WANT_PMI_BACKWARD
pmi_client_SOURCES = $(headers) \
        pmi_client.c
pmi_client_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
pmi_client_LDADD = \
    $(top_builddir)/src/libpmi.la

pmi2_client_SOURCES = $(headers) \
        pmi2_client.c
pmi2_client_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
pmi2_client_LDADD = \
    $(top_builddir)/src/libpmi2.la
endif

pmix_client_SOURCES = $(headers) \
        pmix_client.c test_fence.c test_common.c test_publish.c test_spawn.c \
        test_cd.c test_resolve_peers.c test_error.c test_replace.c test_internal.c
pmix_client_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
pmix_client_LDADD = \
    $(top_builddir)/src/libpmix.la

pmix_regex_SOURCES = $(headers) \
        pmix_regex.c test_common.c cli_stages.c server_callbacks.c test_server.c utils.c
pmix_regex_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
pmix_regex_LDADD = \
    $(top_builddir)/src/libpmix.la

EXTRA_DIST = $(noinst_SCRIPTS)
