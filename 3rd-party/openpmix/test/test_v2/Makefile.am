## Process this file with automake to produce Makefile.in
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
# Copyright (c) 2018      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2021      Triad National Security, LLC
#                         All rights reserved.
# Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

headers = cli_stages.h server_callbacks.h test_common.h test_server.h

AM_CPPFLAGS = -I$(top_builddir)/src -I$(top_builddir)/src/include -I$(top_builddir)/src/api

# Can be added in for purposes of debugging on platforms building with gcc (gcc-specific flags)
#AM_CFLAGS = -fno-omit-frame-pointer -g -Wfatal-errors #-Wall -Wextra -Wpedantic -Wconversion -Wshadow

# No scripts at present, but if they are added, use this as a model
#noinst_SCRIPTS = pmix_client_otheruser.sh \
#	run_tests00.pl \
#	run_tests15.pl

#########################
# Support for "make check" - could add each individual test here for a different approach

check_PROGRAMS = # test_init_fin (etc)

##########################

noinst_PROGRAMS = pmix_test test_init_fin test_helloworld \
				   test_get_basic test_get_peers \
				   test_fence_basic test_fence_wildcard test_fence_partial

PCFILES = pmix_test.c test_common.c cli_stages.c test_server.c \
		  server_callbacks.c base64_enc_dec.c
TCCOMMONFILES = test_common.c test_clientapp_funcs.c base64_enc_dec.c
TC1FILES = test_init_fin.c $(TCCOMMONFILES)
TC2FILES = test_helloworld.c $(TCCOMMONFILES)
TC3FILES = test_get_basic.c $(TCCOMMONFILES)
TC4FILES = test_get_peers.c $(TCCOMMONFILES)
TC5FILES = test_fence_basic.c $(TCCOMMONFILES)
TC6FILES = test_fence_wildcard.c $(TCCOMMONFILES)
TC7FILES = test_fence_partial.c $(TCCOMMONFILES)

# Note the use of -no-install in LDFLAGS to force rpath
# into binaries and prevent libtool from creating the usual scripts.
# This simplifies testing and debugging.
if OAC_HAVE_APPLE
INSTALLFLAG=-no-fast-install
else
INSTALLFLAG=-no-install
endif

pmix_test_SOURCES = $(headers) $(PCFILES)
pmix_test_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS) $(INSTALLFLAG)
pmix_test_LDADD = $(top_builddir)/src/libpmix.la

test_init_fin_SOURCES = $(headers) $(TC1FILES)
test_init_fin_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS) $(INSTALLFLAG)
test_init_fin_LDADD = $(top_builddir)/src/libpmix.la

test_helloworld_SOURCES = $(headers) $(TC2FILES)
test_helloworld_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS) $(INSTALLFLAG)
test_helloworld_LDADD = $(top_builddir)/src/libpmix.la

test_get_basic_SOURCES = $(headers) $(TC3FILES)
test_get_basic_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS) $(INSTALLFLAG)
test_get_basic_LDADD = $(top_builddir)/src/libpmix.la

test_get_peers_SOURCES = $(headers) $(TC4FILES)
test_get_peers_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS) $(INSTALLFLAG)
test_get_peers_LDADD = $(top_builddir)/src/libpmix.la

test_fence_basic_SOURCES = $(headers) $(TC5FILES)
test_fence_basic_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS) $(INSTALLFLAG)
test_fence_basic_LDADD = $(top_builddir)/src/libpmix.la

test_fence_wildcard_SOURCES = $(headers) $(TC6FILES)
test_fence_wildcard_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS) $(INSTALLFLAG)
test_fence_wildcard_LDADD = $(top_builddir)/src/libpmix.la

test_fence_partial_SOURCES = $(headers) $(TC7FILES)
test_fence_partial_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS) $(INSTALLFLAG)
test_fence_partial_LDADD = $(top_builddir)/src/libpmix.la
