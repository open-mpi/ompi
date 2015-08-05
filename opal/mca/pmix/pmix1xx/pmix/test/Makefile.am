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

headers = test_common.h cli_stages.h server_callbacks.h utils.h test_fence.h test_publish.h test_spawn.h test_cd.h test_resolve_peers.h

AM_CPPFLAGS = -I$(top_builddir)/src -I$(top_builddir)/src/include -I$(top_builddir)/src/api

bin_PROGRAMS = pmix_test pmix_client pmix_test_lite pmi_client pmi2_client pmix_regex

check_PROGRAMS = pmix_test pmix_test_lite

TESTS = \
	$(check_PROGRAMS)

pmix_test_SOURCES = \
        pmix_test.c test_common.c cli_stages.c server_callbacks.c utils.c

pmix_test_LDADD = \
	$(top_builddir)/libpmix_server.la 

pmix_test_lite_SOURCES = \
        pmix_test_lite.c test_common.c cli_stages.c server_callbacks.c utils.c

pmix_test_lite_LDADD = \
	$(top_builddir)/libpmix_server.la 

pmi_client_SOURCES = \
        pmi_client.c 

pmi_client_LDADD = \
	$(top_builddir)/libpmix.la

pmi2_client_SOURCES = \
        pmi2_client.c

pmi2_client_LDADD = \
	$(top_builddir)/libpmix.la

pmix_client_SOURCES = \
        pmix_client.c test_fence.c test_common.c test_publish.c test_spawn.c test_cd.c test_resolve_peers.c

pmix_client_LDADD = \
	$(top_builddir)/libpmix.la

pmix_regex_SOURCES = \
        pmix_regex.c test_common.c cli_stages.c utils.c

pmix_regex_LDADD = \
	$(top_builddir)/libpmix_server.la $(top_builddir)/libpmix.la

