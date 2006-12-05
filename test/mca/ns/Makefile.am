# -*- makefile -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


AM_CPPFLAGS = -I$(top_srcdir)/test/support

check_PROGRAMS = \
	ns_replica \
	ns_proxy \
	ns_nds \
        ns_dt_buffer \
        ns_dt_compare \
        ns_dt_print \
        ns_dt_size \
        ns_dt_release \
        ns_dt_copy

TESTS = \
	$(check_PROGRAMS)

ns_replica_SOURCES = ns_replica.c
ns_replica_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
ns_replica_DEPENDENCIES = $(ns_replica_LDADD)

ns_proxy_SOURCES = ns_proxy.c
ns_proxy_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
ns_proxy_DEPENDENCIES = $(ns_proxy_LDADD)

ns_nds_SOURCES = ns_nds.c
ns_nds_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
ns_nds_DEPENDENCIES = $(ns_nds_LDADD)

ns_dt_buffer_SOURCES = ns_dt_buffer.c
ns_dt_buffer_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
ns_dt_buffer_DEPENDENCIES = $(ns_dt_buffer_LDADD)

ns_dt_copy_SOURCES = ns_dt_copy.c
ns_dt_copy_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
ns_dt_copy_DEPENDENCIES = $(ns_dt_copy_LDADD)

ns_dt_compare_SOURCES = ns_dt_compare.c
ns_dt_compare_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
ns_dt_compare_DEPENDENCIES = $(ns_dt_compare_LDADD)

ns_dt_print_SOURCES = ns_dt_print.c
ns_dt_print_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
ns_dt_print_DEPENDENCIES = $(ns_dt_print_LDADD)

ns_dt_size_SOURCES = ns_dt_size.c
ns_dt_size_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
ns_dt_size_DEPENDENCIES = $(ns_dt_size_LDADD)

ns_dt_release_SOURCES = ns_dt_release.c
ns_dt_release_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
ns_dt_release_DEPENDENCIES = $(ns_dt_release_LDADD)
