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
        rmgr_dt_buffer \
        rmgr_dt_compare \
        rmgr_dt_print \
        rmgr_dt_size \
        rmgr_dt_release \
        rmgr_dt_copy

TESTS = \
	$(check_PROGRAMS)

rmgr_dt_buffer_SOURCES = rmgr_dt_buffer.c
rmgr_dt_buffer_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
rmgr_dt_buffer_DEPENDENCIES = $(rmgr_dt_buffer_LDADD)

rmgr_dt_copy_SOURCES = rmgr_dt_copy.c
rmgr_dt_copy_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
rmgr_dt_copy_DEPENDENCIES = $(rmgr_dt_copy_LDADD)

rmgr_dt_compare_SOURCES = rmgr_dt_compare.c
rmgr_dt_compare_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
rmgr_dt_compare_DEPENDENCIES = $(rmgr_dt_compare_LDADD)

rmgr_dt_print_SOURCES = rmgr_dt_print.c
rmgr_dt_print_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
rmgr_dt_print_DEPENDENCIES = $(rmgr_dt_print_LDADD)

rmgr_dt_size_SOURCES = rmgr_dt_size.c
rmgr_dt_size_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
rmgr_dt_size_DEPENDENCIES = $(rmgr_dt_size_LDADD)

rmgr_dt_release_SOURCES = rmgr_dt_release.c
rmgr_dt_release_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
rmgr_dt_release_DEPENDENCIES = $(rmgr_dt_release_LDADD)
