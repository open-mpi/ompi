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
        smr_dt_buffer \
        smr_dt_compare \
        smr_dt_print \
        smr_dt_size \
        smr_dt_release \
        smr_dt_copy

TESTS = \
	$(check_PROGRAMS)

smr_dt_buffer_SOURCES = smr_dt_buffer.c
smr_dt_buffer_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
smr_dt_buffer_DEPENDENCIES = $(smr_dt_buffer_LDADD)

smr_dt_copy_SOURCES = smr_dt_copy.c
smr_dt_copy_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
smr_dt_copy_DEPENDENCIES = $(smr_dt_copy_LDADD)

smr_dt_compare_SOURCES = smr_dt_compare.c
smr_dt_compare_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
smr_dt_compare_DEPENDENCIES = $(smr_dt_compare_LDADD)

smr_dt_print_SOURCES = smr_dt_print.c
smr_dt_print_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
smr_dt_print_DEPENDENCIES = $(smr_dt_print_LDADD)

smr_dt_size_SOURCES = smr_dt_size.c
smr_dt_size_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
smr_dt_size_DEPENDENCIES = $(smr_dt_size_LDADD)

smr_dt_release_SOURCES = smr_dt_release.c
smr_dt_release_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
smr_dt_release_DEPENDENCIES = $(smr_dt_release_LDADD)
