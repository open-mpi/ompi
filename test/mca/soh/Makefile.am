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
        soh_dt_buffer \
        soh_dt_compare \
        soh_dt_print \
        soh_dt_size \
        soh_dt_release \
        soh_dt_copy

TESTS = \
	$(check_PROGRAMS)

soh_dt_buffer_SOURCES = soh_dt_buffer.c
soh_dt_buffer_LDADD = \
        $(top_builddir)/orte/liborte.la \
        $(top_builddir)/opal/libopal.la
soh_dt_buffer_DEPENDENCIES = $(soh_dt_buffer_LDADD)

soh_dt_copy_SOURCES = soh_dt_copy.c
soh_dt_copy_LDADD = \
        $(top_builddir)/orte/liborte.la \
        $(top_builddir)/opal/libopal.la
soh_dt_copy_DEPENDENCIES = $(soh_dt_copy_LDADD)

soh_dt_compare_SOURCES = soh_dt_compare.c
soh_dt_compare_LDADD = \
        $(top_builddir)/orte/liborte.la \
        $(top_builddir)/opal/libopal.la
soh_dt_compare_DEPENDENCIES = $(soh_dt_compare_LDADD)

soh_dt_print_SOURCES = soh_dt_print.c
soh_dt_print_LDADD = \
        $(top_builddir)/orte/liborte.la \
        $(top_builddir)/opal/libopal.la
soh_dt_print_DEPENDENCIES = $(soh_dt_print_LDADD)

soh_dt_size_SOURCES = soh_dt_size.c
soh_dt_size_LDADD = \
        $(top_builddir)/orte/liborte.la \
        $(top_builddir)/opal/libopal.la
soh_dt_size_DEPENDENCIES = $(soh_dt_size_LDADD)

soh_dt_release_SOURCES = soh_dt_release.c
soh_dt_release_LDADD = \
        $(top_builddir)/orte/liborte.la \
        $(top_builddir)/opal/libopal.la
soh_dt_release_DEPENDENCIES = $(soh_dt_release_LDADD)
