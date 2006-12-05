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



TESTS_ENVIRONMENT = TEST_WRITE_TO_FILE=1

check_PROGRAMS = \
    gpr_dt_buffer \
    gpr_dt_copy \
    gpr_dt_cmp \
    gpr_dt_release \
    gpr_dt_size \
    gpr_dt_print \
    gpr_internal_fns \
    gpr_mem_leaks \
    gpr_overwrite \
    gpr_put_get \
    gpr_quick_put \
    gpr_quick_triggers \
    gpr_sub_test \
    gpr_threads \
    gpr_triggers

TESTS =

gpr_dt_buffer_SOURCES = gpr_dt_buffer.c
gpr_dt_buffer_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
gpr_dt_buffer_DEPENDENCIES = $(gpr_dt_buffer_LDADD)

gpr_dt_copy_SOURCES = gpr_dt_copy.c
gpr_dt_copy_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
gpr_dt_copy_DEPENDENCIES = $(gpr_dt_copy_LDADD)

gpr_dt_cmp_SOURCES = gpr_dt_cmp.c
gpr_dt_cmp_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
gpr_dt_cmp_DEPENDENCIES = $(gpr_dt_cmp_LDADD)

gpr_dt_release_SOURCES = gpr_dt_release.c
gpr_dt_release_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
gpr_dt_release_DEPENDENCIES = $(gpr_dt_release_LDADD)

gpr_dt_size_SOURCES = gpr_dt_size.c
gpr_dt_size_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
gpr_dt_size_DEPENDENCIES = $(gpr_dt_size_LDADD)

gpr_dt_print_SOURCES = gpr_dt_print.c
gpr_dt_print_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
gpr_dt_print_DEPENDENCIES = $(gpr_dt_print_LDADD)

gpr_internal_fns_SOURCES = gpr_internal_fns.c
gpr_internal_fns_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
gpr_internal_fns_DEPENDENCIES = $(gpr_internal_fns_LDADD)

gpr_mem_leaks_SOURCES = gpr_mem_leaks.c
gpr_mem_leaks_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
gpr_mem_leaks_DEPENDENCIES = $(gpr_mem_leaks_LDADD)

gpr_overwrite_SOURCES = gpr_overwrite.c
gpr_overwrite_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
gpr_overwrite_DEPENDENCIES = $(gpr_overwrite_LDADD)

gpr_put_get_SOURCES = gpr_put_get.c
gpr_put_get_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
gpr_put_get_DEPENDENCIES = $(gpr_put_get_LDADD)

gpr_threads_SOURCES = gpr_threads.c
gpr_threads_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
gpr_threads_DEPENDENCIES = $(gpr_threads_LDADD)

gpr_triggers_SOURCES = gpr_triggers.c
gpr_triggers_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
gpr_triggers_DEPENDENCIES = $(gpr_triggers_LDADD)

gpr_quick_put_SOURCES = gpr_quick_put.c
gpr_quick_put_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
gpr_quick_put_DEPENDENCIES = $(gpr_quick_put_LDADD)

gpr_quick_triggers= gpr_quick_triggers.c
gpr_quick_triggers_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
gpr_quick_triggers_DEPENDENCIES = $(gpr_quick_triggers_LDADD)

gpr_sub_test= gpr_sub_test.c
gpr_sub_test_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
gpr_sub_test_DEPENDENCIES = $(gpr_sub_test_LDADD)
