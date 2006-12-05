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
    gpr_put_get \
    gpr_quick_put \
    gpr_quick_triggers \
    gpr_sub_test

TESTS =

gpr_put_get_SOURCES = gpr_put_get.c
gpr_put_get_LDADD = \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la
gpr_put_get_DEPENDENCIES = $(gpr_put_get_LDADD)

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
