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


AM_CPPFLAGS = -I$(top_srcdir)/opal/libltdl -I$(top_srcdir)/test/support

check_PROGRAMS = \
	test_schema

TESTS = \
	$(check_PROGRAMS)

test_schema_SOURCES = test_schema.c
test_schema_LDADD = \
        $(top_builddir)/test/support/libsupport.a \
        $(top_builddir)/orte/libopen-rte.la \
        $(top_builddir)/opal/libopen-pal.la 
test_schema_DEPENDENCIES = $(test_schema_LDADD)
