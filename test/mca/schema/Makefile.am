# -*- makefile -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
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

include $(top_srcdir)/config/Makefile.options
AM_CPPFLAGS = -I$(top_srcdir)/test/support

check_PROGRAMS = \
	test_schema

TESTS = \
	$(check_PROGRAMS)

test_schema_SOURCES = test_schema.c
test_schema_LDADD = \
        $(top_builddir)/test/support/libsupport.a \
        $(top_builddir)/src/libmpi.la 
test_schema_DEPENDENCIES = $(test_schema_LDADD)
