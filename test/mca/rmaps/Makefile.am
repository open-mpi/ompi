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
AM_CPPFLAGS = -I$(top_srcdir)/test/support -DOMPI_ENABLE_DEBUG_OVERRIDE=1

check_PROGRAMS = \
	test_rmaps

TESTS = \
	$(check_PROGRAMS)

test_rmaps_SOURCES = test_rmaps.c
test_rmaps_LDADD = \
        $(top_builddir)/test/support/libsupport.a \
        $(top_builddir)/src/libmpi.la 
test_rmaps_DEPENDENCIES = $(test_rmaps_LDADD)
