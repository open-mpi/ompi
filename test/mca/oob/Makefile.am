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
AM_CPPFLAGS = -g -I$(top_srcdir)/test/support -DOMPI_ENABLE_DEBUG_OVERRIDE=1

check_PROGRAMS = \
    oob_test \
    oob_test_self \
    oob_test_packed

TESTS = \
	$(check_PROGRAMS)

oob_test_SOURCES = oob_test.c
oob_test_LDADD = \
        $(top_builddir)/test/support/libsupport.a \
        $(top_builddir)/src/libmpi.la
oob_test_DEPENDENCIES = $(oob_test_LDADD)

oob_test_self_SOURCES = oob_test_self.c
oob_test_self_LDADD = \
        $(top_builddir)/test/support/libsupport.a \
        $(top_builddir)/src/libmpi.la
oob_test_self_DEPENDENCIES = $(oob_test_LDADD)

oob_test_packed_SOURCES = oob_test_packed.c
oob_test_packed_LDADD = \
        $(top_builddir)/test/support/libsupport.a \
        $(top_builddir)/src/libmpi.la
oob_test_packed_DEPENDENCIES = $(oob_test_LDADD)

