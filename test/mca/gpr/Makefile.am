# -*- makefile -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options
AM_CPPFLAGS = -I$(top_srcdir)/test/support -DOMPI_ENABLE_DEBUG_OVERRIDE=1 -g

noinst_PROGRAMS = \
    gpr_test \
    gpr_test_trigs \
    gpr_test_proxy

gpr_test_SOURCES = gpr_test.c
gpr_test_LDADD = \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.la 
gpr_test_DEPENDENCIES = $(gpr_test_LDADD)

gpr_test_trigs_SOURCES = gpr_test_trigs.c
gpr_test_trigs_LDADD = \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.la 
gpr_test_trigs_DEPENDENCIES = $(gpr_test_trigs_LDADD)

gpr_test_proxy_SOURCES = gpr_test_proxy.c
gpr_test_proxy_LDADD = \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.la 
gpr_test_proxy_DEPENDENCIES = $(gpr_test_proxy_LDADD)
