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
AM_CPPFLAGS = -D__USE_GNU -I$(top_srcdir)/test/support -DOMPI_ENABLE_DEBUG_OVERRIDE=1

noinst_PROGRAMS = \
    test_gpr_proxy \
	test_gpr_replica 

test_gpr_replica_SOURCES = test_gpr_replica.c
test_gpr_replica_LDADD = \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.la 
test_gpr_replica_DEPENDENCIES = $(test_gpr_replica_LDADD)

test_gpr_proxy_SOURCES = test_gpr_proxy.c
test_gpr_proxy_LDADD = \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.la 
test_gpr_proxy_DEPENDENCIES = $(test_gpr_proxy_LDADD)

