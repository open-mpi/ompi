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
AM_CPPFLAGS = -I$(top_srcdir)/test/support -DOMPI_ENABLE_DEBUG_OVERRIDE=1

noinst_PROGRAMS = \
	test_rds_resfile

test_rds_resfile_SOURCES = test_rds_resfile.c
test_rds_resfile_LDADD = \
        $(top_builddir)/test/support/libsupport.la \
        $(top_builddir)/src/libmpi.la 
test_rds_resfile_DEPENDENCIES = $(test_rds_resfile_LDADD)
