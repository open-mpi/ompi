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
AM_LDFLAGS = -lpthread

noinst_PROGRAMS = chello

chello_SOURCES = chello.c
chello_LDADD = \
	$(top_builddir)/src/libmpi.la \
	$(top_builddir)/src/libompi.la 
chello_DEPENDENCIES = $(chello_LDADD)

