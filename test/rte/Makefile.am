# -*- makefile -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options
AM_CPPFLAGS = \
            -I$(top_srcdir)/test/support

bin_PROGRAMS = \
	sigchld


sigchld_SOURCES = \
	sigchld.c
sigchld_LDADD = \
	$(top_builddir)/src/libmpi.la
sigchld_LDFLAGS = $(LIBMPI_EXTRA_LDFLAGS)
sigchld_DEPENDENCIES = $(sigchld_LDADD)
