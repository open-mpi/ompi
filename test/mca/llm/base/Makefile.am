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
AM_CPPFLAGS = -I$(top_srcdir)/test/support -DOMPI_ENABLE_DEBUG_OVERRIDE=1

noinst_PROGRAMS = \
	parse_hostfile

parse_hostfile_SOURCES = \
	parse_hostfile.c
parse_hostfile_LDADD = \
	$(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.la 
parse_hostfile_DEPENDENCIES = $(parse_hostfile_LDADD)
