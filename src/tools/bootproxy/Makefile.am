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

libs = $(top_builddir)/src/libmpi.la 

EXTRA_DIST = $(pkgdata_DATA)

pkgdata_DATA = help-bootproxy.txt

bin_PROGRAMS = mca_pcm_rsh_bootproxy

mca_pcm_rsh_bootproxy_SOURCES = bootproxy.c
mca_pcm_rsh_bootproxy_LDADD = $(libs) $(LIBMPI_EXTRA_LIBS) -lm
mca_pcm_rsh_bootproxy_LDFLAGS = $(LIBMPI_EXTRA_LDFLAGS)
mca_pcm_rsh_bootproxy_DEPENDENCIES = $(libs)

