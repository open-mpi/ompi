#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004 The Ohio State University.
#                    All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

AM_CPPFLAGS = -I$(top_ompi_builddir)/src/include \
    -I$(top_ompi_srcdir)/src -I$(top_ompi_srcdir)/src/include

noinst_LTLIBRARIES = libmca_pls_tm.la
libmca_pls_tm_la_SOURCES = \
        pls_tm.h \
        pls_tm_child.c \
        pls_tm_component.c \
        pls_tm_module.c \
        pls_tm_registry.c
