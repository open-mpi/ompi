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

noinst_LTLIBRARIES = libmca_iof_base.la

# For VPATH builds, have to specify where static-modules.h will be found

AM_CPPFLAGS = -I$(top_builddir)/src

# Source code files

headers = \
        base.h \
        iof_base_header.h \
        iof_base_endpoint.h \
        iof_base_fragment.h

libmca_iof_base_la_SOURCES = \
        $(headers) \
        iof_base_close.c \
        iof_base_open.c \
        iof_base_flush.c \
        iof_base_endpoint.c \
        iof_base_fragment.c \
 	    iof_base_select.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca/iof/base
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif

