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

noinst_LTLIBRARIES = libmca_ptl_base.la

# For VPATH builds, have to specify where static-modules.h will be found

AM_CPPFLAGS = -I$(top_builddir)/src

# Source code files

headers = \
        base.h \
	ptl_base_comm.h \
	ptl_base_fragment.h \
	ptl_base_header.h \
	ptl_base_match.h \
	ptl_base_recvfrag.h \
	ptl_base_sendfrag.h

libmca_ptl_base_la_SOURCES = \
        $(headers) \
        ptl_base_close.c \
	ptl_base_comm.c \
        ptl_base_fragment.c \
	ptl_base_match.c \
        ptl_base_open.c \
        ptl_base_recvfrag.c \
        ptl_base_select.c \
        ptl_base_sendfrag.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca/ptl/base
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif

