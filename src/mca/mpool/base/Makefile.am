#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmca_mpool_base.la

# For VPATH builds, have to specify where static-modules.h will be found

AM_CPPFLAGS = -I$(top_builddir)/src

# Source code files

headers = \
        base.h 

libmca_mpool_base_la_SOURCES = \
        $(headers) \
        mpool_base_open.c \
        mpool_base_close.c \
        mpool_base_init.c \
        mpool_base_lookup.c \
        mpool_base_alloc.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca/mpool/base
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif

