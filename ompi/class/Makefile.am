# -*- makefile -*-
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

noinst_LTLIBRARIES = libclass.la

# Source code files

headers = \
        ompi_circular_buffer_fifo.h \
        ompi_fifo.h \
        ompi_free_list.h \
        ompi_bitmap.h \
        ompi_pointer_array.h

libclass_la_SOURCES = \
	$(headers) \
        ompi_bitmap.c \
        ompi_free_list.c \
        ompi_pointer_array.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/ompi/class
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif

