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

noinst_LTLIBRARIES = liblfc.la

# Source code files

headers = \
	ompi_bitmap.h \
	ompi_free_list.h \
	ompi_hash_table.h \
	ompi_list.h \
	ompi_object.h \
	ompi_pointer_array.h \
	ompi_proc_table.h \
	ompi_value_array.h \
	ompi_rb_tree.h

liblfc_la_SOURCES = \
	$(headers) \
	ompi_free_list.c \
	ompi_hash_table.c \
	ompi_list.c \
	ompi_object.c \
	ompi_pointer_array.c \
	ompi_proc_table.c \
	ompi_bitmap.c \
	ompi_value_array.c \
	ompi_rb_tree.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/class
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
