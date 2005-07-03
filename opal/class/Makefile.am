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
        opal_free_list.h \
        opal_hash_table.h \
        opal_list.h \
        opal_object.h \
        opal_value_array.h

libclass_la_SOURCES = \
	$(headers) \
        opal_free_list.c \
        opal_hash_table.c \
        opal_list.c \
        opal_object.c \
        opal_value_array.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/opal/class
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
