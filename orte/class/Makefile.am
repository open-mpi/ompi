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
        ompi_proc_table.h \
        orte_bitmap.h \
        orte_pointer_array.h \
        orte_value_array.h

libclass_la_SOURCES = \
	$(headers) \
        ompi_proc_table.c \
        orte_bitmap.c \
        orte_pointer_array.c \
        orte_value_array.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ortedir = $(includedir)/openmpi/orte/class
orte_HEADERS = $(headers)
else
ortedir = $(includedir)
endif
