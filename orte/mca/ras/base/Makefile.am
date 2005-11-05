#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
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

noinst_LTLIBRARIES = libmca_ras_base.la

# Source code files

headers = \
        base.h

# Library

libmca_ras_base_la_SOURCES = \
        $(headers) \
        ras_base_alloc.c \
        ras_base_allocate.c \
        ras_base_close.c \
        ras_base_find_available.c \
	ras_base_node.h \
        ras_base_node.c \
        ras_base_open.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ortedir = $(includedir)/openmpi/orte/mca/ras/base
orte_HEADERS = $(headers)
else
ortedir = $(includedir)
endif

distclean-local:
	rm -f static-components.h
