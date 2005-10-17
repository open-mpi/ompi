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



noinst_LTLIBRARIES = libmca_paffinity_base.la

headers = \
        base.h \
        internal.h

libmca_paffinity_base_la_SOURCES = \
        $(headers) \
        paffinity_base_close.c \
        paffinity_base_select.c \
        paffinity_base_open.c \
        paffinity_base_wrappers.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/opal/mca/paffinity/base
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif

distclean-local:
	rm -f static-components.h
