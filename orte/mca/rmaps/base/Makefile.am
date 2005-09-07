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



noinst_LTLIBRARIES = libmca_rmaps_base.la

# Source code files

headers = \
        base.h

# Library

libmca_rmaps_base_la_SOURCES = \
        $(headers) \
        rmaps_base_close.c \
	rmaps_base_map.h \
        rmaps_base_map.c \
        rmaps_base_open.c \
        rmaps_base_select.c


# Conditionally install the header files

if WANT_INSTALL_HEADERS
ortedir = $(includedir)/openmpi/orte/mca/rmaps/base
orte_HEADERS = $(headers)
else
ortedir = $(includedir)
endif

distclean-local:
	rm -f static-components.h
