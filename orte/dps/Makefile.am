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

noinst_LTLIBRARIES = libdps.la

# Source code files

headers = \
        dps.h \
        dps_types.h \
        dps_internal.h

libdps_la_SOURCES = \
        $(headers) \
        dps_internal_functions.c \
        dps_load_unload.c \
        dps_lookup.c \
        dps_pack.c \
        dps_peek.c \
        dps_register.c \
        dps_unpack.c \
        orte_dps_open_close.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/orte/dps
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
