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

AM_CPPFLAGS = -I$(top_builddir)/src/event

EXTRA_DIST = $(pkgdata_DATA)

noinst_LTLIBRARIES = libmpiruntime.la

pkgdata_DATA = help-mpi-runtime.txt

# Source code files

headers = \
        mpiruntime.h \
        params.h

libmpiruntime_la_SOURCES = \
        $(headers) \
        ompi_mpi_abort.c \
        ompi_mpi_init.c \
        ompi_mpi_finalize.c \
        ompi_mpi_params.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/ompi/mpi/runtime
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
