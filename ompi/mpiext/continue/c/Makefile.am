#
# Copyright (c) 2021      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# OMPI_BUILD_MPI_PROFILING is enabled when we want our generated MPI_* symbols
# to be replaced by PMPI_*.
# In this directory, we need it to be 0

AM_CPPFLAGS = -DOMPI_BUILD_MPI_PROFILING=0 -DOMPI_COMPILING_FORTRAN_WRAPPERS=0

include $(top_srcdir)/Makefile.ompi-rules

noinst_LTLIBRARIES = libmpiext_continue_c.la

# This is where the top-level header file (that is included in
# <mpi-ext.h>) must be installed.
ompidir = $(ompiincludedir)/mpiext

# This is the header file that is installed.
nodist_ompi_HEADERS = mpiext_continue_c.h

libmpiext_continue_c_la_SOURCES = \
    continuation.c \
    continue.c \
    continueall.c \
    continue_init.c \
    mpiext_continue_module.c

#libmpiext_continue_c_la_LDFLAGS = -module -avoid-version

dist_ompidata_DATA = help-mpi-continue.txt

ompi_HEADERS = $(headers)

MAINTAINERCLEANFILES = $(nodist_libmpiext_continue_c_la_SOURCES)

