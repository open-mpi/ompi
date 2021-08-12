#
# Copyright (c) 2017      FUJITSU LIMITED.  All rights reserved.
# Copyright (c) 2018      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# OMPI_BUILD_MPI_PROFILING is enabled when we want our generated MPI_* symbols
# to be replaced by PMPI_*.
# In this directory, we need it to be 0

AM_CPPFLAGS     = -DOMPI_BUILD_MPI_PROFILING=0

include $(top_srcdir)/Makefile.ompi-rules

noinst_LTLIBRARIES = libmpiext_pcollreq_c.la

# This is where the top-level header file (that is included in
# <mpi-ext.h>) must be installed.
ompidir = $(ompiincludedir)/mpiext

# This is the header file that is installed.
ompi_HEADERS = mpiext_pcollreq_c.h

# Sources for the convenience libtool library.  Other than the one
# header file, all source files in the extension have no file naming
# conventions.
libmpiext_pcollreq_c_la_SOURCES = \
        $(ompi_HEADERS) \
        mpiext_pcollreq_c.c
