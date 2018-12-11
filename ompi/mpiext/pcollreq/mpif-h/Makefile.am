#
# Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
# Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
# Copyright (c) 2018      Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

SUBDIRS = profile

# This file builds the mpif.h-based bindings for MPI extensions.  It
# is optional in MPI extensions.

# We must set these #defines so that the inner OMPI MPI prototype
# header files do the Right Thing.
AM_CPPFLAGS = -DOMPI_BUILD_MPI_PROFILING=0 -DOMPI_COMPILING_FORTRAN_WRAPPERS=1

# Note that the mpif.h-based bindings are optional -- they can only be
# built if OMPI is also building the Fortran-based bindings.  So we
# initially set some Makefile macros to empty, and then conditionally
# add to them later.
noinst_LTLIBRARIES =

# Directory where the header file to be included in mpif-ext.h must be
# installed.
ompidir = $(ompiincludedir)/mpiext

# Just like noinst_LTLIBRARIES, set this macro to empty and
# conditionally add to it later.
ompi_HEADERS =

# Use the Automake conditional to know if we're building the mpif.h
# bindings.
if OMPI_BUILD_FORTRAN_MPIFH_BINDINGS

# If we are, build the convenience libtool library that will be
# slurped up into libmpi_mpifh.la.
noinst_LTLIBRARIES += libmpiext_pcollreq_mpifh.la

# This is the header file that is installed.
ompi_HEADERS += mpiext_pcollreq_mpifh.h

noinst_HEADERS = mpiext_pcollreq_prototypes.h

# Sources for the convenience libtool library.
libmpiext_pcollreq_mpifh_la_SOURCES = \
        $(ompi_HEADERS) \
        mpiext_pcollreq_mpifh.c

if BUILD_MPI_BINDINGS_LAYER
libmpiext_pcollreq_mpifh_la_SOURCES += \
        allgather_init_f.c \
        allgatherv_init_f.c \
        allreduce_init_f.c \
        alltoall_init_f.c \
        alltoallv_init_f.c \
        alltoallw_init_f.c \
        barrier_init_f.c \
        bcast_init_f.c \
        exscan_init_f.c \
        gather_init_f.c \
        gatherv_init_f.c \
        reduce_init_f.c \
        reduce_scatter_init_f.c \
        reduce_scatter_block_init_f.c \
        scan_init_f.c \
        scatter_init_f.c \
        scatterv_init_f.c \
        \
        neighbor_allgather_init_f.c \
        neighbor_allgatherv_init_f.c \
        neighbor_alltoall_init_f.c \
        neighbor_alltoallv_init_f.c \
        neighbor_alltoallw_init_f.c
endif

libmpiext_pcollreq_mpifh_la_LIBADD = \
        profile/libpmpiext_pcollreq_mpifh.la

endif
