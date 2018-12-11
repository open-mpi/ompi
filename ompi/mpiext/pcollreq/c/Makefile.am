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

SUBDIRS = profile

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

if BUILD_MPI_BINDINGS_LAYER
libmpiext_pcollreq_c_la_SOURCES += \
        allgather_init.c \
        allgatherv_init.c \
        allreduce_init.c \
        alltoall_init.c \
        alltoallv_init.c \
        alltoallw_init.c \
        barrier_init.c \
        bcast_init.c \
        exscan_init.c \
        gather_init.c \
        gatherv_init.c \
        reduce_init.c \
        reduce_scatter_block_init.c \
        reduce_scatter_init.c \
        scan_init.c \
        scatter_init.c \
        scatterv_init.c \
        \
        neighbor_allgather_init.c \
        neighbor_allgatherv_init.c \
        neighbor_alltoall_init.c \
        neighbor_alltoallv_init.c \
        neighbor_alltoallw_init.c
endif

libmpiext_pcollreq_c_la_LIBADD = \
        profile/libpmpiext_pcollreq_c.la

# Man page installation
nodist_man_MANS = \
        MPIX_Allgather_init.3 \
        MPIX_Allgatherv_init.3 \
        MPIX_Allreduce_init.3 \
        MPIX_Alltoall_init.3 \
        MPIX_Alltoallv_init.3 \
        MPIX_Alltoallw_init.3 \
        MPIX_Barrier_init.3 \
        MPIX_Bcast_init.3 \
        MPIX_Exscan_init.3 \
        MPIX_Gather_init.3 \
        MPIX_Gatherv_init.3 \
        MPIX_Reduce_init.3 \
        MPIX_Reduce_scatter_block_init.3 \
        MPIX_Reduce_scatter_init.3 \
        MPIX_Scan_init.3 \
        MPIX_Scatter_init.3 \
        MPIX_Scatterv_init.3 \
        MPIX_Neighbor_allgather_init.3 \
        MPIX_Neighbor_allgatherv_init.3 \
        MPIX_Neighbor_alltoall_init.3 \
        MPIX_Neighbor_alltoallv_init.3 \
        MPIX_Neighbor_alltoallw_init.3

# Man page sources
EXTRA_DIST = $(nodist_man_MANS:.3=.3in)

distclean-local:
	rm -f $(nodist_man_MANS)
