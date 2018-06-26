#
# Copyright (c) 2017      FUJITSU LIMITED.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# We must set these #defines so that the inner OMPI MPI prototype
# header files do the Right Thing.
AM_CPPFLAGS = -DOMPI_PROFILE_LAYER=0 -DOMPI_COMPILING_FORTRAN_WRAPPERS=1

include $(top_srcdir)/Makefile.ompi-rules

# Convenience libtool library that will be slurped up into libmpi.la.
noinst_LTLIBRARIES = libmpiext_pcollreq_c.la

# This is where the top-level header file (that is included in
# <mpi-ext.h>) must be installed.
ompidir = $(ompiincludedir)/ompi/mpiext/pcollreq/c

# This is the header file that is installed.
ompi_HEADERS = mpiext_pcollreq_c.h

# Sources for the convenience libtool library.  Other than the one
# header file, all source files in the extension have no file naming
# conventions.
libmpiext_pcollreq_c_la_SOURCES = \
        $(ompi_HEADERS) \
        mpiext_pcollreq.c
libmpiext_pcollreq_c_la_LDFLAGS = -module -avoid-version

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
