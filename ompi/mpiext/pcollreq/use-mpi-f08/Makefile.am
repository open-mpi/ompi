#
# Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2017-2018 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# This file builds the use_mpi_f08-based bindings for MPI extensions.  It
# is optional in MPI extensions.

# We must set these #defines and include paths so that the inner OMPI
# MPI prototype header files do the Right Thing.
AM_FCFLAGS = $(OMPI_FC_MODULE_FLAG)$(top_builddir)/ompi/mpi/fortran/use-mpi-f08/mod \
             -I$(top_srcdir) $(FCFLAGS_f90)

# Note that the mpi_f08-based bindings are optional -- they can only
# be built if OMPI is also building the Fortran-based bindings.  So we
# initially set some Makefile macros to empty, and then conditionally
# add to them later.
noinst_LTLIBRARIES =

# Use the Automake conditional to know if we're building the mpif.h
# bindings.
if OMPI_BUILD_FORTRAN_USEMPIF08_BINDINGS

# If we are, build the convenience libtool library that will be
# slurped up into libmpi_usempif08.la.
noinst_LTLIBRARIES += libmpiext_pcollreq_usempif08.la

# Note that no header files are installed; instead,
# mpiext_pcollreq_usempif08.h is automatically slurped up into the
# mpi_f08_ext module.  It must be listed so that it is included in
# dist tarballs.
noinst_HEADERS = mpiext_pcollreq_usempif08.h

mpi_api_files = \
        allgather_init_f08.F90 \
        allgatherv_init_f08.F90 \
        allreduce_init_f08.F90 \
        alltoall_init_f08.F90 \
        alltoallv_init_f08.F90 \
        alltoallw_init_f08.F90 \
        barrier_init_f08.F90 \
        bcast_init_f08.F90 \
        exscan_init_f08.F90 \
        gather_init_f08.F90 \
        gatherv_init_f08.F90 \
        reduce_init_f08.F90 \
        reduce_scatter_block_init_f08.F90 \
        reduce_scatter_init_f08.F90 \
        scan_init_f08.F90 \
        scatter_init_f08.F90 \
        scatterv_init_f08.F90 \
        \
        neighbor_allgather_init_f08.F90 \
        neighbor_allgatherv_init_f08.F90 \
        neighbor_alltoall_init_f08.F90 \
        neighbor_alltoallv_init_f08.F90 \
        neighbor_alltoallw_init_f08.F90

pmpi_api_files = \
        profile/pallgather_init_f08.F90 \
        profile/pallgatherv_init_f08.F90 \
        profile/pallreduce_init_f08.F90 \
        profile/palltoall_init_f08.F90 \
        profile/palltoallv_init_f08.F90 \
        profile/palltoallw_init_f08.F90 \
        profile/pbarrier_init_f08.F90 \
        profile/pbcast_init_f08.F90 \
        profile/pexscan_init_f08.F90 \
        profile/pgather_init_f08.F90 \
        profile/pgatherv_init_f08.F90 \
        profile/preduce_init_f08.F90 \
        profile/preduce_scatter_block_init_f08.F90 \
        profile/preduce_scatter_init_f08.F90 \
        profile/pscan_init_f08.F90 \
        profile/pscatter_init_f08.F90 \
        profile/pscatterv_init_f08.F90 \
        \
        profile/pneighbor_allgather_init_f08.F90 \
        profile/pneighbor_allgatherv_init_f08.F90 \
        profile/pneighbor_alltoall_init_f08.F90 \
        profile/pneighbor_alltoallv_init_f08.F90 \
        profile/pneighbor_alltoallw_init_f08.F90

mpi_api_lo_files = $(mpi_api_files:.F90=.lo)
pmpi_api_lo_files = $(pmpi_api_files:.F90=.lo)

$(mpi_api_lo_files): mpiext_pcollreq_f08.lo
$(pmpi_api_lo_files): mpiext_pcollreq_f08.lo

# Sources for the convenience libtool library.
libmpiext_pcollreq_usempif08_la_SOURCES = \
        mpiext_pcollreq_f08.F90 \
        $(mpi_api_files) \
        $(pmpi_api_files)

# Remove the intermediate module file
distclean-local:
	rm -f mpiext_pcollreq_f08.mod

endif
