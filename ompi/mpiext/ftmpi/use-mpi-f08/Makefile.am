#
# Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2017-2018 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
# Copyright (c) 2018-2022 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2022      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# Note that Automake's Fortran-buidling rules uses CPPFLAGS and
# AM_CPPFLAGS.  This can cause weirdness (e.g.,
# https://github.com/open-mpi/ompi/issues/7253 and
# https://github.com/open-mpi/ompi/issues/9716).  Let's just zero
# those out and rely on AM_FCFLAGS.
CPPFLAGS =
AM_CPPFLAGS =

# This file builds the use_mpi_f08-based bindings for MPI extensions.  It
# is optional in MPI extensions.

# We must set these #defines and include paths so that the inner OMPI
# MPI prototype header files do the Right Thing.
AM_FCFLAGS = $(OMPI_FC_MODULE_FLAG)$(top_builddir)/ompi/mpi/fortran/use-mpi \
             $(OMPI_FC_MODULE_FLAG)$(top_builddir)/ompi/mpi/fortran/use-mpi-f08/mod \
             -I$(top_builddir) -I$(top_srcdir) $(FCFLAGS_f90)

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
noinst_LTLIBRARIES += libmpiext_ftmpi_usempif08.la

# Note that no header files are installed; instead,
# mpiext_ftmpi_usempif08.h is automatically slurped up into the
# mpi_f08_ext module.  It must be listed so that it is included in
# dist tarballs.
noinst_HEADERS = mpiext_ftmpi_usempif08.h


# Sources for the convenience libtool library.
mpi_api_files = \
    comm_revoke_f08.F90 \
    comm_is_revoked_f08.F90 \
    comm_failure_ack_f08.F90 \
    comm_failure_get_acked_f08.F90 \
    comm_get_failed_f08.F90 \
    comm_ack_failed_f08.F90 \
    comm_agree_f08.F90 \
    comm_iagree_f08.F90 \
    comm_shrink_f08.F90

pmpi_api_files = \
    profile/pcomm_revoke_f08.F90 \
    profile/pcomm_is_revoked_f08.F90 \
    profile/pcomm_failure_ack_f08.F90 \
    profile/pcomm_failure_get_acked_f08.F90 \
    profile/pcomm_get_failed_f08.F90 \
    profile/pcomm_ack_failed_f08.F90 \
    profile/pcomm_agree_f08.F90 \
    profile/pcomm_iagree_f08.F90 \
    profile/pcomm_shrink_f08.F90

mpi_api_lo_files = $(mpi_api_files:.F90=.lo)
pmpi_api_lo_files = $(pmpi_api_files:.F90=.lo)

# Sources for the convenience libtool library.
libmpiext_ftmpi_usempif08_la_SOURCES = \
        $(mpi_api_files) \
        $(pmpi_api_files)

# Remove the intermediate module file
distclean-local:
	rm -f mpiext_ftmpi_f08.mod

endif
