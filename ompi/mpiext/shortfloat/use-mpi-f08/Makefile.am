#
# Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
# Copyright (c) 2019      Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# This file builds the use_mpi_f08-based bindings for MPI extensions.  It
# is optional in MPI extensions.

# Use the Automake conditional to know if we're building the "use mpi_f08"
# bindings.
if OMPI_BUILD_FORTRAN_USEMPIF08_BINDINGS

# Note that no header files are installed; instead,
# mpiext_shortfloat_usempif08.h is automatically slurped up into the
# mpi_f08_ext module.  It must be listed so that it is included in
# dist tarballs.
nodist_noinst_HEADERS = mpiext_shortfloat_usempif08.h

endif
