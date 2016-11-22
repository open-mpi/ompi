! -*- f90 -*-
!
! Copyright (c) 2010-2015 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2015 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

function PMPI_Aint_add_f08(base, diff)
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   use :: mpi_f08, only : ompi_aint_add_f
   implicit none
   INTEGER(MPI_ADDRESS_KIND) :: PMPI_Aint_add_f08
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: base
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: diff
   PMPI_Aint_add_f08 = ompi_aint_add_f(base, diff)
end function PMPI_Aint_add_f08
