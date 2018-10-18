! -*- f90 -*-
!
! Copyright (c) 2010-2015 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2015 Los Alamos National Security, LLC.
!               All Rights reserved.
! Copyright (c) 2018      Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

function MPI_Aint_add_f08(base, disp)
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   use :: ompi_mpifh_bindings, only : ompi_aint_add_f
   implicit none
   INTEGER(MPI_ADDRESS_KIND) :: MPI_Aint_add_f08
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: base
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: disp
   MPI_Aint_add_f08 = ompi_aint_add_f(base, disp)
end function MPI_Aint_add_f08
