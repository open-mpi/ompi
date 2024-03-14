! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! Copyright (c) 2018      Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! Copyright (c) 2020      Sandia National Laboratories. All rights reserved.
! $COPYRIGHT$

#include "mpi-f08-rename.h"
#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine MPI_Pready_range_f08(partition_low,partition_high,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   use :: ompi_mpifh_bindings, only : ompi_pready_range_f
   implicit none
   INTEGER, INTENT(IN) :: partition_low,partition_high
   TYPE(MPI_Request), INTENT(IN) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_pready_range_f(partition_low,partition_high,request%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Pready_range_f08
