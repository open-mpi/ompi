! -*- f90 -*-
!
! Copyright (c) 2010-2014 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine MPI_Op_create_f08(user_fn,commute,op,ierror)
   use :: mpi_f08_types, only : MPI_Op
   use :: mpi_f08_interfaces_callbacks, only : MPI_User_function
   ! See note in mpi-f-interfaces-bind.h for why we "use mpi" here and
   ! call a PMPI_* subroutine below.
   use :: mpi, only : PMPI_Op_create
   implicit none
   PROCEDURE(MPI_User_function) :: user_fn
   LOGICAL, INTENT(IN) :: commute
   TYPE(MPI_Op), INTENT(OUT) :: op
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call PMPI_Op_create(user_fn,commute,op%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror
end subroutine MPI_Op_create_f08
