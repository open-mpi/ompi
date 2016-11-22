! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Op_free_f08(op,ierror)
   use :: mpi_f08_types, only : MPI_Op
   use :: mpi_f08, only : ompi_op_free_f
   implicit none
   TYPE(MPI_Op), INTENT(INOUT) :: op
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_op_free_f(op%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Op_free_f08
