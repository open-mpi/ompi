! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Op_commutative_f08(op,commute,ierror)
   use :: mpi_f08_types, only : MPI_Op
   use :: mpi_f08, only : ompi_op_commutative_f
   implicit none
   TYPE(MPI_Op), INTENT(IN) :: op
   LOGICAL, INTENT(OUT) :: commute
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_op_commutative_f(op%MPI_VAL,commute,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Op_commutative_f08
