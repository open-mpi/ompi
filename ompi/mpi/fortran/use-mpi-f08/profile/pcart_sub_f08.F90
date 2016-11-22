! -*- f90 -*-
!
! Copyright (c) 2010-2013 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Cart_sub_f08(comm,remain_dims,newcomm,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   ! See note in mpi-f-interfaces-bind.h for why we "use mpi" here and
   ! call a PMPI_* subroutine below.
   use :: mpi, only : PMPI_Cart_sub
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   LOGICAL, INTENT(IN) :: remain_dims(*)
   TYPE(MPI_Comm), INTENT(OUT) :: newcomm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call PMPI_Cart_sub(comm%MPI_VAL,remain_dims,newcomm%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror
end subroutine PMPI_Cart_sub_f08
