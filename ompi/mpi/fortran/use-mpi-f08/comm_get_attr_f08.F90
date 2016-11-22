! -*- f90 -*-
!
! Copyright (c) 2009-2013 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine MPI_Comm_get_attr_f08(comm,comm_keyval,attribute_val,flag,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_ADDRESS_KIND
   ! See note in mpi-f-interfaces-bind.h for why we "use mpi" here and
   ! call a PMPI_* subroutine below.
   use :: mpi, only : PMPI_Comm_get_attr
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: comm_keyval
   INTEGER(MPI_ADDRESS_KIND), INTENT(OUT) :: attribute_val
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call PMPI_Comm_get_attr(comm%MPI_VAL,comm_keyval,attribute_val,flag,c_ierror)
   if (present(ierror)) ierror = c_ierror
end subroutine MPI_Comm_get_attr_f08
