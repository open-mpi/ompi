! -*- f90 -*-
!
! Copyright (c) 2009-2013 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine MPI_Intercomm_merge_f08(intercomm,high,newintracomm,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   ! See note in mpi-f-interfaces-bind.h for why we "use mpi" here and
   ! call a PMPI_* subroutine below.
   use :: mpi, only : PMPI_Intercomm_merge
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: intercomm
   LOGICAL, INTENT(IN) :: high
   TYPE(MPI_Comm), INTENT(OUT) :: newintracomm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call PMPI_Intercomm_merge(intercomm%MPI_VAL,high,newintracomm%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror
end subroutine MPI_Intercomm_merge_f08
