! -*- f90 -*-
!
! Copyright (c) 2009-2013 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Test_cancelled_f08(status,flag,ierror)
   use :: mpi_f08_types, only : MPI_Status
   implicit none
   TYPE(MPI_Status), INTENT(IN) :: status
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   ! See note in mpi-f-interfaces-bind.h for why we include an
   ! interface here and call a PMPI_* subroutine below.
   interface
      subroutine PMPI_Test_cancelled(status, flag, ierror)
        use :: mpi_f08_types, only : MPI_Status
        type(MPI_Status), intent(in) :: status
        logical, intent(out) :: flag
        integer, intent(out) :: ierror
      end subroutine PMPI_Test_cancelled
   end interface

   call PMPI_Test_cancelled(status,flag,c_ierror)
   if (present(ierror)) ierror = c_ierror
end subroutine PMPI_Test_cancelled_f08
