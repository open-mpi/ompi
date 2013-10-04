! -*- f90 -*-
!
! Copyright (c) 2009-2013 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Test_f08(request,flag,status,ierror)
   use :: mpi_f08_types, only : MPI_Request, MPI_Status
   implicit none
   TYPE(MPI_Request), INTENT(INOUT) :: request
   LOGICAL, INTENT(OUT) :: flag
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   ! See note in mpi-f-interfaces-bind.h for why we include an
   ! interface here and call a PMPI_* subroutine below.
   interface
      subroutine PMPI_Test(request, flag, status, ierror)
        use :: mpi_f08_types, only : MPI_Status
        integer, intent(inout) :: request
        logical, intent(out) :: flag
        TYPE(MPI_Status) :: status
        integer, intent(out) :: ierror
      end subroutine PMPI_Test
   end interface

   call PMPI_Test(request%MPI_VAL,flag,status,c_ierror)
   if (present(ierror)) ierror = c_ierror
end subroutine PMPI_Test_f08
