! -*- f90 -*-
!
! Copyright (c) 2009-2013 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Request_get_status_f08(request,flag,status,ierror)
   use :: mpi_f08_types, only : MPI_Request, MPI_Status
   implicit none
   TYPE(MPI_Request), INTENT(IN) :: request
   LOGICAL, INTENT(OUT) :: flag
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   ! See note in mpi-f-interfaces-bind.h for why we include an
   ! interface here and call a PMPI_* subroutine below.
   interface
      subroutine PMPI_Request_get_status(request, flag, status, ierror)
        use :: mpi_f08_types, only : MPI_Status
        integer, intent(in) :: request
        logical, intent(out) :: flag
        type(MPI_Status), intent(out) :: status
        integer, intent(out) :: ierror
      end subroutine PMPI_Request_get_status
   end interface

   call PMPI_Request_get_status(request%MPI_VAL,flag,status,c_ierror)
   if (present(ierror)) ierror = c_ierror
end subroutine PMPI_Request_get_status_f08
