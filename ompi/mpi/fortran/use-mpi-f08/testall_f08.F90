! -*- f90 -*-
!
! Copyright (c) 2009-2013 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine MPI_Testall_f08(count,array_of_requests,flag,array_of_statuses,ierror)
   use :: mpi_f08_types, only : MPI_Request, MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(count)
   LOGICAL, INTENT(OUT) :: flag
   TYPE(MPI_Status), INTENT(OUT) :: array_of_statuses(*)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   ! See note in mpi-f-interfaces-bind.h for why we include an
   ! interface here and call a PMPI_* subroutine below.
   interface
      subroutine PMPI_Testall(count, array_of_requests, flag, array_of_statuses, ierror)
        use :: mpi_f08_types, only : MPI_Status
        integer, intent(in) :: count
        integer, dimension(count), intent(inout) :: array_of_requests
        logical, intent(out) :: flag
        type(MPI_Status), dimension(*), intent(out) :: array_of_statuses
        integer, intent(out) :: ierror
      end subroutine PMPI_Testall
   end interface

   call PMPI_Testall(count,array_of_requests(:)%MPI_VAL,flag,array_of_statuses,c_ierror)
   if (present(ierror)) ierror = c_ierror
end subroutine MPI_Testall_f08
