! -*- f90 -*-
!
! Copyright (c) 2009-2013 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine MPI_Testany_f08(count,array_of_requests,index,flag,status,ierror)
   use :: mpi_f08_types, only : MPI_Request, MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(count)
   INTEGER, INTENT(OUT) :: index
   LOGICAL, INTENT(OUT) :: flag
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   ! See note in mpi-f-interfaces-bind.h for why we include an
   ! interface here and call a PMPI_* subroutine below.
   interface
      subroutine PMPI_Testany(count, array_of_requests, index, flag, status&
           , ierror)
        use :: mpi_f08_types, only : MPI_Status
        integer, intent(in) :: count
        integer, dimension(count), intent(inout) :: array_of_requests
        integer, intent(out) :: index
        logical, intent(out) :: flag
        type(MPI_Status), intent(out) :: status
        integer, intent(out) :: ierror
      end subroutine PMPI_Testany
   end interface

   call PMPI_Testany(count,array_of_requests(:)%MPI_VAL,index,flag,status,c_ierror)
   if (present(ierror)) ierror = c_ierror
end subroutine MPI_Testany_f08
