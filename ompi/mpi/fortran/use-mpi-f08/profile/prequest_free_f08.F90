! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Request_free_f08(request,ierror)
   use :: mpi_f08_types, only : MPI_Request
   use :: mpi_f08, only : ompi_request_free_f
   implicit none
   TYPE(MPI_Request), INTENT(INOUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_request_free_f(request%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Request_free_f08
