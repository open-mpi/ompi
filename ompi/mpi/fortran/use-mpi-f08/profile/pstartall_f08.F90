! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Startall_f08(count,array_of_requests,ierror)
   use :: mpi_f08_types, only : MPI_Request
   use :: mpi_f08, only : ompi_startall_f
   implicit none
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(count)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_startall_f(count,array_of_requests(:)%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Startall_f08
