! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine MPI_Waitall_f08(count,array_of_requests,array_of_statuses,ierror)
   use :: mpi_f08_types, only : MPI_Request, MPI_Status
   use :: mpi_f08, only : ompi_waitall_f
   implicit none
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(count)
   TYPE(MPI_Status), INTENT(OUT) :: array_of_statuses(*)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_waitall_f(count,array_of_requests(:)%MPI_VAL,array_of_statuses,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Waitall_f08
