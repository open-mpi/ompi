! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine MPI_Wait_f08(request,status,ierror)
   use :: mpi_f08_types, only : MPI_Request, MPI_Status
   use :: mpi_f08, only : ompi_wait_f
   implicit none
   TYPE(MPI_Request), INTENT(INOUT) :: request
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_wait_f(request%MPI_VAL,status,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Wait_f08
