! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

subroutine MPI_Status_set_cancelled_f08(status,flag,ierror)
   use :: mpi_f08_types, only : MPI_Status
   use :: mpi_f08, only : ompi_status_set_cancelled_f
   implicit none
   TYPE(MPI_Status), INTENT(INOUT) :: status
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_status_set_cancelled_f(status,flag,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Status_set_cancelled_f08
