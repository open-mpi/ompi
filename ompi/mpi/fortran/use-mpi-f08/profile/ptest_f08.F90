! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Test_f08(request,flag,status,ierror)
   use :: mpi_f08_types, only : MPI_Request, MPI_Status
   use :: mpi_f08, only : ompi_test_f
   implicit none
   TYPE(MPI_Request), INTENT(INOUT) :: request
   LOGICAL, INTENT(OUT) :: flag
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_test_f(request%MPI_VAL,flag,status,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Test_f08
