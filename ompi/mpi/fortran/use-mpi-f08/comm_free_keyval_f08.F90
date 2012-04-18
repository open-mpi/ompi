! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine MPI_Comm_free_keyval_f08(comm_keyval,ierror)
   use :: mpi_f08, only : ompi_comm_free_keyval_f
   implicit none
   INTEGER, INTENT(INOUT) :: comm_keyval
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_comm_free_keyval_f(comm_keyval,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Comm_free_keyval_f08
