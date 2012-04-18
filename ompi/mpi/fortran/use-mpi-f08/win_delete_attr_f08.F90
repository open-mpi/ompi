! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine MPI_Win_delete_attr_f08(win,win_keyval,ierror)
   use :: mpi_f08_types, only : MPI_Win
   use :: mpi_f08, only : ompi_win_delete_attr_f
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, INTENT(IN) :: win_keyval
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_win_delete_attr_f(win%MPI_VAL,win_keyval,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Win_delete_attr_f08
