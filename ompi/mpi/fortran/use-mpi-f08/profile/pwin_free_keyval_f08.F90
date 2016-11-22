! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Win_free_keyval_f08(win_keyval,ierror)
   use :: mpi_f08, only : ompi_win_free_keyval_f
   implicit none
   INTEGER, INTENT(INOUT) :: win_keyval
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_win_free_keyval_f(win_keyval,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Win_free_keyval_f08
