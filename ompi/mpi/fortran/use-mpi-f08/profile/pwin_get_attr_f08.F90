! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Win_get_attr_f08(win,win_keyval,attribute_val,flag,ierror)
   use :: mpi_f08_types, only : MPI_Win, MPI_ADDRESS_KIND
   use :: mpi_f08, only : ompi_win_get_attr_f
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   INTEGER, INTENT(IN) :: win_keyval
   INTEGER(MPI_ADDRESS_KIND), INTENT(OUT) :: attribute_val
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_win_get_attr_f(win%MPI_VAL,win_keyval,attribute_val,flag,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Win_get_attr_f08
