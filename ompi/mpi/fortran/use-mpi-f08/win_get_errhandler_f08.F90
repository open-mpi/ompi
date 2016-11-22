! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

subroutine MPI_Win_get_errhandler_f08(win,errhandler,ierror)
   use :: mpi_f08_types, only : MPI_Win, MPI_Errhandler
   use :: mpi_f08, only : ompi_win_get_errhandler_f
   implicit none
   TYPE(MPI_Win), INTENT(IN) :: win
   TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_win_get_errhandler_f(win%MPI_VAL,errhandler%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Win_get_errhandler_f08
