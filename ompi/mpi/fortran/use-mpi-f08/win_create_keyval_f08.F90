! -*- f90 -*-
!
! Copyright (c) 2010-2014 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine MPI_Win_create_keyval_f08(win_copy_attr_fn,win_delete_attr_fn,&
                                     win_keyval,extra_state,ierror)
   use, intrinsic :: iso_c_binding, only: c_funptr, c_funloc
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   use :: mpi_f08_interfaces_callbacks, only : MPI_Win_copy_attr_function
   use :: mpi_f08_interfaces_callbacks, only : MPI_Win_delete_attr_function
   use :: mpi_f08, only : ompi_win_create_keyval_f
   implicit none
   PROCEDURE(MPI_Win_copy_attr_function) :: win_copy_attr_fn
   PROCEDURE(MPI_Win_delete_attr_function) :: win_delete_attr_fn
   INTEGER, INTENT(OUT) :: win_keyval
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: extra_state
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror
   type(c_funptr) :: fcopy_fn, fdelete_fn

   fcopy_fn = c_funloc(win_copy_attr_fn)
   fdelete_fn = c_funloc(win_delete_attr_fn)
   call ompi_win_create_keyval_f(fcopy_fn,fdelete_fn,&
                                 win_keyval,extra_state,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Win_create_keyval_f08
