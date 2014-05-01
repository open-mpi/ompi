! -*- f90 -*-
!
! Copyright (c) 2009-2014 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine PMPI_Type_create_keyval_f08(type_copy_attr_fn,type_delete_attr_fn,&
                                       type_keyval,extra_state,ierror)
   use, intrinsic :: iso_c_binding, only: c_funptr, c_funloc
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   use :: mpi_f08_interfaces_callbacks, only : MPI_Type_copy_attr_function
   use :: mpi_f08_interfaces_callbacks, only : MPI_Type_delete_attr_function
   use :: mpi_f08, only : ompi_type_create_keyval_f
   implicit none
   PROCEDURE(MPI_Type_copy_attr_function) :: type_copy_attr_fn
   PROCEDURE(MPI_Type_delete_attr_function) :: type_delete_attr_fn
   INTEGER, INTENT(OUT) :: type_keyval
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: extra_state
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror
   type(c_funptr) :: fcopy_fn, fdelete_fn

   fcopy_fn = c_funloc(type_copy_attr_fn)
   fdelete_fn = c_funloc(type_delete_attr_fn)
   call ompi_type_create_keyval_f(fcopy_fn,fdelete_fn,&
                                  type_keyval,extra_state,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Type_create_keyval_f08
