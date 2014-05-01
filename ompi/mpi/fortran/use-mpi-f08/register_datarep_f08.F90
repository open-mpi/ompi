! -*- f90 -*-
!
! Copyright (c) 2009-2014 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine MPI_Register_datarep_f08(datarep,read_conversion_fn,write_conversion_fn, &
                                    dtype_file_extent_fn,extra_state,ierror)
   use, intrinsic :: iso_c_binding, only: c_funptr, c_funloc
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   use :: mpi_f08_interfaces_callbacks, only : MPI_Datarep_conversion_function
   use :: mpi_f08_interfaces_callbacks, only : MPI_Datarep_extent_function
   use :: mpi_f08, only : ompi_register_datarep_f
   implicit none
   PROCEDURE(MPI_Datarep_conversion_function) :: read_conversion_fn
   PROCEDURE(MPI_Datarep_conversion_function) :: write_conversion_fn
   PROCEDURE(MPI_Datarep_extent_function) :: dtype_file_extent_fn
   CHARACTER(LEN=*), INTENT(IN) :: datarep
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: extra_state
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror
   type(c_funptr) :: fread_fn, fwrite_fn, fdtype_fn

   fread_fn = c_funloc(read_conversion_fn)
   fwrite_fn = c_funloc(write_conversion_fn)
   fdtype_fn = c_funloc(dtype_file_extent_fn)
   call ompi_register_datarep_f(datarep,fread_fn,fwrite_fn, &
                                fdtype_fn,extra_state,c_ierror,len(datarep))
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Register_datarep_f08
