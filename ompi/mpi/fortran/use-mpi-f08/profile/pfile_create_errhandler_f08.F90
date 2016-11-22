! -*- f90 -*-
!
! Copyright (c) 2010-2014 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine PMPI_File_create_errhandler_f08(file_errhandler_fn,errhandler,ierror)
   use, intrinsic :: iso_c_binding, only: c_funptr, c_funloc
   use :: mpi_f08_types, only : MPI_Errhandler
   use :: mpi_f08_interfaces_callbacks, only : MPI_File_errhandler_function
   use :: mpi_f08, only : ompi_file_create_errhandler_f
   implicit none
   PROCEDURE(MPI_File_errhandler_function) :: file_errhandler_fn
   TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror
   type(c_funptr) :: fn

   fn = c_funloc(file_errhandler_fn)
   call ompi_file_create_errhandler_f(fn,errhandler%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_File_create_errhandler_f08
