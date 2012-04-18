! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine PMPI_Register_datarep_f08(datarep,read_conversion_fn,write_conversion_fn, &
                                    dtype_file_extent_fn,extra_state,ierror)
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   use :: mpi_f08_interfaces_callbacks, only : MPI_Datarep_conversion_function
   use :: mpi_f08_interfaces_callbacks, only : MPI_Datarep_extent_function
   use :: mpi_f08, only : ompi_register_datarep_f
   implicit none
   OMPI_PROCEDURE(MPI_Datarep_conversion_function) :: read_conversion_fn
   OMPI_PROCEDURE(MPI_Datarep_conversion_function) :: write_conversion_fn
   OMPI_PROCEDURE(MPI_Datarep_extent_function) :: dtype_file_extent_fn
   CHARACTER(LEN=*), INTENT(IN) :: datarep
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: extra_state
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_register_datarep_f(datarep,read_conversion_fn,write_conversion_fn, &
                                dtype_file_extent_fn,extra_state,c_ierror,len(datarep))
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Register_datarep_f08
