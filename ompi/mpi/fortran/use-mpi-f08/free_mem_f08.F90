! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$
!
! This file creates mappings between MPI C types (e.g., MPI_Comm) and
! variables (e.g., MPI_COMM_WORLD) and corresponding Fortran names
! (type(MPI_Comm_world) and MPI_COMM_WORLD, respectively).

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine MPI_Free_mem_f08(base,ierror)
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   use :: mpi_f08, only : ompi_free_mem_f
   implicit none
   INTEGER(MPI_ADDRESS_KIND), DIMENSION(*) OMPI_ASYNCHRONOUS :: base
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_free_mem_f(base,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Free_mem_f08
