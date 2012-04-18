! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$
!
! This file provides the interface specifications for the MPI Fortran
! API bindings.  It effectively maps between public names ("MPI_Init")
! and the back-end implementation subroutine name (e.g., "ompi_init_f").

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine PMPI_Free_mem_f08(base,ierror)
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   use :: mpi_f08, only : ompi_free_mem_f
   implicit none
   INTEGER(MPI_ADDRESS_KIND), DIMENSION(*) OMPI_ASYNCHRONOUS :: base
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_free_mem_f(base,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Free_mem_f08
