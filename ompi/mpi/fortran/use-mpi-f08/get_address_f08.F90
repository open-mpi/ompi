! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine MPI_Get_address_f08(location,address,ierror)
   use :: mpi_f08_types, only : MPI_ADDRESS_KIND
   use :: mpi_f08, only : ompi_get_address_f
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: location
   INTEGER(MPI_ADDRESS_KIND), INTENT(OUT) :: address
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_get_address_f(location,address,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Get_address_f08
