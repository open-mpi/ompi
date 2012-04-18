! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine MPI_Buffer_attach_f08(buffer,size,ierror)
   use :: mpi_f08, only : ompi_buffer_attach_f
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buffer
   INTEGER, INTENT(IN) :: size
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_buffer_attach_f(buffer,size,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Buffer_attach_f08
