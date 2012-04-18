! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine PMPI_Buffer_detach_f08(buffer_addr,size,ierror)
   use :: mpi_f08, only : ompi_buffer_detach_f
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buffer_addr
   INTEGER, INTENT(OUT) :: size
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_buffer_detach_f(buffer_addr,size,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Buffer_detach_f08
