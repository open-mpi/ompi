! -*- f90 -*-
!
! Copyright (c) 2009-2015 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! Copyright (c) 2018      Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine MPI_Buffer_detach_f08(buffer_addr,size,ierror)
   USE, INTRINSIC ::  ISO_C_BINDING, ONLY : C_PTR
   use :: ompi_mpifh_bindings, only : ompi_buffer_detach_f
   implicit none
   TYPE(C_PTR), INTENT(OUT) ::  buffer_addr
   INTEGER, INTENT(OUT) :: size
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_buffer_detach_f(buffer_addr,size,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Buffer_detach_f08
