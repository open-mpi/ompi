! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! Copyright (c) 2018      Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine PMPI_Buffer_attach_f08(buffer,size,ierror)
   use :: ompi_mpifh_bindings, only : ompi_buffer_attach_f
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: buffer
   INTEGER, INTENT(IN) :: size
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_buffer_attach_f(buffer,size,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Buffer_attach_f08
