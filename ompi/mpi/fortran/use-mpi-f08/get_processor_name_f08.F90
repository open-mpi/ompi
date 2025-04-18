! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! Copyright (c) 2018-2020 Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! $COPYRIGHT$

#include "mpi-f08-rename.h"

subroutine MPI_Get_processor_name_f08(name,resultlen,ierror)
   use :: ompi_mpifh_bindings, only : ompi_get_processor_name_f
   use, intrinsic :: ISO_C_BINDING, only : C_INT
   implicit none
   character(len=*), intent(out) :: name
   integer, intent(out) :: resultlen
   integer, optional, intent(out) :: ierror
   integer :: c_ierror

   call ompi_get_processor_name_f(name,resultlen,c_ierror,len(name,KIND=C_INT))
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Get_processor_name_f08
