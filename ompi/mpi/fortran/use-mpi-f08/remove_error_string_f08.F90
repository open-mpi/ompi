! -*- f90 -*-
!
! Copyright (c) 2010-2011 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! Copyright (c) 2018-2020 Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! Copyright (c) 2025      UT-Battelle, LLC.  All rights reserved.
! $COPYRIGHT$

#include "mpi-f08-rename.h"

subroutine MPI_Remove_error_string_f08(errorcode,ierror)
   use :: ompi_mpifh_bindings, only : ompi_remove_error_string_f
   use, intrinsic :: ISO_C_BINDING, only : C_INT
   implicit none
   integer, intent(in) :: errorcode
   integer, optional, intent(out) :: ierror
   integer :: c_ierror

   call ompi_remove_error_string_f(errorcode, c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Remove_error_string_f08
