! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! Copyright (c) 2012      University of Oregon.  All rights reserved.
! Copyright (c) 2018-2020 Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

#include "mpi-f08-rename.h"

subroutine MPI_F_sync_reg_f08(buf)
   use :: ompi_mpifh_bindings, only : ompi_f_sync_reg_f
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: buf

   call ompi_f_sync_reg_f(buf)

end subroutine MPI_F_sync_reg_f08
