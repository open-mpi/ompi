! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! Copyright (c) 2012      University of Oregon.  All rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine MPI_F_sync_reg_f08(buf)
   use :: mpi_f08, only : ompi_f_sync_reg_f
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: buf

   call ompi_f_sync_reg_f(buf)

end subroutine MPI_F_sync_reg_f08
