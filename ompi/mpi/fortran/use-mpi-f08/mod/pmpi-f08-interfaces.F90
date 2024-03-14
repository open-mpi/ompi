! -*- f90 -*-
!
! Copyright (c) 2009-2018 Cisco Systems, Inc.  All rights reserved
! Copyright (c) 2009-2013 Los Alamos National Security, LLC.
!                         All rights reserved.
! Copyright (c) 2012      The University of Tennessee and The University
!                         of Tennessee Research Foundation.  All rights
!                         reserved.
! Copyright (c) 2012      Inria.  All rights reserved.
! Copyright (c) 2015-2020 Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! Copyright (c) 2017-2018 FUJITSU LIMITED.  All rights reserved.
! Copyright (c) 2019      Triad National Security, LLC. All rights
!                         reserved.
! $COPYRIGHT$
!
! This file provides the interface specifications for the MPI Fortran
! API bindings.  It effectively maps between public names ("MPI_Init")
! and the name for tools ("MPI_Init_f08") and the back-end implementation
! name (e.g., "MPI_Init_f08").

#include "ompi/mpi/fortran/configure-fortran-output.h"

#define OMPI_BUILD_MPI_PROFILING 1

#include "mpi-f08-rename.h"

module pmpi_f08_interfaces

#include "mpi-f08-interfaces.h"

! MPI_Wtick is not a wrapper function
!
interface PMPI_Wtick
function  PMPI_Wtick_f08( ) BIND(C,name="PMPI_Wtick")
   use, intrinsic :: ISO_C_BINDING
   implicit none
   DOUBLE PRECISION :: PMPI_Wtick_f08
end function  PMPI_Wtick_f08
end interface PMPI_Wtick

! MPI_Wtime is not a wrapper function
!
interface PMPI_Wtime
function  PMPI_Wtime_f08( ) BIND(C,name="PMPI_Wtime")
   use, intrinsic :: ISO_C_BINDING
   implicit none
   DOUBLE PRECISION :: PMPI_Wtime_f08
end function  PMPI_Wtime_f08
end interface PMPI_Wtime

end module pmpi_f08_interfaces
