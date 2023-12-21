! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! Copyright (c) 2018      Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! Copyright (c) 2020      Sandia National Laboratories. All rights reserved.
! $COPYRIGHT$

#include "mpi-f08-rename.h"
#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine MPI_Parrived_f08(request,partition,flag,ierror)
   ! See note in mpi-f-interfaces-bind.h for why we "use mpi" here and
   ! call a PMPI_* subroutine below.
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   use :: mpi, only : PMPI_Parrived
   implicit none
   TYPE(MPI_Request), INTENT(IN) :: request
   INTEGER, INTENT(IN) :: partition
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call PMPI_Parrived(request%MPI_VAL,partition,flag,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Parrived_f08
