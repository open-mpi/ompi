! -*- f90 -*-
!
! Copyright (c) 2010-2013 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

subroutine MPI_Initialized_f08(flag,ierror)
   ! See note in mpi-f-interfaces-bind.h for why we "use mpi" here and
   ! call a PMPI_* subroutine below.
   use :: mpi, only : PMPI_Initialized
   implicit none
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call PMPI_Initialized(flag,c_ierror)
   if (present(ierror)) ierror = c_ierror
end subroutine MPI_Initialized_f08
