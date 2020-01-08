! -*- f90 -*-
!
! Copyright (c) 2010-2013 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! Copyright (c) 2019      Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! $COPYRIGHT$

#if OMPI_BUILD_MPI_PROFILING
#define MPI_Is_thread_main_f08 PMPI_Is_thread_main_f08
#endif

subroutine MPI_Is_thread_main_f08(flag,ierror)
   ! See note in mpi-f-interfaces-bind.h for why we "use mpi" here and
   ! call a PMPI_* subroutine below.
   use :: mpi, only : PMPI_Is_thread_main
   implicit none
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call PMPI_Is_thread_main(flag,c_ierror)
   if (present(ierror)) ierror = c_ierror
end subroutine MPI_Is_thread_main_f08
