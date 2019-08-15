! -*- f90 -*-
!
! Copyright (c) 2009-2013 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! Copyright (c) 2019      Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! $COPYRIGHT$

#if OMPI_BUILD_MPI_PROFILING
#define MPI_Comm_test_inter_f08 PMPI_Comm_test_inter_f08
#endif

subroutine MPI_Comm_test_inter_f08(comm,flag,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   ! See note in mpi-f-interfaces-bind.h for why we "use mpi" here and
   ! call a PMPI_* subroutine below.
   use :: mpi, only : PMPI_Comm_test_inter
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   LOGICAL, INTENT(OUT) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call PMPI_Comm_test_inter(comm%MPI_VAL,flag,c_ierror)
   if (present(ierror)) ierror = c_ierror
end subroutine MPI_Comm_test_inter_f08
