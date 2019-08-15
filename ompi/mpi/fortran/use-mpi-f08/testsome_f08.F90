! -*- f90 -*-
!
! Copyright (c) 2009-2013 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! Copyright (c) 2019      Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! $COPYRIGHT$

#if OMPI_BUILD_MPI_PROFILING
#define MPI_Testsome_f08 PMPI_Testsome_f08
#endif

subroutine MPI_Testsome_f08(incount,array_of_requests,outcount, &
                            array_of_indices,array_of_statuses,ierror)
   use :: mpi_f08_types, only : MPI_Request, MPI_Status
   implicit none
   INTEGER, INTENT(IN) :: incount
   TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(incount)
   INTEGER, INTENT(OUT) :: outcount
   INTEGER, INTENT(OUT) :: array_of_indices(*)
   TYPE(MPI_Status), INTENT(OUT) :: array_of_statuses(*)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   ! See note in mpi-f-interfaces-bind.h for why we include an
   ! interface here and call a PMPI_* subroutine below.
   interface
      subroutine PMPI_Testsome(incount, array_of_requests, outcount, array_of_indices, array_of_statuses&
           , ierror)
        use :: mpi_f08_types, only : MPI_Status
        integer, intent(in) :: incount
        integer, dimension(incount), intent(inout) :: array_of_requests
        integer, intent(out) :: outcount
        integer, dimension(*), intent(out) :: array_of_indices
        type(MPI_Status), dimension(*), intent(out) :: array_of_statuses
        integer, intent(out) :: ierror
      end subroutine PMPI_Testsome
   end interface

   call PMPI_Testsome(incount,array_of_requests(:)%MPI_VAL,outcount, &
                        array_of_indices,array_of_statuses,c_ierror)
   if (present(ierror)) ierror = c_ierror
end subroutine MPI_Testsome_f08
