! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Waitsome_f08(incount,array_of_requests,outcount, &
                             array_of_indices,array_of_statuses,ierror)
   use :: mpi_f08_types, only : MPI_Request, MPI_Status
   use :: mpi_f08, only : ompi_waitsome_f
   implicit none
   INTEGER, INTENT(IN) :: incount
   TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(incount)
   INTEGER, INTENT(OUT) :: outcount
   INTEGER, INTENT(OUT) :: array_of_indices(*)
   TYPE(MPI_Status), INTENT(OUT) :: array_of_statuses(*)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_waitsome_f(incount,array_of_requests(:)%MPI_VAL,outcount, &
                        array_of_indices,array_of_statuses,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Waitsome_f08
