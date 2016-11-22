! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine MPI_Group_rank_f08(group,rank,ierror)
   use :: mpi_f08_types, only : MPI_Group
   use :: mpi_f08, only : ompi_group_rank_f
   implicit none
   TYPE(MPI_Group), INTENT(IN) :: group
   INTEGER, INTENT(OUT) :: rank
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_group_rank_f(group%MPI_VAL,rank,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Group_rank_f08
