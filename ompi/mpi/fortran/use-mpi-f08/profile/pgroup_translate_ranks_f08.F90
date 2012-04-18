! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Group_translate_ranks_f08(group1,n,ranks1,group2,ranks2,ierror)
   use :: mpi_f08_types, only : MPI_Group
   use :: mpi_f08, only : ompi_group_translate_ranks_f
   implicit none
   TYPE(MPI_Group), INTENT(IN) :: group1, group2
   INTEGER, INTENT(IN) :: n
   INTEGER, INTENT(IN) :: ranks1(*)
   INTEGER, INTENT(OUT) :: ranks2(*)
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_group_translate_ranks_f(group1%MPI_VAL,n,ranks1,group2%MPI_VAL,ranks2,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Group_translate_ranks_f08
