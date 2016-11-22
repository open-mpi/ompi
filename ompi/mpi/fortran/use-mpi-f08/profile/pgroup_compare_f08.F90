! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Group_compare_f08(group1,group2,result,ierror)
   use :: mpi_f08_types, only : MPI_Group
   use :: mpi_f08, only : ompi_group_compare_f
   implicit none
   TYPE(MPI_Group), INTENT(IN) :: group1
   TYPE(MPI_Group), INTENT(IN) :: group2
   INTEGER, INTENT(OUT) :: result
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_group_compare_f(group1%MPI_VAL,group2%MPI_VAL,result,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Group_compare_f08
