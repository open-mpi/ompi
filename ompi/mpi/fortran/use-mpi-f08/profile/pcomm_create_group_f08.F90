! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2013 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Comm_create_group_f08(comm,group,tag,newcomm,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Group
   use :: mpi_f08, only : ompi_comm_create_group_f
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Group), INTENT(IN) :: group
   INTEGER, INTENT(IN) :: tag
   TYPE(MPI_Comm), INTENT(OUT) :: newcomm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_comm_create_group_f(comm%MPI_VAL,group%MPI_VAL,tag,newcomm%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Comm_create_group_f08
