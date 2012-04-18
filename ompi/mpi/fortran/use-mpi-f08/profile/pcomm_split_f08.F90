! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Comm_split_f08(comm,color,key,newcomm,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   use :: mpi_f08, only : ompi_comm_split_f
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: color, key
   TYPE(MPI_Comm), INTENT(OUT) :: newcomm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_comm_split_f(comm%MPI_VAL,color,key,newcomm%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Comm_split_f08
