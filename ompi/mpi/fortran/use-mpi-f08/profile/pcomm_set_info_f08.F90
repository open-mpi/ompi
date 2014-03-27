! -*- f90 -*-
!
! Copyright (c) 2009-2014 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine PMPI_Comm_set_info_f08(comm,info,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Info
   use :: mpi_f08, only : ompi_comm_set_info_f
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Info), INTENT(IN) :: info
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_comm_set_info_f(comm%MPI_VAL,info%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Comm_set_info_f08
