! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine MPI_Comm_set_name_f08(comm,comm_name,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   use :: mpi_f08, only : ompi_comm_set_name_f
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   CHARACTER(LEN=*), INTENT(IN) :: comm_name
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_comm_set_name_f(comm%MPI_VAL,comm_name,c_ierror,len(comm_name))
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Comm_set_name_f08
