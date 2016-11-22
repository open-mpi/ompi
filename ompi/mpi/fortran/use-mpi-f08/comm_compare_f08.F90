! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine MPI_Comm_compare_f08(comm1,comm2,result,ierror)
   use :: mpi_f08_types, only : MPI_Comm
   use :: mpi_f08, only : ompi_comm_compare_f
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm1
   TYPE(MPI_Comm), INTENT(IN) :: comm2
   INTEGER, INTENT(OUT) :: result
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_comm_compare_f(comm1%MPI_VAL,comm2%MPI_VAL,result,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Comm_compare_f08
