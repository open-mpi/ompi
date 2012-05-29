! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine MPI_Topo_test_f08(comm,status,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Status
   use :: mpi_f08, only : ompi_topo_test_f
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, INTENT(OUT) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_topo_test_f(comm%MPI_VAL,status,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Topo_test_f08
