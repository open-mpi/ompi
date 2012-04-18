! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

subroutine MPI_Comm_get_errhandler_f08(comm,errhandler,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Errhandler
   use :: mpi_f08, only : ompi_comm_get_errhandler_f
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_comm_get_errhandler_f(comm%MPI_VAL,errhandler%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Comm_get_errhandler_f08
