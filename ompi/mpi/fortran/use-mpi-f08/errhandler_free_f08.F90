! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

subroutine MPI_Errhandler_free_f08(errhandler,ierror)
   use :: mpi_f08_types, only : MPI_Errhandler
   use :: mpi_f08, only : ompi_errhandler_free_f
   implicit none
   TYPE(MPI_Errhandler), INTENT(INOUT) :: errhandler
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_errhandler_free_f(errhandler%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Errhandler_free_f08
