! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

subroutine PMPI_File_get_errhandler_f08(file,errhandler,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Errhandler
   use :: mpi_f08, only : ompi_file_get_errhandler_f
   implicit none
   TYPE(MPI_File), INTENT(IN) :: file
   TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_file_get_errhandler_f(file%MPI_VAL,errhandler%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_File_get_errhandler_f08
