! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

subroutine PMPI_File_set_atomicity_f08(fh,flag,ierror)
   use :: mpi_f08_types, only : MPI_File
   use :: mpi_f08, only : ompi_file_set_atomicity_f
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   LOGICAL, INTENT(IN) :: flag
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_file_set_atomicity_f(fh%MPI_VAL,flag,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_File_set_atomicity_f08
