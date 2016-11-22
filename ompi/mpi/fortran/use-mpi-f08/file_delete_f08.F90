! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

subroutine MPI_File_delete_f08(filename,info,ierror)
   use :: mpi_f08_types, only : MPI_Info
   use :: mpi_f08, only : ompi_file_delete_f
   implicit none
   CHARACTER(LEN=*), INTENT(IN) :: filename
   TYPE(MPI_Info), INTENT(IN) :: info
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_file_delete_f(filename,info%MPI_VAL,c_ierror,len(filename))
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_File_delete_f08
