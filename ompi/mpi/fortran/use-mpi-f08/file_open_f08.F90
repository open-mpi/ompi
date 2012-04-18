! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

subroutine MPI_File_open_f08(comm,filename,amode,info,fh,ierror)
   use :: mpi_f08_types, only : MPI_Comm, MPI_Info, MPI_File
   use :: mpi_f08, only : ompi_file_open_f
   implicit none
   TYPE(MPI_Comm), INTENT(IN) :: comm
   CHARACTER(LEN=*), INTENT(IN) :: filename
   INTEGER, INTENT(IN) :: amode
   TYPE(MPI_Info), INTENT(IN) :: info
   TYPE(MPI_File), INTENT(OUT) :: fh
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_file_open_f(comm%MPI_VAL,filename,amode,info%MPI_VAL,fh%MPI_VAL, &
                         c_ierror, len(filename))
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_File_open_f08
