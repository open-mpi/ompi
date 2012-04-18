! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

subroutine MPI_File_set_info_f08(fh,info,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Info
   use :: mpi_f08, only : ompi_file_set_info_f
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   TYPE(MPI_Info), INTENT(IN) :: info
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_file_set_info_f(fh%MPI_VAL,info%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_File_set_info_f08
