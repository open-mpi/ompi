! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

subroutine MPI_File_get_byte_offset_f08(fh,offset,disp,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_OFFSET_KIND
   use :: mpi_f08, only : ompi_file_get_byte_offset_f
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: offset
   INTEGER(MPI_OFFSET_KIND), INTENT(OUT) :: disp
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_file_get_byte_offset_f(fh%MPI_VAL,offset,disp,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_File_get_byte_offset_f08
