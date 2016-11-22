! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

subroutine PMPI_File_get_view_f08(fh,disp,etype,filetype,datarep,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_OFFSET_KIND
   use :: mpi_f08, only : ompi_file_get_view_f
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(OUT) :: disp
   TYPE(MPI_Datatype), INTENT(OUT) :: etype
   TYPE(MPI_Datatype), INTENT(OUT) :: filetype
   CHARACTER(LEN=*), INTENT(OUT) :: datarep
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_file_get_view_f(fh%MPI_VAL,disp,etype%MPI_VAL,filetype%MPI_VAL, &
                             datarep,c_ierror,len(datarep))
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_File_get_view_f08
