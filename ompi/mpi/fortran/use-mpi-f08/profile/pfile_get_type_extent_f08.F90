! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

subroutine PMPI_File_get_type_extent_f08(fh,datatype,extent,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_ADDRESS_KIND
   use :: mpi_f08, only : ompi_file_get_type_extent_f
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER(MPI_ADDRESS_KIND), INTENT(OUT) :: extent
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_file_get_type_extent_f(fh%MPI_VAL,datatype%MPI_VAL,extent,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_File_get_type_extent_f08
