! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine PMPI_File_write_ordered_end_f08(fh,buf,status,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Status
   use :: mpi_f08, only : ompi_file_write_ordered_end_f
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_file_write_ordered_end_f(fh%MPI_VAL,buf,status,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_File_write_ordered_end_f08
