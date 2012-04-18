! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine PMPI_File_write_ordered_f08(fh,buf,count,datatype,status,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_Status
   use :: mpi_f08, only : ompi_file_write_ordered_f
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Status), INTENT(OUT) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_file_write_ordered_f(fh%MPI_VAL,buf,count,&
                                  datatype%MPI_VAL,status,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_File_write_ordered_f08
