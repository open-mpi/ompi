! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine PMPI_File_iread_at_f08(fh,offset,buf,count,datatype,request,ierror)
   use :: mpi_f08_types, only : MPI_File, MPI_Datatype, MPI_Request, MPI_OFFSET_KIND
   use :: mpi_f08, only : ompi_file_iread_at_f
   implicit none
   TYPE(MPI_File), INTENT(IN) :: fh
   INTEGER(MPI_OFFSET_KIND), INTENT(IN) :: offset
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buf
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_file_iread_at_f(fh%MPI_VAL,offset,buf,count,&
                             datatype%MPI_VAL,request%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_File_iread_at_f08
