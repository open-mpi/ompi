! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine PMPI_Unpack_external_f08(datarep,inbuf,insize,position,outbuf,outcount,datatype,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_ADDRESS_KIND
   use :: mpi_f08, only : ompi_unpack_external_f
   implicit none
   CHARACTER(LEN=*), INTENT(IN) :: datarep
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: inbuf, outbuf
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: insize
   INTEGER(MPI_ADDRESS_KIND), INTENT(INOUT) :: position
   INTEGER, INTENT(IN) :: outcount
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_unpack_external_f(datarep,inbuf,insize,position,outbuf,&
                               outcount,datatype%MPI_VAL,c_ierror,len(datarep))
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Unpack_external_f08
