! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine PMPI_Pack_external_f08(datarep,inbuf,incount,datatype,outbuf,outsize, &
                                 position,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_ADDRESS_KIND
   use :: mpi_f08, only : ompi_pack_external_f
   implicit none
   CHARACTER(LEN=*), INTENT(IN) :: datarep
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: inbuf, outbuf
   INTEGER, INTENT(IN) :: incount
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: outsize
   INTEGER(MPI_ADDRESS_KIND), INTENT(INOUT) :: position
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_pack_external_f(datarep,inbuf,incount,datatype%MPI_VAL,outbuf, &
                             outsize,position,c_ierror,len(datarep))
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Pack_external_f08
