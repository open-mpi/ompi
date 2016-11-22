! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine PMPI_Mrecv_f08(buf,count,datatype,message,status,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Message, MPI_Status
   use :: mpi_f08, only : ompi_mrecv_f
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: buf
   INTEGER, INTENT(IN) :: count
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Message), INTENT(INOUT) :: message
   TYPE(MPI_Status) :: status
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_mrecv_f(buf,count,datatype%MPI_VAL,message%MPI_VAL,status,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Mrecv_f08
