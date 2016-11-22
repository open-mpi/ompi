! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine PMPI_Ireduce_scatter_block_f08(sendbuf,recvbuf,recvcount,datatype,op,comm,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Op, MPI_Comm, MPI_Request
   use :: mpi_f08, only : ompi_ireduce_scatter_block_f
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: recvcount
   TYPE(MPI_Datatype), INTENT(IN) :: datatype
   TYPE(MPI_Op), INTENT(IN) :: op
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompi_ireduce_scatter_block_f(sendbuf,recvbuf,recvcount,&
                                    datatype%MPI_VAL,op%MPI_VAL,comm%MPI_VAL,request%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPI_Ireduce_scatter_block_f08
