! -*- f90 -*-
!
! Copyright (c) 2009-2013 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine MPI_Alltoallw_f08(sendbuf,sendcounts,sdispls,sendtypes,&
                             recvbuf,recvcounts,rdispls,recvtypes,comm,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm
   use :: mpi_f08, only : ompi_alltoallw_f
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
   TYPE(MPI_Datatype), INTENT(IN) :: sendtypes(*)
   TYPE(MPI_Datatype), INTENT(IN) :: recvtypes(*)
   TYPE(MPI_Comm), INTENT(IN) :: comm
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   ! Note that we pass a scalar here for both the sendtypes and
   ! recvtypes arguments, even though the real Alltoallw function
   ! expects an array of integers.  This is a hack: we know that
   ! [send|recv]types(1)%MPI_VAL will pass the address of the first
   ! integer in the array of Type(MPI_Datatype) derived types.  And
   ! since Type(MPI_Datatype) are exactly memory-equivalent to a
   ! single INTEGER, passing the address of the first one is the same
   ! as passing the address to an array of integers.  To be clear: the
   ! back-end ompi_alltoallw_f is expecting a pointer to an array of
   ! integers.  So it all works out (but is a hack :-\ ).
   call ompi_alltoallw_f(sendbuf,sendcounts,sdispls,sendtypes(1)%MPI_VAL,&
                         recvbuf,recvcounts,rdispls,recvtypes(1)%MPI_VAL,comm%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPI_Alltoallw_f08
