! -*- f90 -*-
!
! Copyright (c) 2009-2013 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! Copyright (c) 2018      Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine PMPIX_Alltoallw_init_f08(sendbuf,sendcounts,sdispls,sendtypes,&
                                    recvbuf,recvcounts,rdispls,recvtypes,comm,info,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Info, MPI_Request
   use :: mpiext_pcollreq_f08, only : ompix_alltoallw_init_f
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN), ASYNCHRONOUS :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, ASYNCHRONOUS :: recvbuf
   INTEGER, INTENT(IN), ASYNCHRONOUS :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
   TYPE(MPI_Datatype), INTENT(IN), ASYNCHRONOUS :: sendtypes(*), recvtypes(*)
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Info), INTENT(IN) :: info
   TYPE(MPI_Request), INTENT(OUT) :: request
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
   call ompix_alltoallw_init_f(sendbuf,sendcounts,sdispls,sendtypes(1)%MPI_VAL,&
                               recvbuf,recvcounts,rdispls,recvtypes(1)%MPI_VAL,comm%MPI_VAL,info%MPI_VAL,request%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPIX_Alltoallw_init_f08
