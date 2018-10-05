! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! Copyright (c) 2018      Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine PMPIX_Alltoallv_init_f08(sendbuf,sendcounts,sdispls,sendtype,recvbuf,&
                                    recvcounts,rdispls,recvtype,comm,info,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Info, MPI_Request
   use :: mpiext_pcollreq_f08, only : ompix_alltoallv_init_f
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN), ASYNCHRONOUS :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, ASYNCHRONOUS :: recvbuf
   INTEGER, INTENT(IN), ASYNCHRONOUS :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
   TYPE(MPI_Datatype), INTENT(IN) :: sendtype
   TYPE(MPI_Datatype), INTENT(IN) :: recvtype
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Info), INTENT(IN) :: info
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompix_alltoallv_init_f(sendbuf,sendcounts,sdispls,sendtype%MPI_VAL,&
                               recvbuf,recvcounts,rdispls,recvtype%MPI_VAL,comm%MPI_VAL,info%MPI_VAL,request%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine PMPIX_Alltoallv_init_f08
