! -*- f90 -*-
!
! Copyright (c) 2009-2013 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2013 Los Alamos National Security, LLC.
!                         All rights reserved.
! Copyright (c) 2018      Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! $COPYRIGHT$

#include "ompi/mpi/fortran/configure-fortran-output.h"

subroutine MPIX_Neighbor_alltoallw_init_f08(sendbuf,sendcounts,sdispls,sendtypes,&
                                            recvbuf,recvcounts,rdispls,recvtypes,comm,info,request,ierror)
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Info, MPI_Request, MPI_ADDRESS_KIND
   use :: mpiext_pcollreq_f08, only : ompix_neighbor_alltoallw_init_f
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN), ASYNCHRONOUS :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE, ASYNCHRONOUS :: recvbuf
   INTEGER, INTENT(IN), ASYNCHRONOUS :: sendcounts(*), recvcounts(*)
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN), ASYNCHRONOUS :: sdispls(*), rdispls(*)
   TYPE(MPI_Datatype), INTENT(IN), ASYNCHRONOUS :: sendtypes(*)
   TYPE(MPI_Datatype), INTENT(IN), ASYNCHRONOUS :: recvtypes(*)
   TYPE(MPI_Comm), INTENT(IN) :: comm
   TYPE(MPI_Info), INTENT(IN) :: info
   TYPE(MPI_Request), INTENT(OUT) :: request
   INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   integer :: c_ierror

   call ompix_neighbor_alltoallw_init_f(sendbuf,sendcounts,sdispls,sendtypes(1)%MPI_VAL,&
                                        recvbuf,recvcounts,rdispls,recvtypes(1)%MPI_VAL,&
                                        comm%MPI_VAL,info%MPI_VAL,request%MPI_VAL,c_ierror)
   if (present(ierror)) ierror = c_ierror

end subroutine MPIX_Neighbor_alltoallw_init_f08
