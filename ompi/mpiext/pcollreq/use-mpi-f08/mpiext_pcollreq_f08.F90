! -*- f90 -*-
!
! Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
!                         University Research and Technology
!                         Corporation.  All rights reserved.
! Copyright (c) 2004-2005 The University of Tennessee and The University
!                         of Tennessee Research Foundation.  All rights
!                         reserved.
! Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
!                         University of Stuttgart.  All rights reserved.
! Copyright (c) 2004-2005 The Regents of the University of California.
!                         All rights reserved.
! Copyright (c) 2006-2014 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! Copyright (c) 2016-2018 Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.
! $COPYRIGHT$
!
! Additional copyrights may follow
!
! $HEADER$
!

#include "ompi/mpi/fortran/configure-fortran-output.h"

module mpiext_pcollreq_f08

  use mpi_f08_types
  use mpi_f08_interfaces  ! this module contains the  mpi_f08 interface declarations
  use pmpi_f08_interfaces ! this module contains the pmpi_f08 interface declarations
  use mpi_f08_callbacks   ! this module contains the mpi_f08 attribute callback subroutines
  use mpi_f08_interfaces_callbacks ! this module contains the mpi_f08 callback interfaces

interface

subroutine ompix_allgather_init_f(sendbuf,sendcount,sendtype,recvbuf, &
                                  recvcount,recvtype,comm,info,request,ierror) &
   BIND(C, name="ompix_allgather_init_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: sendcount, recvcount
   INTEGER, INTENT(IN) :: sendtype
   INTEGER, INTENT(IN) :: recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompix_allgather_init_f

subroutine ompix_allgatherv_init_f(sendbuf,sendcount,sendtype,recvbuf, &
                                   recvcounts,displs,recvtype,comm,info,request,ierror) &
   BIND(C, name="ompix_allgatherv_init_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: sendcount
   INTEGER, INTENT(IN) :: recvcounts(*), displs(*)
   INTEGER, INTENT(IN) :: sendtype
   INTEGER, INTENT(IN) :: recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompix_allgatherv_init_f

subroutine ompix_allreduce_init_f(sendbuf,recvbuf,count,datatype,op,comm,info,request,ierror) &
   BIND(C, name="ompix_allreduce_init_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: op
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompix_allreduce_init_f

subroutine ompix_alltoall_init_f(sendbuf,sendcount,sendtype,recvbuf, &
                                 recvcount,recvtype,comm,info,request,ierror) &
   BIND(C, name="ompix_alltoall_init_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: sendcount, recvcount
   INTEGER, INTENT(IN) :: sendtype
   INTEGER, INTENT(IN) :: recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompix_alltoall_init_f

subroutine ompix_alltoallv_init_f(sendbuf,sendcounts,sdispls,sendtype, &
                                  recvbuf,recvcounts,rdispls,recvtype,comm,info,request,ierror) &
   BIND(C, name="ompix_alltoallv_init_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
   INTEGER, INTENT(IN) :: sendtype
   INTEGER, INTENT(IN) :: recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompix_alltoallv_init_f

subroutine ompix_alltoallw_init_f(sendbuf,sendcounts,sdispls,sendtypes, &
                                  recvbuf,recvcounts,rdispls,recvtypes,comm,info,request,ierror) &
   BIND(C, name="ompix_alltoallw_init_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
   INTEGER, INTENT(IN) :: sendtypes
   INTEGER, INTENT(IN) :: recvtypes
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompix_alltoallw_init_f

subroutine ompix_barrier_init_f(comm,info,request,ierror) &
   BIND(C, name="ompix_barrier_init_f")
   implicit none
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompix_barrier_init_f

subroutine ompix_bcast_init_f(buffer,count,datatype,root,comm,info,request,ierror) &
   BIND(C, name="ompix_bcast_init_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: buffer
   INTEGER, INTENT(IN) :: count, root
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompix_bcast_init_f

subroutine ompix_exscan_init_f(sendbuf,recvbuf,count,datatype,op,comm,info,request,ierror) &
   BIND(C, name="ompix_exscan_init_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: op
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompix_exscan_init_f

subroutine ompix_gather_init_f(sendbuf,sendcount,sendtype,recvbuf, &
                               recvcount,recvtype,root,comm,info,request,ierror) &
   BIND(C, name="ompix_gather_init_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: sendcount, recvcount, root
   INTEGER, INTENT(IN) :: sendtype
   INTEGER, INTENT(IN) :: recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompix_gather_init_f

subroutine ompix_gatherv_init_f(sendbuf,sendcount,sendtype,recvbuf, &
                                recvcounts,displs,recvtype,root,comm,info,request,ierror) &
   BIND(C, name="ompix_gatherv_init_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: sendcount, root
   INTEGER, INTENT(IN) :: recvcounts(*), displs(*)
   INTEGER, INTENT(IN) :: sendtype
   INTEGER, INTENT(IN) :: recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompix_gatherv_init_f

subroutine ompix_reduce_init_f(sendbuf,recvbuf,count,datatype,op,root,comm,info,request,ierror) &
   BIND(C, name="ompix_reduce_init_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: count, root
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: op
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompix_reduce_init_f

subroutine ompix_reduce_scatter_init_f(sendbuf,recvbuf,recvcounts, &
                                       datatype,op,comm,info,request,ierror) &
   BIND(C, name="ompix_reduce_scatter_init_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: recvcounts(*)
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: op
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompix_reduce_scatter_init_f

subroutine ompix_reduce_scatter_block_init_f(sendbuf,recvbuf,recvcount, &
                                             datatype,op,comm,info,request,ierror) &
   BIND(C, name="ompix_reduce_scatter_block_init_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: recvcount
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: op
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompix_reduce_scatter_block_init_f

subroutine ompix_scan_init_f(sendbuf,recvbuf,count,datatype,op,comm,info,request,ierror) &
   BIND(C, name="ompix_scan_init_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: count
   INTEGER, INTENT(IN) :: datatype
   INTEGER, INTENT(IN) :: op
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompix_scan_init_f

subroutine ompix_scatter_init_f(sendbuf,sendcount,sendtype,recvbuf, &
                                recvcount,recvtype,root,comm,info,request,ierror) &
   BIND(C, name="ompix_scatter_init_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: sendcount, recvcount, root
   INTEGER, INTENT(IN) :: sendtype
   INTEGER, INTENT(IN) :: recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompix_scatter_init_f

subroutine ompix_scatterv_init_f(sendbuf,sendcounts,displs,sendtype, &
                                 recvbuf,recvcount,recvtype,root,comm,info,request,ierror) &
   BIND(C, name="ompix_scatterv_init_f")
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf, recvbuf
   INTEGER, INTENT(IN) :: recvcount, root
   INTEGER, INTENT(IN) :: sendcounts(*), displs(*)
   INTEGER, INTENT(IN) :: sendtype
   INTEGER, INTENT(IN) :: recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompix_scatterv_init_f

subroutine ompix_neighbor_allgather_init_f(sendbuf,sendcount,sendtype,recvbuf,recvcount,recvtype, &
                                           comm,info,request,ierror) &
   BIND(C, name="ompix_neighbor_allgather_init_f")
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcount, recvcount
   INTEGER, INTENT(IN) :: sendtype, recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompix_neighbor_allgather_init_f

subroutine ompix_neighbor_allgatherv_init_f(sendbuf,sendcount,sendtype,recvbuf,recvcounts,displs, &
                                            recvtype,comm,info,request,ierror) &
   BIND(C, name="ompix_neighbor_allgatherv_init_f")
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcount
   INTEGER, INTENT(IN) :: recvcounts(*), displs(*)
   INTEGER, INTENT(IN) :: sendtype, recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompix_neighbor_allgatherv_init_f

subroutine ompix_neighbor_alltoall_init_f(sendbuf,sendcount,sendtype,recvbuf,recvcount,recvtype, &
                                          comm,info,request,ierror) &
   BIND(C, name="ompix_neighbor_alltoall_init_f")
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcount, recvcount
   INTEGER, INTENT(IN) :: sendtype, recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompix_neighbor_alltoall_init_f

subroutine ompix_neighbor_alltoallv_init_f(sendbuf,sendcounts,sdispls,sendtype,recvbuf,recvcounts, &
                                           rdispls,recvtype,comm,info,request,ierror) &
   BIND(C, name="ompix_neighbor_alltoallv_init_f")
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
   INTEGER, INTENT(IN) :: sendtype, recvtype
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompix_neighbor_alltoallv_init_f

subroutine ompix_neighbor_alltoallw_init_f(sendbuf,sendcounts,sdispls,sendtypes,recvbuf,recvcounts, &
                                           rdispls,recvtypes,comm,info,request,ierror) &
  BIND(C, name="ompix_neighbor_alltoallw_init_f")
   use :: mpi_f08_types, only : MPI_Datatype, MPI_Comm, MPI_Request, MPI_ADDRESS_KIND
   implicit none
   OMPI_FORTRAN_IGNORE_TKR_TYPE, INTENT(IN) :: sendbuf
   OMPI_FORTRAN_IGNORE_TKR_TYPE :: recvbuf
   INTEGER, INTENT(IN) :: sendcounts(*), recvcounts(*)
   INTEGER(MPI_ADDRESS_KIND), INTENT(IN) :: sdispls(*), rdispls(*)
   INTEGER, INTENT(IN) :: sendtypes, recvtypes
   INTEGER, INTENT(IN) :: comm
   INTEGER, INTENT(IN) :: info
   INTEGER, INTENT(OUT) :: request
   INTEGER, INTENT(OUT) :: ierror
end subroutine ompix_neighbor_alltoallw_init_f

end interface

end module mpiext_pcollreq_f08
