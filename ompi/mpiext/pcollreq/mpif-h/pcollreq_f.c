/*
 * Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/mpi/fortran/base/constants.h"
#include "ompi/mpiext/pcollreq/c/mpiext_pcollreq_c.h"

/* Prototype everything so that the compiler doesn't complain */
/* MPIX_ALLGATHER_INIT */
OMPI_DECLSPEC void mpix_allgather_init_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void MPIX_ALLGATHER_INIT(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_allgather_init(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_allgather_init_(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_allgather_init__(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
/* MPIX_ALLGATHERV_INIT */
OMPI_DECLSPEC void mpix_allgatherv_init_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void MPIX_ALLGATHERV_INIT(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_allgatherv_init(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_allgatherv_init_(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_allgatherv_init__(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
/* MPIX_ALLREDUCE_INIT */
OMPI_DECLSPEC void mpix_allreduce_init_f(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void MPIX_ALLREDUCE_INIT(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_allreduce_init(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_allreduce_init_(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_allreduce_init__(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
/* MPIX_ALLTOALL_INIT */
OMPI_DECLSPEC void mpix_alltoall_init_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void MPIX_ALLTOALL_INIT(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_alltoall_init(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_alltoall_init_(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_alltoall_init__(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
/* MPIX_ALLTOALLV_INIT */
OMPI_DECLSPEC void mpix_alltoallv_init_f(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void MPIX_ALLTOALLV_INIT(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_alltoallv_init(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_alltoallv_init_(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_alltoallv_init__(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
/* MPIX_ALLTOALLW_INIT */
OMPI_DECLSPEC void mpix_alltoallw_init_f(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtypes, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtypes, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void MPIX_ALLTOALLW_INIT(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtypes, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtypes, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_alltoallw_init(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtypes, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtypes, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_alltoallw_init_(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtypes, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtypes, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_alltoallw_init__(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtypes, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtypes, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
/* MPIX_BARRIER_INIT */
OMPI_DECLSPEC void mpix_barrier_init_f(MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void MPIX_BARRIER_INIT(MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_barrier_init(MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_barrier_init_(MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_barrier_init__(MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
/* MPIX_BCAST_INIT */
OMPI_DECLSPEC void mpix_bcast_init_f(char *buffer, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void MPIX_BCAST_INIT(char *buffer, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_bcast_init(char *buffer, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_bcast_init_(char *buffer, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_bcast_init__(char *buffer, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
/* MPIX_EXSCAN_INIT */
OMPI_DECLSPEC void mpix_exscan_init_f(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void MPIX_EXSCAN_INIT(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_exscan_init(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_exscan_init_(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_exscan_init__(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
/* MPIX_GATHER_INIT */
OMPI_DECLSPEC void mpix_gather_init_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void MPIX_GATHER_INIT(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_gather_init(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_gather_init_(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_gather_init__(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
/* MPIX_GATHERV_INIT */
OMPI_DECLSPEC void mpix_gatherv_init_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void MPIX_GATHERV_INIT(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_gatherv_init(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_gatherv_init_(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_gatherv_init__(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
/* MPIX_REDUCE_INIT */
OMPI_DECLSPEC void mpix_reduce_init_f(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void MPIX_REDUCE_INIT(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_reduce_init(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_reduce_init_(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_reduce_init__(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
/* MPIX_REDUCE_SCATTER_INIT */
OMPI_DECLSPEC void mpix_reduce_scatter_init_f(char *sendbuf, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void MPIX_REDUCE_SCATTER_INIT(char *sendbuf, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_reduce_scatter_init(char *sendbuf, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_reduce_scatter_init_(char *sendbuf, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_reduce_scatter_init__(char *sendbuf, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
/* MPIX_REDUCE_SCATTER_BLOCK_INIT */
OMPI_DECLSPEC void mpix_reduce_scatter_block_init_f(char *sendbuf, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void MPIX_REDUCE_SCATTER_BLOCK_INIT(char *sendbuf, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_reduce_scatter_block_init(char *sendbuf, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_reduce_scatter_block_init_(char *sendbuf, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_reduce_scatter_block_init__(char *sendbuf, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
/* MPIX_SCAN_INIT */
OMPI_DECLSPEC void mpix_scan_init_f(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void MPIX_SCAN_INIT(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_scan_init(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_scan_init_(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_scan_init__(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
/* MPIX_SCATTER_INIT */
OMPI_DECLSPEC void mpix_scatter_init_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void MPIX_SCATTER_INIT(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_scatter_init(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_scatter_init_(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_scatter_init__(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
/* MPIX_SCATTERV_INIT */
OMPI_DECLSPEC void mpix_scatterv_init_f(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *rdispls, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void MPIX_SCATTERV_INIT(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *rdispls, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_scatterv_init(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *rdispls, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_scatterv_init_(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *rdispls, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_scatterv_init__(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *rdispls, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
/* MPIX_NEIGHBOR_ALLGATHER_INIT */
OMPI_DECLSPEC void mpix_neighbor_allgather_init_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void MPIX_NEIGHBOR_ALLGATHER_INIT(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_neighbor_allgather_init(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_neighbor_allgather_init_(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_neighbor_allgather_init__(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
/* MPIX_NEIGHBOR_ALLGATHERV_INIT */
OMPI_DECLSPEC void mpix_neighbor_allgatherv_init_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void MPIX_NEIGHBOR_ALLGATHERV_INIT(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_neighbor_allgatherv_init(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_neighbor_allgatherv_init_(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_neighbor_allgatherv_init__(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
/* MPIX_NEIGHBOR_ALLTOALL_INIT */
OMPI_DECLSPEC void mpix_neighbor_alltoall_init_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void MPIX_NEIGHBOR_ALLTOALL_INIT(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_neighbor_alltoall_init(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_neighbor_alltoall_init_(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_neighbor_alltoall_init__(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
/* MPIX_NEIGHBOR_ALLTOALLV_INIT */
OMPI_DECLSPEC void mpix_neighbor_alltoallv_init_f(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void MPIX_NEIGHBOR_ALLTOALLV_INIT(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_neighbor_alltoallv_init(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_neighbor_alltoallv_init_(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_neighbor_alltoallv_init__(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
/* MPIX_NEIGHBOR_ALLTOALLW_INIT */
OMPI_DECLSPEC void mpix_neighbor_alltoallw_init_f(char *sendbuf, MPI_Fint *sendcounts, MPI_Aint *sdispls, MPI_Fint *sendtypes, char *recvbuf, MPI_Fint *recvcounts, MPI_Aint *rdispls, MPI_Fint *recvtypes, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void MPIX_NEIGHBOR_ALLTOALLW_INIT(char *sendbuf, MPI_Fint *sendcounts, MPI_Aint *sdispls, MPI_Fint *sendtypes, char *recvbuf, MPI_Fint *recvcounts, MPI_Aint *rdispls, MPI_Fint *recvtypes, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_neighbor_alltoallw_init(char *sendbuf, MPI_Fint *sendcounts, MPI_Aint *sdispls, MPI_Fint *sendtypes, char *recvbuf, MPI_Fint *recvcounts, MPI_Aint *rdispls, MPI_Fint *recvtypes, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_neighbor_alltoallw_init_(char *sendbuf, MPI_Fint *sendcounts, MPI_Aint *sdispls, MPI_Fint *sendtypes, char *recvbuf, MPI_Fint *recvcounts, MPI_Aint *rdispls, MPI_Fint *recvtypes, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);
OMPI_DECLSPEC void mpix_neighbor_alltoallw_init__(char *sendbuf, MPI_Fint *sendcounts, MPI_Aint *sdispls, MPI_Fint *sendtypes, char *recvbuf, MPI_Fint *recvcounts, MPI_Aint *rdispls, MPI_Fint *recvtypes, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr);


/* Back-end function */
/* MPIX_ALLGATHER_INIT */
void mpix_allgather_init_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype,
                           char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype,
                           MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    MPI_Datatype c_sendtype, c_recvtype;
    MPI_Info c_info;
    MPI_Request c_req;

    c_comm = MPI_Comm_f2c(*comm);
    c_sendtype = MPI_Type_f2c(*sendtype);
    c_recvtype = MPI_Type_f2c(*recvtype);
    c_info = MPI_Info_f2c(*info);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = MPIX_Allgather_init(sendbuf,
                                 OMPI_FINT_2_INT(*sendcount),
                                 c_sendtype,
                                 recvbuf,
                                 OMPI_FINT_2_INT(*recvcount),
                                 c_recvtype,
                                 c_comm,
                                 c_info,
                                 &c_req);

    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *request = MPI_Request_c2f(c_req);
    }
}

/* MPIX_ALLGATHERV_INIT */
void mpix_allgatherv_init_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype,
                            char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype,
                            MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    MPI_Datatype c_sendtype, c_recvtype;
    MPI_Info c_info;
    MPI_Request c_req;
    int size, c_ierr;
    OMPI_ARRAY_NAME_DECL(recvcounts);
    OMPI_ARRAY_NAME_DECL(displs);

    c_comm = MPI_Comm_f2c(*comm);
    c_sendtype = MPI_Type_f2c(*sendtype);
    c_recvtype = MPI_Type_f2c(*recvtype);
    c_info = MPI_Info_f2c(*info);

    MPI_Comm_size(c_comm, &size);
    OMPI_ARRAY_FINT_2_INT(recvcounts, size);
    OMPI_ARRAY_FINT_2_INT(displs, size);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = MPIX_Allgatherv_init(sendbuf,
                                  OMPI_FINT_2_INT(*sendcount),
                                  c_sendtype,
                                  recvbuf,
                                  OMPI_ARRAY_NAME_CONVERT(recvcounts),
                                  OMPI_ARRAY_NAME_CONVERT(displs),
                                  c_recvtype,
                                  c_comm,
                                  c_info,
                                  &c_req);

    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *request = MPI_Request_c2f(c_req);
    }

    OMPI_ARRAY_FINT_2_INT_CLEANUP(recvcounts);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(displs);
}

/* MPIX_ALLREDUCE_INIT */
void mpix_allreduce_init_f(char *sendbuf, char *recvbuf, MPI_Fint *count,
                           MPI_Fint *datatype, MPI_Fint *op,
                           MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    MPI_Datatype c_type;
    MPI_Op c_op;
    MPI_Info c_info;
    MPI_Request c_req;

    c_comm = MPI_Comm_f2c(*comm);
    c_type = MPI_Type_f2c(*datatype);
    c_op = MPI_Op_f2c(*op);
    c_info = MPI_Info_f2c(*info);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = MPIX_Allreduce_init(sendbuf,
                                 recvbuf,
                                 OMPI_FINT_2_INT(*count),
                                 c_type,
                                 c_op,
                                 c_comm,
                                 c_info,
                                 &c_req);

    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *request = MPI_Request_c2f(c_req);
    }
}

/* MPI_ALLTOALL_INIT */
void mpix_alltoall_init_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype,
                          char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype,
                          MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    MPI_Datatype c_sendtype, c_recvtype;
    MPI_Info c_info;
    MPI_Request c_req;

    c_comm = MPI_Comm_f2c(*comm);
    c_sendtype = MPI_Type_f2c(*sendtype);
    c_recvtype = MPI_Type_f2c(*recvtype);
    c_info = MPI_Info_f2c(*info);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = MPIX_Alltoall_init(sendbuf,
                                OMPI_FINT_2_INT(*sendcount),
                                c_sendtype,
                                recvbuf,
                                OMPI_FINT_2_INT(*recvcount),
                                c_recvtype,
                                c_comm,
                                c_info,
                                &c_req);

    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *request = MPI_Request_c2f(c_req);
    }
}

/* MPI_ALLTOALLV_INIT */
void mpix_alltoallv_init_f(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtype,
                           char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype,
                           MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    MPI_Datatype c_sendtype, c_recvtype;
    MPI_Info c_info;
    MPI_Request c_req;
    int size, c_ierr;
    OMPI_ARRAY_NAME_DECL(sendcounts);
    OMPI_ARRAY_NAME_DECL(sdispls);
    OMPI_ARRAY_NAME_DECL(recvcounts);
    OMPI_ARRAY_NAME_DECL(rdispls);

    c_comm = MPI_Comm_f2c(*comm);
    c_sendtype = MPI_Type_f2c(*sendtype);
    c_recvtype = MPI_Type_f2c(*recvtype);
    c_info = MPI_Info_f2c(*info);

    MPI_Comm_size(c_comm, &size);
    OMPI_ARRAY_FINT_2_INT(sendcounts, size);
    OMPI_ARRAY_FINT_2_INT(sdispls, size);
    OMPI_ARRAY_FINT_2_INT(recvcounts, size);
    OMPI_ARRAY_FINT_2_INT(rdispls, size);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = MPIX_Alltoallv_init(sendbuf,
                                 OMPI_ARRAY_NAME_CONVERT(sendcounts),
                                 OMPI_ARRAY_NAME_CONVERT(sdispls),
                                 c_sendtype,
                                 recvbuf,
                                 OMPI_ARRAY_NAME_CONVERT(recvcounts),
                                 OMPI_ARRAY_NAME_CONVERT(rdispls),
                                 c_recvtype,
                                 c_comm,
                                 c_info,
                                 &c_req);

    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *request = MPI_Request_c2f(c_req);
    }

    OMPI_ARRAY_FINT_2_INT_CLEANUP(sendcounts);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(sdispls);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(recvcounts);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(rdispls);
}

/* MPI_ALLTOALLW_INIT */
void mpix_alltoallw_init_f(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtypes,
                           char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtypes,
                           MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    MPI_Datatype *c_sendtypes, *c_recvtypes;
    MPI_Info c_info;
    MPI_Request c_req;
    int size, c_ierr;
    OMPI_ARRAY_NAME_DECL(sendcounts);
    OMPI_ARRAY_NAME_DECL(sdispls);
    OMPI_ARRAY_NAME_DECL(recvcounts);
    OMPI_ARRAY_NAME_DECL(rdispls);

    c_comm = MPI_Comm_f2c(*comm);
    MPI_Comm_size(c_comm, &size);
    c_info = MPI_Info_f2c(*info);

    c_sendtypes = (MPI_Datatype *) malloc(size * sizeof(MPI_Datatype));
    c_recvtypes = (MPI_Datatype *) malloc(size * sizeof(MPI_Datatype));

    OMPI_ARRAY_FINT_2_INT(sendcounts, size);
    OMPI_ARRAY_FINT_2_INT(sdispls, size);
    OMPI_ARRAY_FINT_2_INT(recvcounts, size);
    OMPI_ARRAY_FINT_2_INT(rdispls, size);

    while (size > 0) {
        c_sendtypes[size - 1] = MPI_Type_f2c(sendtypes[size - 1]);
        c_recvtypes[size - 1] = MPI_Type_f2c(recvtypes[size - 1]);
        --size;
    }

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = MPIX_Alltoallw_init(sendbuf,
                                 OMPI_ARRAY_NAME_CONVERT(sendcounts),
                                 OMPI_ARRAY_NAME_CONVERT(sdispls),
                                 c_sendtypes,
                                 recvbuf,
                                 OMPI_ARRAY_NAME_CONVERT(recvcounts),
                                 OMPI_ARRAY_NAME_CONVERT(rdispls),
                                 c_recvtypes,
                                 c_comm,
                                 c_info,
                                 &c_req);

    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *request = MPI_Request_c2f(c_req);
    }

    OMPI_ARRAY_FINT_2_INT_CLEANUP(sendcounts);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(sdispls);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(recvcounts);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(rdispls);
    free(c_sendtypes);
    free(c_recvtypes);
}

/* MPI_BARRIER_INIT */
void mpix_barrier_init_f(MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    MPI_Info c_info;
    MPI_Request c_req;

    c_comm = MPI_Comm_f2c(*comm);
    c_info = MPI_Info_f2c(*info);

    c_ierr = MPIX_Barrier_init(c_comm,
                               c_info,
                               &c_req);

    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *request = MPI_Request_c2f(c_req);
    }
}

/* MPI_BCAST_INIT */
void mpix_bcast_init_f(char *buffer, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    MPI_Datatype c_type;
    MPI_Info c_info;
    MPI_Request c_req;

    c_comm = MPI_Comm_f2c(*comm);
    c_type = MPI_Type_f2c(*datatype);
    c_info = MPI_Info_f2c(*info);

    c_ierr = MPIX_Bcast_init(OMPI_F2C_BOTTOM(buffer),
                             OMPI_FINT_2_INT(*count),
                             c_type,
                             OMPI_FINT_2_INT(*root),
                             c_comm,
                             c_info,
                             &c_req);

    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *request = MPI_Request_c2f(c_req);
    }
}

/* MPI_EXSCAN_INIT */
void mpix_exscan_init_f(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    MPI_Datatype c_type;
    MPI_Op c_op;
    MPI_Info c_info;
    MPI_Request c_req;

    c_comm = MPI_Comm_f2c(*comm);
    c_type = MPI_Type_f2c(*datatype);
    c_op = MPI_Op_f2c(*op);
    c_info = MPI_Info_f2c(*info);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM (sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM (recvbuf);

    c_ierr = MPIX_Exscan_init(sendbuf,
                              recvbuf,
                              OMPI_FINT_2_INT(*count),
                              c_type,
                              c_op,
                              c_comm,
                              c_info,
                              &c_req);

    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *request = MPI_Request_c2f(c_req);
    }
}

/* MPI_GATHER_INIT */
void mpix_gather_init_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype,
                        char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype,
                        MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    MPI_Datatype c_sendtype, c_recvtype;
    MPI_Info c_info;
    MPI_Request c_req;

    c_comm = MPI_Comm_f2c(*comm);
    c_sendtype = MPI_Type_f2c(*sendtype);
    c_recvtype = MPI_Type_f2c(*recvtype);
    c_info = MPI_Info_f2c(*info);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = MPIX_Gather_init(sendbuf,
                              OMPI_FINT_2_INT(*sendcount),
                              c_sendtype,
                              recvbuf,
                              OMPI_FINT_2_INT(*recvcount),
                              c_recvtype,
                              OMPI_FINT_2_INT(*root),
                              c_comm,
                              c_info,
                              &c_req);

    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *request = MPI_Request_c2f(c_req);
    }
}

/* MPI_GATHERV_INIT */
void mpix_gatherv_init_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype,
                         char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype,
                         MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    MPI_Datatype c_sendtype, c_recvtype;
    MPI_Info c_info;
    MPI_Request c_req;
    int size, c_ierr;
    OMPI_ARRAY_NAME_DECL(recvcounts);
    OMPI_ARRAY_NAME_DECL(displs);

    c_comm = MPI_Comm_f2c(*comm);
    c_sendtype = MPI_Type_f2c(*sendtype);
    c_recvtype = MPI_Type_f2c(*recvtype);
    c_info = MPI_Info_f2c(*info);

    MPI_Comm_size(c_comm, &size);
    OMPI_ARRAY_FINT_2_INT(recvcounts, size);
    OMPI_ARRAY_FINT_2_INT(displs, size);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = MPIX_Gatherv_init(sendbuf,
                               OMPI_FINT_2_INT(*sendcount),
                               c_sendtype, recvbuf,
                               OMPI_ARRAY_NAME_CONVERT(recvcounts),
                               OMPI_ARRAY_NAME_CONVERT(displs),
                               c_recvtype,
                               OMPI_FINT_2_INT(*root),
                               c_comm,
                               c_info,
                               &c_req);

    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *request = MPI_Request_c2f(c_req);
    }

    OMPI_ARRAY_FINT_2_INT_CLEANUP(recvcounts);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(rdispls);
}

/* MPI_REDUCE_INIT */
void mpix_reduce_init_f(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *root,
                        MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Datatype c_type;
    MPI_Op c_op;
    MPI_Comm c_comm;
    MPI_Info c_info;
    MPI_Request c_req;

    c_type = MPI_Type_f2c(*datatype);
    c_op = MPI_Op_f2c(*op);
    c_comm = MPI_Comm_f2c(*comm);
    c_info = MPI_Info_f2c(*info);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = MPIX_Reduce_init(sendbuf, recvbuf,
                              OMPI_FINT_2_INT(*count),
                              c_type,
                              c_op,
                              OMPI_FINT_2_INT(*root),
                              c_comm,
                              c_info,
                              &c_req);

    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *request = MPI_Request_c2f(c_req);
    }
}

/* MPI_REDUCE_SCATTER_INIT */
void mpix_reduce_scatter_init_f(char *sendbuf, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *datatype, MPI_Fint *op,
                                MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    MPI_Datatype c_type;
    MPI_Op c_op;
    MPI_Info c_info;
    MPI_Request c_req;
    int size;
    OMPI_ARRAY_NAME_DECL(recvcounts);

    c_comm = MPI_Comm_f2c(*comm);
    c_type = MPI_Type_f2c(*datatype);
    c_op = MPI_Op_f2c(*op);
    c_info = MPI_Info_f2c(*info);

    MPI_Comm_size(c_comm, &size);
    OMPI_ARRAY_FINT_2_INT(recvcounts, size);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = MPIX_Reduce_scatter_init(sendbuf,
                                      recvbuf,
                                      OMPI_ARRAY_NAME_CONVERT(recvcounts),
                                      c_type,
                                      c_op,
                                      c_comm,
                                      c_info,
                                      &c_req);

    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *request = MPI_Request_c2f(c_req);
    }

    OMPI_ARRAY_FINT_2_INT_CLEANUP(recvcounts);
}

/* MPI_REDUCE_SCATTER_BLOCK_INIT */
void mpix_reduce_scatter_block_init_f(char *sendbuf, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *datatype, MPI_Fint *op,
                                      MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    MPI_Datatype c_type;
    MPI_Op c_op;
    MPI_Info c_info;
    MPI_Request c_req;
    int size;

    c_comm = MPI_Comm_f2c(*comm);
    c_type = MPI_Type_f2c(*datatype);
    c_op = MPI_Op_f2c(*op);
    c_info = MPI_Info_f2c(*info);

    MPI_Comm_size(c_comm, &size);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = MPIX_Reduce_scatter_block_init(sendbuf,
                                            recvbuf,
                                            OMPI_FINT_2_INT(*recvcount),
                                            c_type,
                                            c_op,
                                            c_comm,
                                            c_info,
                                            &c_req);

    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *request = MPI_Request_c2f(c_req);
    }
}

/* MPI_SCAN_INIT */
void mpix_scan_init_f(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op,
                      MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    MPI_Datatype c_type;
    MPI_Op c_op;
    MPI_Info c_info;
    MPI_Request c_req;

    c_type = MPI_Type_f2c(*datatype);
    c_op = MPI_Op_f2c(*op);
    c_comm = MPI_Comm_f2c(*comm);
    c_info = MPI_Info_f2c(*info);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = MPIX_Scan_init(sendbuf, recvbuf,
                            OMPI_FINT_2_INT(*count),
                            c_type,
                            c_op,
                            c_comm,
                            c_info,
                            &c_req);

    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *request = MPI_Request_c2f(c_req);
    }
}

/* MPI_SCATTER_INIT */
void mpix_scatter_init_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype,
                         char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype,
                         MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Datatype c_sendtype, c_recvtype;
    MPI_Comm c_comm = MPI_Comm_f2c(*comm);
    MPI_Info c_info;
    MPI_Request c_req;

    c_sendtype = MPI_Type_f2c(*sendtype);
    c_recvtype = MPI_Type_f2c(*recvtype);
    c_info = MPI_Info_f2c(*info);

    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_IN_PLACE(recvbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = MPIX_Scatter_init(sendbuf,
                               OMPI_FINT_2_INT(*sendcount),
                               c_sendtype,
                               recvbuf,
                               OMPI_FINT_2_INT(*recvcount),
                               c_recvtype,
                               OMPI_FINT_2_INT(*root),
                               c_comm,
                               c_info,
                               &c_req);

    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *request = MPI_Request_c2f(c_req);
    }
}

/* MPI_SCATTERV_INIT */
void mpix_scatterv_init_f(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *displs, MPI_Fint *sendtype,
                          char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root,
                          MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    MPI_Datatype c_sendtype, c_recvtype;
    MPI_Info c_info;
    MPI_Request c_req;
    int size, c_ierr;
    OMPI_ARRAY_NAME_DECL(sendcounts);
    OMPI_ARRAY_NAME_DECL(displs);

    c_comm = MPI_Comm_f2c(*comm);
    c_sendtype = MPI_Type_f2c(*sendtype);
    c_recvtype = MPI_Type_f2c(*recvtype);
    c_info = MPI_Info_f2c(*info);

    MPI_Comm_size(c_comm, &size);
    OMPI_ARRAY_FINT_2_INT(sendcounts, size);
    OMPI_ARRAY_FINT_2_INT(displs, size);

    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_IN_PLACE(recvbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = MPIX_Scatterv_init(sendbuf,
                                OMPI_ARRAY_NAME_CONVERT(sendcounts),
                                OMPI_ARRAY_NAME_CONVERT(displs),
                                c_sendtype, recvbuf,
                                OMPI_FINT_2_INT(*recvcount),
                                c_recvtype,
                                OMPI_FINT_2_INT(*root),
                                c_comm,
                                c_info,
                                &c_req);

    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *request = MPI_Request_c2f(c_req);
    }

    OMPI_ARRAY_FINT_2_INT_CLEANUP(sendcounts);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(displs);
}

/* MPI_NEIGHBOR_ALLGATHER_INIT */
void mpix_neighbor_allgather_init_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype,
                                    char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype,
                                    MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    MPI_Datatype c_sendtype, c_recvtype;
    MPI_Info c_info;
    MPI_Request c_req;

    c_comm = MPI_Comm_f2c(*comm);
    c_sendtype = MPI_Type_f2c(*sendtype);
    c_recvtype = MPI_Type_f2c(*recvtype);
    c_info = MPI_Info_f2c(*info);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = MPIX_Neighbor_allgather_init(sendbuf,
                                          OMPI_FINT_2_INT(*sendcount),
                                          c_sendtype,
                                          recvbuf,
                                          OMPI_FINT_2_INT(*recvcount),
                                          c_recvtype,
                                          c_comm,
                                          c_info,
                                          &c_req);

    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *request = MPI_Request_c2f(c_req);
    }
}

/* MPI_NEIGHBOR_ALLGATHERV_INIT */
void mpix_neighbor_allgatherv_init_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype,
                                     char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype,
                                     MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    MPI_Datatype c_sendtype, c_recvtype;
    MPI_Info c_info;
    MPI_Request c_req;
    int size, c_ierr;
    OMPI_ARRAY_NAME_DECL(recvcounts);
    OMPI_ARRAY_NAME_DECL(displs);

    c_comm = MPI_Comm_f2c(*comm);
    c_sendtype = MPI_Type_f2c(*sendtype);
    c_recvtype = MPI_Type_f2c(*recvtype);
    c_info = MPI_Info_f2c(*info);

    MPI_Comm_size(c_comm, &size);
    OMPI_ARRAY_FINT_2_INT(recvcounts, size);
    OMPI_ARRAY_FINT_2_INT(displs, size);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = MPIX_Neighbor_allgatherv_init(sendbuf,
                                           OMPI_FINT_2_INT(*sendcount),
                                           c_sendtype,
                                           recvbuf,
                                           OMPI_ARRAY_NAME_CONVERT(recvcounts),
                                           OMPI_ARRAY_NAME_CONVERT(displs),
                                           c_recvtype,
                                           c_comm,
                                           c_info,
                                           &c_req);


    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *request = MPI_Request_c2f(c_req);
    }

    OMPI_ARRAY_FINT_2_INT_CLEANUP(recvcounts);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(displs);
}

/* MPI_NEIGHBOR_ALLTOALL_INIT */
void mpix_neighbor_alltoall_init_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype,
                                   char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype,
                                   MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    MPI_Datatype c_sendtype, c_recvtype;
    MPI_Info c_info;
    MPI_Request c_req;

    c_comm = MPI_Comm_f2c(*comm);
    c_sendtype = MPI_Type_f2c(*sendtype);
    c_recvtype = MPI_Type_f2c(*recvtype);
    c_info = MPI_Info_f2c(*info);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = MPIX_Neighbor_alltoall_init(sendbuf,
                                         OMPI_FINT_2_INT(*sendcount),
                                         c_sendtype,
                                         recvbuf,
                                         OMPI_FINT_2_INT(*recvcount),
                                         c_recvtype,
                                         c_comm,
                                         c_info,
                                         &c_req);

    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *request = MPI_Request_c2f(c_req);
    }
}

/* MPI_NEIGHBOR_ALLTOALLV_INIT */
void mpix_neighbor_alltoallv_init_f(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtype,
                                    char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype,
                                    MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    MPI_Datatype c_sendtype, c_recvtype;
    MPI_Info c_info;
    MPI_Request c_req;
    int size, c_ierr;
    OMPI_ARRAY_NAME_DECL(sendcounts);
    OMPI_ARRAY_NAME_DECL(sdispls);
    OMPI_ARRAY_NAME_DECL(recvcounts);
    OMPI_ARRAY_NAME_DECL(rdispls);

    c_comm = MPI_Comm_f2c(*comm);
    c_sendtype = MPI_Type_f2c(*sendtype);
    c_recvtype = MPI_Type_f2c(*recvtype);
    c_info = MPI_Info_f2c(*info);

    MPI_Comm_size(c_comm, &size);
    OMPI_ARRAY_FINT_2_INT(sendcounts, size);
    OMPI_ARRAY_FINT_2_INT(sdispls, size);
    OMPI_ARRAY_FINT_2_INT(recvcounts, size);
    OMPI_ARRAY_FINT_2_INT(rdispls, size);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = MPIX_Neighbor_alltoallv_init(sendbuf,
                                          OMPI_ARRAY_NAME_CONVERT(sendcounts),
                                          OMPI_ARRAY_NAME_CONVERT(sdispls),
                                          c_sendtype,
                                          recvbuf,
                                          OMPI_ARRAY_NAME_CONVERT(recvcounts),
                                          OMPI_ARRAY_NAME_CONVERT(rdispls),
                                          c_recvtype,
                                          c_comm,
                                          c_info,
                                          &c_req);

    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *request = MPI_Request_c2f(c_req);
    }

    OMPI_ARRAY_FINT_2_INT_CLEANUP(sendcounts);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(sdispls);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(recvcounts);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(rdispls);
}

/* MPI_NEIGHBOR_ALLTOALLW_INIT */
void mpix_neighbor_alltoallw_init_f(char *sendbuf, MPI_Fint *sendcounts, MPI_Aint *sdispls, MPI_Fint *sendtypes,
                                    char *recvbuf, MPI_Fint *recvcounts, MPI_Aint *rdispls, MPI_Fint *recvtypes,
                                    MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    MPI_Datatype *c_sendtypes, *c_recvtypes;
    MPI_Info c_info;
    MPI_Request c_req;
    int size, c_ierr;
    OMPI_ARRAY_NAME_DECL(sendcounts);
    OMPI_ARRAY_NAME_DECL(recvcounts);

    c_comm = MPI_Comm_f2c(*comm);
    MPI_Comm_size(c_comm, &size);
    c_info = MPI_Info_f2c(*info);

    c_sendtypes = (MPI_Datatype *) malloc(size * sizeof(MPI_Datatype));
    c_recvtypes = (MPI_Datatype *) malloc(size * sizeof(MPI_Datatype));

    OMPI_ARRAY_FINT_2_INT(sendcounts, size);
    OMPI_ARRAY_FINT_2_INT(recvcounts, size);

    while (size > 0) {
        c_sendtypes[size - 1] = MPI_Type_f2c(sendtypes[size - 1]);
        c_recvtypes[size - 1] = MPI_Type_f2c(recvtypes[size - 1]);
        --size;
    }

    /* Alltoallw does not support MPI_IN_PLACE */
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = MPIX_Neighbor_alltoallw_init(sendbuf,
                                          OMPI_ARRAY_NAME_CONVERT(sendcounts),
                                          sdispls,
                                          c_sendtypes,
                                          recvbuf,
                                          OMPI_ARRAY_NAME_CONVERT(recvcounts),
                                          rdispls,
                                          c_recvtypes,
                                          c_comm,
                                          c_info,
                                          &c_req);

    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *request = MPI_Request_c2f(c_req);
    }

    OMPI_ARRAY_FINT_2_INT_CLEANUP(sendcounts);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(recvcounts);
    free(c_sendtypes);
    free(c_recvtypes);
}

/* Front-end functions */
/* MPIX_ALLGATHER_INIT */
void MPIX_ALLGATHER_INIT(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_allgather_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, info, request, ierr);
}
void mpix_allgather_init(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_allgather_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, info, request, ierr);
}
void mpix_allgather_init_(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_allgather_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, info, request, ierr);
}
void mpix_allgather_init__(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_allgather_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, info, request, ierr);
}
/* MPIX_ALLGATHERV_INIT */
void MPIX_ALLGATHERV_INIT(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_allgatherv_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, comm, info, request, ierr);
}
void mpix_allgatherv_init(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_allgatherv_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, comm, info, request, ierr);
}
void mpix_allgatherv_init_(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_allgatherv_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, comm, info, request, ierr);
}
void mpix_allgatherv_init__(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_allgatherv_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, comm, info, request, ierr);
}
/* MPIX_ALLREDUCE_INIT */
void MPIX_ALLREDUCE_INIT(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_allreduce_init_f(sendbuf, recvbuf, count, datatype, op, comm, info, request, ierr);
}
void mpix_allreduce_init(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_allreduce_init_f(sendbuf, recvbuf, count, datatype, op, comm, info, request, ierr);
}
void mpix_allreduce_init_(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_allreduce_init_f(sendbuf, recvbuf, count, datatype, op, comm, info, request, ierr);
}
void mpix_allreduce_init__(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_allreduce_init_f(sendbuf, recvbuf, count, datatype, op, comm, info, request, ierr);
}
/* MPIX_ALLTOALL_INIT */
void MPIX_ALLTOALL_INIT(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_alltoall_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, info, request, ierr);
}
void mpix_alltoall_init(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_alltoall_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, info, request, ierr);
}
void mpix_alltoall_init_(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_alltoall_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, info, request, ierr);
}
void mpix_alltoall_init__(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_alltoall_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, info, request, ierr);
}
/* MPIX_ALLTOALLV_INIT */
void MPIX_ALLTOALLV_INIT(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_alltoallv_init_f(sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts, rdispls, recvtype, comm, info, request, ierr);
}
void mpix_alltoallv_init(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_alltoallv_init_f(sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts, rdispls, recvtype, comm, info, request, ierr);
}
void mpix_alltoallv_init_(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_alltoallv_init_f(sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts, rdispls, recvtype, comm, info, request, ierr);
}
void mpix_alltoallv_init__(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_alltoallv_init_f(sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts, rdispls, recvtype, comm, info, request, ierr);
}
/* MPIX_ALLTOALLW_INIT */
void MPIX_ALLTOALLW_INIT(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtypes, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtypes, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_alltoallw_init_f(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts, rdispls, recvtypes, comm, info, request, ierr);
}
void mpix_alltoallw_init(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtypes, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtypes, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_alltoallw_init_f(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts, rdispls, recvtypes, comm, info, request, ierr);
}
void mpix_alltoallw_init_(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtypes, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtypes, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_alltoallw_init_f(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts, rdispls, recvtypes, comm, info, request, ierr);
}
void mpix_alltoallw_init__(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtypes, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtypes, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_alltoallw_init_f(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts, rdispls, recvtypes, comm, info, request, ierr);
}
/* MPIX_BARRIER_INIT */
void MPIX_BARRIER_INIT(MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_barrier_init_f(comm, info, request, ierr);
}
void mpix_barrier_init(MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_barrier_init_f(comm, info, request, ierr);
}
void mpix_barrier_init_(MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_barrier_init_f(comm, info, request, ierr);
}
void mpix_barrier_init__(MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_barrier_init_f(comm, info, request, ierr);
}
/* MPIX_BCAST_INIT */
void MPIX_BCAST_INIT(char *buffer, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_bcast_init_f(buffer, count, datatype, root, comm, info, request, ierr);
}
void mpix_bcast_init(char *buffer, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_bcast_init_f(buffer, count, datatype, root, comm, info, request, ierr);
}
void mpix_bcast_init_(char *buffer, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_bcast_init_f(buffer, count, datatype, root, comm, info, request, ierr);
}
void mpix_bcast_init__(char *buffer, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_bcast_init_f(buffer, count, datatype, root, comm, info, request, ierr);
}
/* MPIX_EXSCAN_INIT */
void MPIX_EXSCAN_INIT(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_exscan_init_f(sendbuf, recvbuf, count, datatype, op, comm, info, request, ierr);
}
void mpix_exscan_init(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_exscan_init_f(sendbuf, recvbuf, count, datatype, op, comm, info, request, ierr);
}
void mpix_exscan_init_(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_exscan_init_f(sendbuf, recvbuf, count, datatype, op, comm, info, request, ierr);
}
void mpix_exscan_init__(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_exscan_init_f(sendbuf, recvbuf, count, datatype, op, comm, info, request, ierr);
}
/* MPIX_GATHER_INIT */
void MPIX_GATHER_INIT(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_gather_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, info, request, ierr);
}
void mpix_gather_init(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_gather_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, info, request, ierr);
}
void mpix_gather_init_(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_gather_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, info, request, ierr);
}
void mpix_gather_init__(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_gather_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, info, request, ierr);
}
/* MPIX_GATHERV_INIT */
void MPIX_GATHERV_INIT(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_gatherv_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, root, comm, info, request, ierr);
}
void mpix_gatherv_init(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_gatherv_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, root, comm, info, request, ierr);
}
void mpix_gatherv_init_(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_gatherv_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, root, comm, info, request, ierr);
}
void mpix_gatherv_init__(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_gatherv_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, root, comm, info, request, ierr);
}
/* MPIX_REDUCE_INIT */
void MPIX_REDUCE_INIT(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_reduce_init_f(sendbuf, recvbuf, count, datatype, op, root, comm, info, request, ierr);
}
void mpix_reduce_init(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_reduce_init_f(sendbuf, recvbuf, count, datatype, op, root, comm, info, request, ierr);
}
void mpix_reduce_init_(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_reduce_init_f(sendbuf, recvbuf, count, datatype, op, root, comm, info, request, ierr);
}
void mpix_reduce_init__(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_reduce_init_f(sendbuf, recvbuf, count, datatype, op, root, comm, info, request, ierr);
}
/* MPIX_REDUCE_SCATTER_INIT */
void MPIX_REDUCE_SCATTER_INIT(char *sendbuf, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_reduce_scatter_init_f(sendbuf, recvbuf, recvcounts, datatype, op, comm, info, request, ierr);
}
void mpix_reduce_scatter_init(char *sendbuf, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_reduce_scatter_init_f(sendbuf, recvbuf, recvcounts, datatype, op, comm, info, request, ierr);
}
void mpix_reduce_scatter_init_(char *sendbuf, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_reduce_scatter_init_f(sendbuf, recvbuf, recvcounts, datatype, op, comm, info, request, ierr);
}
void mpix_reduce_scatter_init__(char *sendbuf, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_reduce_scatter_init_f(sendbuf, recvbuf, recvcounts, datatype, op, comm, info, request, ierr);
}
/* MPIX_REDUCE_SCATTER_BLOCK_INIT */
void MPIX_REDUCE_SCATTER_BLOCK_INIT(char *sendbuf, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_reduce_scatter_block_init_f(sendbuf, recvbuf, recvcount, datatype, op, comm, info, request, ierr);
}
void mpix_reduce_scatter_block_init(char *sendbuf, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_reduce_scatter_block_init_f(sendbuf, recvbuf, recvcount, datatype, op, comm, info, request, ierr);
}
void mpix_reduce_scatter_block_init_(char *sendbuf, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_reduce_scatter_block_init_f(sendbuf, recvbuf, recvcount, datatype, op, comm, info, request, ierr);
}
void mpix_reduce_scatter_block_init__(char *sendbuf, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_reduce_scatter_block_init_f(sendbuf, recvbuf, recvcount, datatype, op, comm, info, request, ierr);
}
/* MPIX_SCAN_INIT */
void MPIX_SCAN_INIT(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_scan_init_f(sendbuf, recvbuf, count, datatype, op, comm, info, request, ierr);
}
void mpix_scan_init(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_scan_init_f(sendbuf, recvbuf, count, datatype, op, comm, info, request, ierr);
}
void mpix_scan_init_(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_scan_init_f(sendbuf, recvbuf, count, datatype, op, comm, info, request, ierr);
}
void mpix_scan_init__(char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_scan_init_f(sendbuf, recvbuf, count, datatype, op, comm, info, request, ierr);
}
/* MPIX_SCATTER_INIT */
void MPIX_SCATTER_INIT(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_scatter_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, info, request, ierr);
}
void mpix_scatter_init(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_scatter_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, info, request, ierr);
}
void mpix_scatter_init_(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_scatter_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, info, request, ierr);
}
void mpix_scatter_init__(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_scatter_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, info, request, ierr);
}
/* MPIX_SCATTERV_INIT */
void MPIX_SCATTERV_INIT(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *displs, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_scatterv_init_f(sendbuf, sendcounts, displs, sendtype, recvbuf, recvcount, recvtype, root, comm, info, request, ierr);
}
void mpix_scatterv_init(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *displs, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_scatterv_init_f(sendbuf, sendcounts, displs, sendtype, recvbuf, recvcount, recvtype, root, comm, info, request, ierr);
}
void mpix_scatterv_init_(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *displs, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_scatterv_init_f(sendbuf, sendcounts, displs, sendtype, recvbuf, recvcount, recvtype, root, comm, info, request, ierr);
}
void mpix_scatterv_init__(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *displs, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_scatterv_init_f(sendbuf, sendcounts, displs, sendtype, recvbuf, recvcount, recvtype, root, comm, info, request, ierr);
}
/* MPIX_NEIGHBOR_ALLGATHER_INIT */
void MPIX_NEIGHBOR_ALLGATHER_INIT(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_neighbor_allgather_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, info, request, ierr);
}
void mpix_neighbor_allgather_init(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_neighbor_allgather_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, info, request, ierr);
}
void mpix_neighbor_allgather_init_(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_neighbor_allgather_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, info, request, ierr);
}
void mpix_neighbor_allgather_init__(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_neighbor_allgather_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, info, request, ierr);
}
/* MPIX_NEIGHBOR_ALLGATHERV_INIT */
void MPIX_NEIGHBOR_ALLGATHERV_INIT(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_neighbor_allgatherv_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, comm, info, request, ierr);
}
void mpix_neighbor_allgatherv_init(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_neighbor_allgatherv_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, comm, info, request, ierr);
}
void mpix_neighbor_allgatherv_init_(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_neighbor_allgatherv_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, comm, info, request, ierr);
}
void mpix_neighbor_allgatherv_init__(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_neighbor_allgatherv_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, comm, info, request, ierr);
}
/* MPIX_NEIGHBOR_ALLTOALL_INIT */
void MPIX_NEIGHBOR_ALLTOALL_INIT(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_neighbor_alltoall_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, info, request, ierr);
}
void mpix_neighbor_alltoall_init(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_neighbor_alltoall_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, info, request, ierr);
}
void mpix_neighbor_alltoall_init_(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_neighbor_alltoall_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, info, request, ierr);
}
void mpix_neighbor_alltoall_init__(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_neighbor_alltoall_init_f(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, info, request, ierr);
}
/* MPIX_NEIGHBOR_ALLTOALLV_INIT */
void MPIX_NEIGHBOR_ALLTOALLV_INIT(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_neighbor_alltoallv_init_f(sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts, rdispls, recvtype, comm, info, request, ierr);
}
void mpix_neighbor_alltoallv_init(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_neighbor_alltoallv_init_f(sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts, rdispls, recvtype, comm, info, request, ierr);
}
void mpix_neighbor_alltoallv_init_(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_neighbor_alltoallv_init_f(sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts, rdispls, recvtype, comm, info, request, ierr);
}
void mpix_neighbor_alltoallv_init__(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_neighbor_alltoallv_init_f(sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts, rdispls, recvtype, comm, info, request, ierr);
}
/* MPIX_NEIGHBOR_ALLTOALLW_INIT */
void MPIX_NEIGHBOR_ALLTOALLW_INIT(char *sendbuf, MPI_Fint *sendcounts, MPI_Aint *sdispls, MPI_Fint *sendtypes, char *recvbuf, MPI_Fint *recvcounts, MPI_Aint *rdispls, MPI_Fint *recvtypes, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_neighbor_alltoallw_init_f(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts, rdispls, recvtypes, comm, info, request, ierr);
}
void mpix_neighbor_alltoallw_init(char *sendbuf, MPI_Fint *sendcounts, MPI_Aint *sdispls, MPI_Fint *sendtypes, char *recvbuf, MPI_Fint *recvcounts, MPI_Aint *rdispls, MPI_Fint *recvtypes, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_neighbor_alltoallw_init_f(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts, rdispls, recvtypes, comm, info, request, ierr);
}
void mpix_neighbor_alltoallw_init_(char *sendbuf, MPI_Fint *sendcounts, MPI_Aint *sdispls, MPI_Fint *sendtypes, char *recvbuf, MPI_Fint *recvcounts, MPI_Aint *rdispls, MPI_Fint *recvtypes, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_neighbor_alltoallw_init_f(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts, rdispls, recvtypes, comm, info, request, ierr);
}
void mpix_neighbor_alltoallw_init__(char *sendbuf, MPI_Fint *sendcounts, MPI_Aint *sdispls, MPI_Fint *sendtypes, char *recvbuf, MPI_Fint *recvcounts, MPI_Aint *rdispls, MPI_Fint *recvtypes, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr){
    mpix_neighbor_alltoallw_init_f(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts, rdispls, recvtypes, comm, info, request, ierr);
}
