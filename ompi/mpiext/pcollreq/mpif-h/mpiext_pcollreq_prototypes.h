/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 Inria.  All rights reserved.
 * Copyright (c) 2011-2013 Universite Bordeaux 1
 * Copyright (c) 2013-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2016-2018 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * This file prototypes all MPI fortran functions in all four fortran
 * symbol conventions as well as all the internal real OMPI wrapper
 * functions (different from any of the four fortran symbol
 * conventions for clarity, at the cost of more typing for me...).
 * This file is included in the top-level build ONLY. The prototyping
 * is done ONLY for MPI_* bindings
 *
 * Zeroth, the OMPI wrapper functions, with a ompi_ prefix and _f
 * suffix.
 *
 * This is needed ONLY if the lower-level prototypes_pmpi.h has not
 * already been included.
 *
 * Note about function pointers: all function pointers are prototyped
 * here as (void*) rather than including the .h file that defines the
 * proper type (e.g., "op/op.h" defines ompi_op_fortran_handler_fn_t,
 * which is the function pointer type for fortran op callback
 * functions).  This is because there is no type checking coming in
 * from fortran, so why bother?  Also, including "op/op.h" (and
 * friends) makes the all the f77 bindings files dependant on these
 * files -- any change to any one of them will cause the recompilation
 * of the entire set of f77 bindings (ugh!).
 */

#include "ompi_config.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/attribute/attribute.h"
#include "ompi/op/op.h"
#include "ompi/request/grequest.h"
#include "ompi/mpi/fortran/base/datarep.h"

#include "ompi/mpiext/pcollreq/c/mpiext_pcollreq_c.h"

BEGIN_C_DECLS

/* These are the prototypes for the "real" back-end fortran functions. */
#define PN2(ret, mixed_name, lower_name, upper_name, args) \
    /* Prototype the actual OMPI function */               \
    OMPI_DECLSPEC ret o##lower_name##_f args;              \
    /* Prototype the 4 versions of the MPI mpif.h name */  \
    OMPI_DECLSPEC ret lower_name args;                     \
    OMPI_DECLSPEC ret lower_name##_ args;                  \
    OMPI_DECLSPEC ret lower_name##__ args;                 \
    OMPI_DECLSPEC ret upper_name args;                     \
    /* Prototype the use mpi/use mpi_f08 names  */         \
    OMPI_DECLSPEC ret mixed_name##_f08 args;               \
    OMPI_DECLSPEC ret mixed_name##_f args;                 \
    /* Prototype the actual POMPI function */              \
    OMPI_DECLSPEC ret po##lower_name##_f args;             \
    /* Prototype the 4 versions of the PMPI mpif.h name */ \
    OMPI_DECLSPEC ret p##lower_name args;                  \
    OMPI_DECLSPEC ret p##lower_name##_ args;               \
    OMPI_DECLSPEC ret p##lower_name##__ args;              \
    OMPI_DECLSPEC ret P##upper_name args;                  \
    /* Prototype the use mpi/use mpi_f08 PMPI names  */    \
    OMPI_DECLSPEC ret P##mixed_name##_f08 args;            \
    OMPI_DECLSPEC ret P##mixed_name##_f args

PN2(void, MPIX_Allgather_init, mpix_allgather_init, MPIX_ALLGATHER_INIT, (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr));
PN2(void, MPIX_Allgatherv_init, mpix_allgatherv_init, MPIX_ALLGATHERV_INIT, (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr));
END_C_DECLS
PN2(void, MPIX_Allreduce_init, mpix_allreduce_init, MPIX_ALLREDUCE_INIT, (char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr));
PN2(void, MPIX_Alltoall_init, mpix_alltoall_init, MPIX_ALLTOALL_INIT, (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr));
PN2(void, MPIX_Alltoallv_init, mpix_alltoallv_init, MPIX_ALLTOALLV_INIT, (char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr));
PN2(void, MPIX_Alltoallw_init, mpix_alltoallw_init, MPIX_ALLTOALLW_INIT, (char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtypes, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtypes, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr));
PN2(void, MPIX_Barrier_init, mpix_barrier_init, MPIX_BARRIER_INIT, (MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr));
PN2(void, MPIX_Bcast_init, mpix_bcast_init, MPIX_BCAST_INIT, (char *buffer, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr));
PN2(void, MPIX_Exscan_init, mpix_exscan_init, MPIX_EXSCAN_INIT, (char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr));
PN2(void, MPIX_Gather_init, mpix_gather_init, MPIX_GATHER_INIT, (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr));
PN2(void, MPIX_Gatherv_init, mpix_gatherv_init, MPIX_GATHERV_INIT, (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr));
PN2(void, MPIX_Reduce_init, mpix_reduce_init, MPIX_REDUCE_INIT, (char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr));
PN2(void, MPIX_Reduce_scatter_init, mpix_reduce_scatter_init, MPIX_REDUCE_SCATTER_INIT, (char *sendbuf, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr));
PN2(void, MPIX_Reduce_scatter_block_init, mpix_reduce_scatter_block_init, MPIX_REDUCE_SCATTER_BLOCK_INIT, (char *sendbuf, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr));
PN2(void, MPIX_Scan_init, mpix_scan_init, MPIX_SCAN_INIT, (char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr));
PN2(void, MPIX_Scatter_init, mpix_scatter_init, MPIX_SCATTER_INIT, (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr));
PN2(void, MPIX_Scatterv_init, mpix_scatterv_init, MPIX_SCATTERV_INIT, (char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *displs, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr));
PN2(void, MPIX_Neighbor_allgather_init, mpix_neighbor_allgather_init, MPIX_NEIGHBOR_ALLGATHER_INIT, (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr));
PN2(void, MPIX_Neighbor_allgatherv_init, mpix_neighbor_allgatherv_init, MPIX_NEIGHBOR_ALLGATHERV_INIT, (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr));
PN2(void, MPIX_Neighbor_alltoall_init, mpix_neighbor_alltoall_init, MPIX_NEIGHBOR_ALLTOALL_INIT, (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr));
PN2(void, MPIX_Neighbor_alltoallv_init, mpix_neighbor_alltoallv_init, MPIX_NEIGHBOR_ALLTOALLV_INIT, (char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr));
PN2(void, MPIX_Neighbor_alltoallw_init, mpix_neighbor_alltoallw_init, MPIX_NEIGHBOR_ALLTOALLW_INIT, (char *sendbuf, MPI_Fint *sendcounts, MPI_Aint *sdispls, MPI_Fint *sendtypes, char *recvbuf, MPI_Fint *recvcounts, MPI_Aint *rdispls, MPI_Fint *recvtypes, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr));
