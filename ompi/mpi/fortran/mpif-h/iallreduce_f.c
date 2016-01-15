/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/mpi/fortran/base/constants.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_IALLREDUCE = ompi_iallreduce_f
#pragma weak pmpi_iallreduce = ompi_iallreduce_f
#pragma weak pmpi_iallreduce_ = ompi_iallreduce_f
#pragma weak pmpi_iallreduce__ = ompi_iallreduce_f

#pragma weak PMPI_Iallreduce_f = ompi_iallreduce_f
#pragma weak PMPI_Iallreduce_f08 = ompi_iallreduce_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_IALLREDUCE,
                            pmpi_iallreduce,
                            pmpi_iallreduce_,
                            pmpi_iallreduce__,
                            pompi_iallreduce_f,
                            (char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                            (sendbuf, recvbuf, count, datatype, op, comm, request, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_IALLREDUCE = ompi_iallreduce_f
#pragma weak mpi_iallreduce = ompi_iallreduce_f
#pragma weak mpi_iallreduce_ = ompi_iallreduce_f
#pragma weak mpi_iallreduce__ = ompi_iallreduce_f

#pragma weak MPI_Iallreduce_f = ompi_iallreduce_f
#pragma weak MPI_Iallreduce_f08 = ompi_iallreduce_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_IALLREDUCE,
                            mpi_iallreduce,
                            mpi_iallreduce_,
                            mpi_iallreduce__,
                            ompi_iallreduce_f,
                            (char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                            (sendbuf, recvbuf, count, datatype, op, comm, request, ierr) )
#else
#define ompi_iallreduce_f pompi_iallreduce_f
#endif
#endif


void ompi_iallreduce_f(char *sendbuf, char *recvbuf, MPI_Fint *count,
                       MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm,
                       MPI_Fint *request, MPI_Fint *ierr)
{
    int ierr_c;
    MPI_Comm c_comm;
    MPI_Datatype c_type;
    MPI_Request c_request;
    MPI_Op c_op;

    c_comm = PMPI_Comm_f2c(*comm);
    c_type = PMPI_Type_f2c(*datatype);
    c_op = PMPI_Op_f2c(*op);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    ierr_c = PMPI_Iallreduce(sendbuf, recvbuf,
                             OMPI_FINT_2_INT(*count),
                             c_type, c_op, c_comm, &c_request);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(ierr_c);
    if (MPI_SUCCESS == ierr_c) *request = PMPI_Request_c2f(c_request);
}
