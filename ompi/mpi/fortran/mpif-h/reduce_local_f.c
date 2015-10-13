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
 * Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
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
#pragma weak PMPI_REDUCE_LOCAL = ompi_reduce_local_f
#pragma weak pmpi_reduce_local = ompi_reduce_local_f
#pragma weak pmpi_reduce_local_ = ompi_reduce_local_f
#pragma weak pmpi_reduce_local__ = ompi_reduce_local_f

#pragma weak PMPI_Reduce_local_f = ompi_reduce_local_f
#pragma weak PMPI_Reduce_local_f08 = ompi_reduce_local_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_REDUCE_LOCAL,
                           pmpi_reduce_local,
                           pmpi_reduce_local_,
                           pmpi_reduce_local__,
                           pompi_reduce_local_f,
                           (char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *ierr),
                           (sendbuf, recvbuf, count, datatype, op, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_REDUCE_LOCAL = ompi_reduce_local_f
#pragma weak mpi_reduce_local = ompi_reduce_local_f
#pragma weak mpi_reduce_local_ = ompi_reduce_local_f
#pragma weak mpi_reduce_local__ = ompi_reduce_local_f

#pragma weak MPI_Reduce_local_f = ompi_reduce_local_f
#pragma weak MPI_Reduce_local_f08 = ompi_reduce_local_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_REDUCE_LOCAL,
                           mpi_reduce_local,
                           mpi_reduce_local_,
                           mpi_reduce_local__,
                           ompi_reduce_local_f,
                           (char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *ierr),
                           (sendbuf, recvbuf, count, datatype, op, ierr) )
#else
#define ompi_reduce_local_f pompi_reduce_local_f
#endif
#endif


void ompi_reduce_local_f(char *inbuf, char *inoutbuf, MPI_Fint *count,
                        MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Datatype c_type;
    MPI_Op c_op;

    c_type = PMPI_Type_f2c(*datatype);
    c_op = PMPI_Op_f2c(*op);

    inbuf = (char *) OMPI_F2C_BOTTOM(inbuf);
    inoutbuf = (char *) OMPI_F2C_BOTTOM(inoutbuf);

    c_ierr = PMPI_Reduce_local(inbuf, inoutbuf,
                              OMPI_FINT_2_INT(*count),
                              c_type, c_op);
   if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
