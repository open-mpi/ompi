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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/mpi/fortran/base/constants.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_REDUCE_LOCAL = ompi_reduce_local_f
#pragma weak pmpi_reduce_local = ompi_reduce_local_f
#pragma weak pmpi_reduce_local_ = ompi_reduce_local_f
#pragma weak pmpi_reduce_local__ = ompi_reduce_local_f

#pragma weak PMPI_Reduce_local_f = ompi_reduce_local_f
#pragma weak PMPI_Reduce_local_f08 = ompi_reduce_local_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_REDUCE_LOCAL,
                           pmpi_reduce_local,
                           pmpi_reduce_local_,
                           pmpi_reduce_local__,
                           pompi_reduce_local_f,
                           (char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *ierr),
                           (sendbuf, recvbuf, count, datatype, op, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_REDUCE_LOCAL = ompi_reduce_local_f
#pragma weak mpi_reduce_local = ompi_reduce_local_f
#pragma weak mpi_reduce_local_ = ompi_reduce_local_f
#pragma weak mpi_reduce_local__ = ompi_reduce_local_f

#pragma weak MPI_Reduce_local_f = ompi_reduce_local_f
#pragma weak MPI_Reduce_local_f08 = ompi_reduce_local_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_REDUCE_LOCAL,
                           mpi_reduce_local,
                           mpi_reduce_local_,
                           mpi_reduce_local__,
                           ompi_reduce_local_f,
                           (char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *ierr),
                           (sendbuf, recvbuf, count, datatype, op, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_reduce_local_f(char *inbuf, char *inoutbuf, MPI_Fint *count,
                        MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Datatype c_type;
    MPI_Op c_op;

    c_type = MPI_Type_f2c(*datatype);
    c_op = MPI_Op_f2c(*op);

    inbuf = (char *) OMPI_F2C_BOTTOM(inbuf);
    inoutbuf = (char *) OMPI_F2C_BOTTOM(inoutbuf);

    c_ierr = MPI_Reduce_local(inbuf, inoutbuf,
                              OMPI_FINT_2_INT(*count),
                              c_type, c_op);
   if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
