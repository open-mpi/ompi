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

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_OP_FREE = ompi_op_free_f
#pragma weak pmpi_op_free = ompi_op_free_f
#pragma weak pmpi_op_free_ = ompi_op_free_f
#pragma weak pmpi_op_free__ = ompi_op_free_f

#pragma weak PMPI_Op_free_f = ompi_op_free_f
#pragma weak PMPI_Op_free_f08 = ompi_op_free_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_OP_FREE,
                           pmpi_op_free,
                           pmpi_op_free_,
                           pmpi_op_free__,
                           pompi_op_free_f,
                           (MPI_Fint *op, MPI_Fint *ierr),
                           (op, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_OP_FREE = ompi_op_free_f
#pragma weak mpi_op_free = ompi_op_free_f
#pragma weak mpi_op_free_ = ompi_op_free_f
#pragma weak mpi_op_free__ = ompi_op_free_f

#pragma weak MPI_Op_free_f = ompi_op_free_f
#pragma weak MPI_Op_free_f08 = ompi_op_free_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_OP_FREE,
                           mpi_op_free,
                           mpi_op_free_,
                           mpi_op_free__,
                           ompi_op_free_f,
                           (MPI_Fint *op, MPI_Fint *ierr),
                           (op, ierr) )
#else
#define ompi_op_free_f pompi_op_free_f
#endif
#endif


void ompi_op_free_f(MPI_Fint *op, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Op c_op;

    c_op = PMPI_Op_f2c(*op);

    c_ierr = PMPI_Op_free(&c_op);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *op = PMPI_Op_c2f(c_op);
    }
}
