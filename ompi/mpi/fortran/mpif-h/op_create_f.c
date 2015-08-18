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
 * Copyright (c) 2008-2012 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_OP_CREATE = ompi_op_create_f
#pragma weak pmpi_op_create = ompi_op_create_f
#pragma weak pmpi_op_create_ = ompi_op_create_f
#pragma weak pmpi_op_create__ = ompi_op_create_f

#pragma weak PMPI_Op_create_f = ompi_op_create_f
#pragma weak PMPI_Op_create_f08 = ompi_op_create_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_OP_CREATE,
                           pmpi_op_create,
                           pmpi_op_create_,
                           pmpi_op_create__,
                           pompi_op_create_f,
                           (ompi_op_fortran_handler_fn_t* function, ompi_fortran_logical_t *commute, MPI_Fint *op, MPI_Fint *ierr),
                           (function, commute, op, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_OP_CREATE = ompi_op_create_f
#pragma weak mpi_op_create = ompi_op_create_f
#pragma weak mpi_op_create_ = ompi_op_create_f
#pragma weak mpi_op_create__ = ompi_op_create_f

#pragma weak MPI_Op_create_f = ompi_op_create_f
#pragma weak MPI_Op_create_f08 = ompi_op_create_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_OP_CREATE,
                           mpi_op_create,
                           mpi_op_create_,
                           mpi_op_create__,
                           ompi_op_create_f,
                           (ompi_op_fortran_handler_fn_t* function, ompi_fortran_logical_t *commute, MPI_Fint *op, MPI_Fint *ierr),
                           (function, commute, op, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_op_create_f(ompi_op_fortran_handler_fn_t* function, ompi_fortran_logical_t *commute,
		     MPI_Fint *op, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Op c_op;

    /* See the note in src/mpi/fortran/mpif-h/prototypes_mpi.h about
       the use of (void*) for function pointers in this function */

    c_ierr = MPI_Op_create((MPI_User_function *) function,
                           OMPI_LOGICAL_2_INT(*commute),
                           &c_op);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        c_op->o_flags |= OMPI_OP_FLAGS_FORTRAN_FUNC;
        *op = MPI_Op_c2f(c_op);
    }
}
