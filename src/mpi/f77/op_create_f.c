/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "op/op.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_OP_CREATE = mpi_op_create_f
#pragma weak pmpi_op_create = mpi_op_create_f
#pragma weak pmpi_op_create_ = mpi_op_create_f
#pragma weak pmpi_op_create__ = mpi_op_create_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_OP_CREATE,
                           pmpi_op_create,
                           pmpi_op_create_,
                           pmpi_op_create__,
                           pmpi_op_create_f,
                           (void *function, MPI_Fint *commute, MPI_Fint *op, MPI_Fint *ierr),
                           (function, commute, op, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_OP_CREATE = mpi_op_create_f
#pragma weak mpi_op_create = mpi_op_create_f
#pragma weak mpi_op_create_ = mpi_op_create_f
#pragma weak mpi_op_create__ = mpi_op_create_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_OP_CREATE,
                           mpi_op_create,
                           mpi_op_create_,
                           mpi_op_create__,
                           mpi_op_create_f,
                           (void *function, MPI_Fint *commute, MPI_Fint *op, MPI_Fint *ierr),
                           (function, commute, op, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_op_create_f(void *function, MPI_Fint *commute,
		     MPI_Fint *op, MPI_Fint *ierr)
{
    MPI_Op c_op;

    /* See the note in src/mpi/f77/prototypes_mpi.h about the use of
       (void*) for function pointers in this function */

    *ierr = OMPI_INT_2_FINT(MPI_Op_create((MPI_User_function *) function,
					  OMPI_FINT_2_INT(*commute),
					  &c_op));
    c_op->o_flags |= OMPI_OP_FLAGS_FORTRAN_FUNC;

    *op = MPI_Op_c2f(c_op);
}
