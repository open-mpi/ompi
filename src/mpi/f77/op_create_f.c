/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_OP_CREATE = mpi_op_create_f
#pragma weak pmpi_op_create = mpi_op_create_f
#pragma weak pmpi_op_create_ = mpi_op_create_f
#pragma weak pmpi_op_create__ = mpi_op_create_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_OP_CREATE,
                           pmpi_op_create,
                           pmpi_op_create_,
                           pmpi_op_create__,
                           pmpi_op_create_f,
                           (MPI_Fint *function, MPI_Fint *commute, MPI_Fint *op, MPI_Fint *ierr),
                           (function, commute, op, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_OP_CREATE = mpi_op_create_f
#pragma weak mpi_op_create = mpi_op_create_f
#pragma weak mpi_op_create_ = mpi_op_create_f
#pragma weak mpi_op_create__ = mpi_op_create_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_OP_CREATE,
                           mpi_op_create,
                           mpi_op_create_,
                           mpi_op_create__,
                           mpi_op_create_f,
                           (MPI_Fint *function, MPI_Fint *commute, MPI_Fint *op, MPI_Fint *ierr),
                           (function, commute, op, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_op_create_f(MPI_Fint *function, MPI_Fint *commute, MPI_Fint *op, MPI_Fint *ierr)
{

}
