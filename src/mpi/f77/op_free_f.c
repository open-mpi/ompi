/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_OP_FREE = mpi_op_free_f
#pragma weak pmpi_op_free = mpi_op_free_f
#pragma weak pmpi_op_free_ = mpi_op_free_f
#pragma weak pmpi_op_free__ = mpi_op_free_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_OP_FREE,
                           pmpi_op_free,
                           pmpi_op_free_,
                           pmpi_op_free__,
                           pmpi_op_free_f,
                           (MPI_Fint *op, MPI_Fint *ierr),
                           (op, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_OP_FREE = mpi_op_free_f
#pragma weak mpi_op_free = mpi_op_free_f
#pragma weak mpi_op_free_ = mpi_op_free_f
#pragma weak mpi_op_free__ = mpi_op_free_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_OP_FREE,
                           mpi_op_free,
                           mpi_op_free_,
                           mpi_op_free__,
                           mpi_op_free_f,
                           (MPI_Fint *op, MPI_Fint *ierr),
                           (op, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_op_free_f(MPI_Fint *op, MPI_Fint *ierr)
{

}
