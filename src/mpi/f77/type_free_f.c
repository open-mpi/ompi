/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_TYPE_FREE = mpi_type_free_f
#pragma weak pmpi_type_free = mpi_type_free_f
#pragma weak pmpi_type_free_ = mpi_type_free_f
#pragma weak pmpi_type_free__ = mpi_type_free_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_TYPE_FREE,
                           pmpi_type_free,
                           pmpi_type_free_,
                           pmpi_type_free__,
                           pmpi_type_free_f,
                           (MPI_Fint *type, MPI_Fint *ierr),
                           (type, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_FREE = mpi_type_free_f
#pragma weak mpi_type_free = mpi_type_free_f
#pragma weak mpi_type_free_ = mpi_type_free_f
#pragma weak mpi_type_free__ = mpi_type_free_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_TYPE_FREE,
                           mpi_type_free,
                           mpi_type_free_,
                           mpi_type_free__,
                           mpi_type_free_f,
                           (MPI_Fint *type, MPI_Fint *ierr),
                           (type, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_type_free_f(MPI_Fint *type, MPI_Fint *ierr)
{

}
