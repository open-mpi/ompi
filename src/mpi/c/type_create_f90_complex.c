/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Type_create_f90_complex = PMPI_Type_create_f90_complex
#endif

int
MPI_Type_create_f90_complex(int p, int r, MPI_Datatype *newtype)
{
    return MPI_SUCCESS;
}
