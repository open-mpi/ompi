/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Type_indexed_block = PMPI_Type_indexed_block
#endif

int
MPI_Type_create_indexed_block(int count,
                              int blocklength, 
                              int array_of_displacements[],
                              MPI_Datatype oldtype,
                              MPI_Datatype *newtype)
{
    return MPI_SUCCESS;
}
