/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Type_get_contents = PMPI_Type_get_contents
#endif

int
MPI_Type_get_contents(MPI_Datatype mtype,
                      int max_integers,
                      int max_addresses,
                      int max_datatypes,
                      int array_of_integers[],
                      MPI_Aint array_of_addresses[],
                      MPI_Datatype array_of_datatypes[])
{
    return MPI_SUCCESS;
}
