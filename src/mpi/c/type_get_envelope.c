/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Type_get_envelope = PMPI_Type_get_envelope
#endif

int
MPI_Type_get_envelope(MPI_Datatype type,
                      int *num_integers,
                      int *num_addresses,
                      int *num_datatypes,
                      int *combiner)
{
    return MPI_SUCCESS;
}
