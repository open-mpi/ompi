/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_WANT_MPI_PROFILING && LAM_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_Type_get_envelope = MPI_Type_get_envelope
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
