/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"
#include "mca/mpi/pml/pml.h"


#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Recv = PMPI_Recv
#endif

int MPI_Recv(void *buf, int count, MPI_Datatype datatype, int source,
             int tag, MPI_Comm comm, MPI_Status *status) {
    /* FIX - error checking */
    return mca_pml.pml_recv(buf, count, datatype, source, tag, comm, status);
}
