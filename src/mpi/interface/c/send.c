/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"
#include "mca/mpi/pml/pml.h"


#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Send = PMPI_Send
#endif

int MPI_Send(void *buf, int count, MPI_Datatype datatype, int dest,
             int tag, MPI_Comm comm) {
    /* FIX - error checking, return value */
    return mca_pml.pml_send(buf, count, datatype, dest, tag, MCA_PML_BASE_SEND_STANDARD, comm);
}
