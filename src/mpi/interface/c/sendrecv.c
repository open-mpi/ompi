/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Sendrecv = PMPI_Sendrecv
#endif

int MPI_Sendrecv(void *sendbuf, int sendcount, MPI_Datatype recvtype,
                 int dest, int sendtag, void *recvbuf, int recvcount,
                 MPI_Datatype sendtype, int source, int recvtag,
                 MPI_Comm comm,  MPI_Status *status) {
    return MPI_SUCCESS;
}
