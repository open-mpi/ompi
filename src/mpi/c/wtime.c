/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>
#include <sys/time.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Wtime = PMPI_Wtime
#endif

double MPI_Wtime(void) {
    struct timeval tv;
    double wtime;
    gettimeofday(&tv, NULL);
    wtime = tv.tv_sec;
    wtime += (double)tv.tv_usec / 1000000.0;
    return wtime;
}
