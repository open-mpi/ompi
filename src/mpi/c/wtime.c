/*
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include "mpi.h"
#include "mpi/c/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Wtime = PMPI_Wtime
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif


double MPI_Wtime(void) 
{
    struct timeval tv;
    double wtime;
    gettimeofday(&tv, NULL);
    wtime = tv.tv_sec;
    wtime += (double)tv.tv_usec / 1000000.0;
    return wtime;
}
