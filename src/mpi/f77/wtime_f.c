/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_WTIME = mpi_wtime_f
#pragma weak pmpi_wtime = mpi_wtime_f
#pragma weak pmpi_wtime_ = mpi_wtime_f
#pragma weak pmpi_wtime__ = mpi_wtime_f
#elif OMPI_PROFILE_LAYER

    double *pmpi_wtime(void) {
        return pmpi_wtime_f();
    }

    double *pmpi_wtime_(void) {
        return pmpi_wtime_f();
    }

    double *pmpi_wtime__(void) {
        return pmpi_wtime_f();
    }

    double *PMPI_WTIME(void) {
        return pmpi_wtime_f();
    }

#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WTIME = mpi_wtime_f
#pragma weak mpi_wtime = mpi_wtime_f
#pragma weak mpi_wtime_ = mpi_wtime_f
#pragma weak mpi_wtime__ = mpi_wtime_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER

    double *mpi_wtime(void) {
        return mpi_wtime_f();
    }

    double *mpi_wtime_(void) {
        return mpi_wtime_f();
    }

    double *mpi_wtime__(void) {
        return mpi_wtime_f();
    }

    double *MPI_WTIME(void) {
        return mpi_wtime_f();
    }

#endif

double *mpi_wtime_f(void)
{
    return (double *)0;
}
