/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_WTICK = mpi_wtick_f
#pragma weak pmpi_wtick = mpi_wtick_f
#pragma weak pmpi_wtick_ = mpi_wtick_f
#pragma weak pmpi_wtick__ = mpi_wtick_f
#elif OMPI_PROFILE_LAYER

    double pmpi_wtick(void) {
        return pmpi_wtick_f();
    }

    double pmpi_wtick_(void) {
        return pmpi_wtick_f();
    }

    double pmpi_wtick__(void) {
        return pmpi_wtick_f();
    }

    double PMPI_WTICK(void) {
        return pmpi_wtick_f();
    }

#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WTICK = mpi_wtick_f
#pragma weak mpi_wtick = mpi_wtick_f
#pragma weak mpi_wtick_ = mpi_wtick_f
#pragma weak mpi_wtick__ = mpi_wtick_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER

    double mpi_wtick(void) {
        return mpi_wtick_f();
    }

    double mpi_wtick_(void) {
        return mpi_wtick_f();
    }

    double mpi_wtick__(void) {
        return mpi_wtick_f();
    }

    double MPI_WTICK(void) {
        return mpi_wtick_f();
    }

#endif

double mpi_wtick_f(void)
{
    return MPI_Wtick();
}
