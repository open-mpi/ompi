/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi/f77/bindings.h"

/* As the standard allow us to define this function as a macro, it cannot
 * have a profiling interface.
 */
#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WTICK = mpi_wtick_f
#pragma weak mpi_wtick = mpi_wtick_f
#pragma weak mpi_wtick_ = mpi_wtick_f
#pragma weak mpi_wtick__ = mpi_wtick_f

#else

    double mpi_wtick(void) {
        return MPI_Wtick();
    }

    double mpi_wtick_(void) {
        return MPI_Wtick();
    }

    double mpi_wtick__(void) {
        return MPI_Wtick();
    }

    double MPI_WTICK(void) {
        return MPI_Wtick();
    }

#endif

double mpi_wtick_f(void)
{
    return MPI_Wtick();
}
