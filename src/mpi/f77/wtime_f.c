/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi/f77/bindings.h"

/* As the standard allow us to implement this function as a macro
 * we cannot have profiling interface.
 */
#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WTIME = mpi_wtime_f
#pragma weak mpi_wtime = mpi_wtime_f
#pragma weak mpi_wtime_ = mpi_wtime_f
#pragma weak mpi_wtime__ = mpi_wtime_f

#else

    double mpi_wtime(void) {
        return MPI_Wtime();
    }

    double mpi_wtime_(void) {
        return MPI_Wtime();
    }

    double mpi_wtime__(void) {
        return MPI_Wtime();
    }

    double MPI_WTIME(void) {
        return MPI_Wtime();
    }

#endif

double mpi_wtime_f(void)
{
    return MPI_Wtime();
}
