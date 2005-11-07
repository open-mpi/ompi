/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_WTIME = mpi_wtime_f
#pragma weak pmpi_wtime = mpi_wtime_f
#pragma weak pmpi_wtime_ = mpi_wtime_f
#pragma weak pmpi_wtime__ = mpi_wtime_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WTIME,
                           pmpi_wtime,
                           pmpi_wtime_,
                           pmpi_wtime__,
                           pmpi_wtime_f,
                           (),
                           () )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WTIME = mpi_wtime_f
#pragma weak mpi_wtime = mpi_wtime_f
#pragma weak mpi_wtime_ = mpi_wtime_f
#pragma weak mpi_wtime__ = mpi_wtime_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WTIME,
                           mpi_wtime,
                           mpi_wtime_,
                           mpi_wtime__,
                           mpi_wtime_f,
                           (),
                           () )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

double mpi_wtime_f(void)
{
    return MPI_Wtime();
}
