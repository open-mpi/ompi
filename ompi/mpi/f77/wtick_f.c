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
#pragma weak PMPI_WTICK = mpi_wtick_f
#pragma weak pmpi_wtick = mpi_wtick_f
#pragma weak pmpi_wtick_ = mpi_wtick_f
#pragma weak pmpi_wtick__ = mpi_wtick_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WTICK,
                           pmpi_wtick,
                           pmpi_wtick_,
                           pmpi_wtick__,
                           pmpi_wtick_f,
                           (),
                           () )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WTICK = mpi_wtick_f
#pragma weak mpi_wtick = mpi_wtick_f
#pragma weak mpi_wtick_ = mpi_wtick_f
#pragma weak mpi_wtick__ = mpi_wtick_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WTICK,
                           mpi_wtick,
                           mpi_wtick_,
                           mpi_wtick__,
                           mpi_wtick_f,
                           (),
                           () )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

double mpi_wtick_f(void)
{
    return MPI_Wtick();
}
