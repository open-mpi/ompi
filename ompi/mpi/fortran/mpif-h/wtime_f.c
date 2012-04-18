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
 * Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"

/* The OMPI_GENERATE_F77_BINDINGS work only for the most common F77 bindings, the
 * one that does not return any value. There are 2 exceptions MPI_Wtick and MPI_Wtime.
 * For these 2 we can insert the bindings manually.
 */
#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_WTIME = ompi_wtime_f
#pragma weak pmpi_wtime = ompi_wtime_f
#pragma weak pmpi_wtime_ = ompi_wtime_f
#pragma weak pmpi_wtime__ = ompi_wtime_f

#pragma weak PMPI_Wtime_f = ompi_wtime_f
#pragma weak PMPI_Wtime_f08 = ompi_wtime_f
#elif OMPI_PROFILE_LAYER
double PMPI_WTIME(void) { return pompi_wtime_f(); }
double pmpi_wtime(void) { return pompi_wtime_f(); }
double pmpi_wtime_(void) { return pompi_wtime_f(); }
double pmpi_wtime__(void) { return pompi_wtime_f(); }
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WTIME = ompi_wtime_f
#pragma weak mpi_wtime = ompi_wtime_f
#pragma weak mpi_wtime_ = ompi_wtime_f
#pragma weak mpi_wtime__ = ompi_wtime_f

#pragma weak MPI_Wtime_f = ompi_wtime_f
#pragma weak MPI_Wtime_f08 = ompi_wtime_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
double MPI_WTIME(void) { return ompi_wtime_f(); }
double mpi_wtime(void) { return ompi_wtime_f(); }
double mpi_wtime_(void) { return ompi_wtime_f(); }
double mpi_wtime__(void) { return ompi_wtime_f(); }
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

double ompi_wtime_f(void)
{
    return MPI_Wtime();
}
