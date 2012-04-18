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

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_START = ompi_start_f
#pragma weak pmpi_start = ompi_start_f
#pragma weak pmpi_start_ = ompi_start_f
#pragma weak pmpi_start__ = ompi_start_f

#pragma weak PMPI_Start_f = ompi_start_f
#pragma weak PMPI_Start_f08 = ompi_start_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_START,
                           pmpi_start,
                           pmpi_start_,
                           pmpi_start__,
                           pompi_start_f,
                           (MPI_Fint *request, MPI_Fint *ierr),
                           (request, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_START = ompi_start_f
#pragma weak mpi_start = ompi_start_f
#pragma weak mpi_start_ = ompi_start_f
#pragma weak mpi_start__ = ompi_start_f

#pragma weak MPI_Start_f = ompi_start_f
#pragma weak MPI_Start_f08 = ompi_start_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_START,
                           mpi_start,
                           mpi_start_,
                           mpi_start__,
                           ompi_start_f,
                           (MPI_Fint *request, MPI_Fint *ierr),
                           (request, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_start_f(MPI_Fint *request, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Request c_req = MPI_Request_f2c(*request);
    MPI_Request tmp_req = c_req;

    c_ierr = MPI_Start(&c_req);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        /* For a persistent request, the underlying request descriptor could
           change (i.e. the old descriptor has not completed and cannot be 
           reused).
           So commit new descriptor.
        */
        if ( tmp_req != c_req ) {
            *request = MPI_Request_c2f(c_req);
        }
    }
}
