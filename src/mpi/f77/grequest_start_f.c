/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_GREQUEST_START = mpi_grequest_start_f
#pragma weak pmpi_grequest_start = mpi_grequest_start_f
#pragma weak pmpi_grequest_start_ = mpi_grequest_start_f
#pragma weak pmpi_grequest_start__ = mpi_grequest_start_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GREQUEST_START,
                           pmpi_grequest_start,
                           pmpi_grequest_start_,
                           pmpi_grequest_start__,
                           pmpi_grequest_start_f,
                           (MPI_Fint *query_fn, MPI_Fint *free_fn, MPI_Fint *cancel_fn, char *extra_state, MPI_Fint *request, MPI_Fint *ierr),
                           (query_fn, free_fn, cancel_fn, extra_state, request, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GREQUEST_START = mpi_grequest_start_f
#pragma weak mpi_grequest_start = mpi_grequest_start_f
#pragma weak mpi_grequest_start_ = mpi_grequest_start_f
#pragma weak mpi_grequest_start__ = mpi_grequest_start_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_GREQUEST_START,
                           mpi_grequest_start,
                           mpi_grequest_start_,
                           mpi_grequest_start__,
                           mpi_grequest_start_f,
                           (MPI_Fint *query_fn, MPI_Fint *free_fn, MPI_Fint *cancel_fn, char *extra_state, MPI_Fint *request, MPI_Fint *ierr),
                           (query_fn, free_fn, cancel_fn, extra_state, request, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_grequest_start_f(MPI_Fint *query_fn, MPI_Fint *free_fn,
			  MPI_Fint *cancel_fn, char *extra_state,
			  MPI_Fint *request, MPI_Fint *ierr)
{
    MPI_Request c_req;
    *ierr = OMPI_INT_2_FINT(MPI_Grequest_start(
			       (MPI_Grequest_query_function *)*query_fn,
			       (MPI_Grequest_free_function *)*free_fn,
			       (MPI_Grequest_cancel_function *)*cancel_fn, 
			       extra_state, &c_req));

    if (MPI_SUCCESS == *ierr) {
        *request = MPI_Request_c2f(c_req);
    }

}
