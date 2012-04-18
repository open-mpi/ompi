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
 * Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_GREQUEST_START = ompi_grequest_start_f
#pragma weak pmpi_grequest_start = ompi_grequest_start_f
#pragma weak pmpi_grequest_start_ = ompi_grequest_start_f
#pragma weak pmpi_grequest_start__ = ompi_grequest_start_f

#pragma weak PMPI_Grequest_start_f = ompi_grequest_start_f
#pragma weak PMPI_Grequest_start_f08 = ompi_grequest_start_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GREQUEST_START,
                           pmpi_grequest_start,
                           pmpi_grequest_start_,
                           pmpi_grequest_start__,
                           pompi_grequest_start_f,
                           (MPI_F_Grequest_query_function* query_fn, MPI_F_Grequest_free_function* free_fn, MPI_F_Grequest_cancel_function* cancel_fn, MPI_Aint *extra_state, MPI_Fint *request, MPI_Fint *ierr),
                           (query_fn, free_fn, cancel_fn, extra_state, request, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GREQUEST_START = ompi_grequest_start_f
#pragma weak mpi_grequest_start = ompi_grequest_start_f
#pragma weak mpi_grequest_start_ = ompi_grequest_start_f
#pragma weak mpi_grequest_start__ = ompi_grequest_start_f

#pragma weak MPI_Grequest_start_f = ompi_grequest_start_f
#pragma weak MPI_Grequest_start_f08 = ompi_grequest_start_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_GREQUEST_START,
                           mpi_grequest_start,
                           mpi_grequest_start_,
                           mpi_grequest_start__,
                           ompi_grequest_start_f,
                           (MPI_F_Grequest_query_function* query_fn, MPI_F_Grequest_free_function* free_fn, MPI_F_Grequest_cancel_function* cancel_fn, MPI_Aint *extra_state, MPI_Fint *request, MPI_Fint *ierr),
                           (query_fn, free_fn, cancel_fn, extra_state, request, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_grequest_start_f(MPI_F_Grequest_query_function* query_fn,
                          MPI_F_Grequest_free_function* free_fn,
			  MPI_F_Grequest_cancel_function* cancel_fn,
                          MPI_Aint *extra_state,
			  MPI_Fint *request, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Request c_req;
    c_ierr = MPI_Grequest_start(
                                (MPI_Grequest_query_function *) query_fn,
                                (MPI_Grequest_free_function *) free_fn,
                                (MPI_Grequest_cancel_function *) cancel_fn, 
                                extra_state, &c_req);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        /* Manually override the function pointer type flag on the
           grequest to indicate that these are Fortran functions */
        ompi_grequest_t *g = (ompi_grequest_t*) c_req;
        g->greq_funcs_are_c = false;

        *request = MPI_Request_c2f(c_req);
    }
}
