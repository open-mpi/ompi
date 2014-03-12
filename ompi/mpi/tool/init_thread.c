/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi/mpi/tool/mpit-internal.h"

#include "ompi/runtime/ompi_info_support.h"
#include "opal/include/opal/sys/atomic.h"
#include "opal/runtime/opal.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_T_init_thread = PMPI_T_init_thread
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/tool/profile/defines.h"
#endif

extern opal_mutex_t mpit_big_lock;

extern volatile uint32_t mpit_init_count;
extern volatile int32_t initted;


int MPI_T_init_thread (int required, int *provided)
{
    static volatile int32_t first_init = 1;
    int rc = MPI_SUCCESS;

    if (opal_atomic_cmpset (&first_init, 1, 0) == 1) {
        OBJ_CONSTRUCT(&mpit_big_lock, opal_mutex_t);
        initted = 1;
    }

    while (!initted) {
        usleep (10);
    }

    mpit_lock ();

    do {
        if (0 != mpit_init_count++) {
            break;
        }

        /* call opal_init_util to intialize the MCA system */
        rc = opal_init_util (NULL, NULL);
        if (OPAL_SUCCESS != rc) {
            rc = MPI_ERR_OTHER;
            break;
        }

        /* register all parameters */
        rc = ompi_info_register_framework_params (NULL);
        if (OMPI_SUCCESS != rc) {
            rc = MPI_ERR_OTHER;
            break;
        }

        /* determine the thread level. TODO -- this might
           be wrong */
        ompi_mpi_thread_level (required, provided);
    } while (0);

    mpit_unlock ();

    return rc;
}
