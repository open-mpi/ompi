/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "ompi/mpit/mpit-internal.h"

#include <sched.h>

#include "ompi/runtime/ompi_info_support.h"
#include "opal/include/opal/sys/atomic.h"
#include "opal/runtime/opal.h"

static const char FUNC_NAME[] = "MPI_T_init_thread";

opal_mutex_t mpit_big_lock;

volatile uint32_t mpit_init_count = 0;
static volatile int32_t initted = 0;

void mpit_lock (void)
{
    if (initted) {
        opal_mutex_lock (&mpit_big_lock);
    }
}

void mpit_unlock (void)
{
    if (initted) {
        opal_mutex_unlock (&mpit_big_lock);
    }
}

int MPI_T_init_thread (int required, int *provided)
{
    static volatile int32_t first_init = 1;
    int rc = MPI_SUCCESS;

    if (opal_atomic_cmpset (&first_init, 1, 0) == 1) {
        OBJ_CONSTRUCT(&mpit_big_lock, opal_mutex_t);
        initted = 1;
    }

    while (!initted) {
        sched_yield ();
        usleep (1000);
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

        rc = ompi_info_register_framework_params (NULL);
        if (OMPI_SUCCESS != rc) {
            rc = MPI_ERR_OTHER;
            break;
        }
        ompi_mpi_thread_level (required, provided);
    } while (0);

    mpit_unlock ();

    return rc;
}
