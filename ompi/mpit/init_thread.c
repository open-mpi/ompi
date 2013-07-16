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

int ompit_var_type_to_datatype (mca_base_var_type_t type, MPI_Datatype *datatype)
{
    switch (type) {
    case MCA_BASE_VAR_TYPE_INT:
        *datatype = MPI_INT;
        break;
    case MCA_BASE_VAR_TYPE_UNSIGNED_INT:
        *datatype = MPI_UNSIGNED;
        break;
    case MCA_BASE_VAR_TYPE_UNSIGNED_LONG:
        *datatype = MPI_UNSIGNED_LONG;
        break;
    case MCA_BASE_VAR_TYPE_UNSIGNED_LONG_LONG:
        *datatype = MPI_UNSIGNED_LONG_LONG;
        break;
    case MCA_BASE_VAR_TYPE_SIZE_T:
        if (sizeof (size_t) == sizeof (unsigned)) {
            *datatype = MPI_UNSIGNED;
        } else if (sizeof (size_t) == sizeof (unsigned long)) {
            *datatype = MPI_UNSIGNED_LONG;
        } else if (sizeof (size_t) == sizeof (unsigned long long)) {
            *datatype = MPI_UNSIGNED_LONG_LONG;
        } else {
            /* not supported -- fixme */
            assert (0);
        }

        break;
    case MCA_BASE_VAR_TYPE_STRING:
        *datatype = MPI_CHAR;
        break;
    case MCA_BASE_VAR_TYPE_BOOL:
        if (sizeof (bool) == sizeof (char)) {
            *datatype = MPI_CHAR;
        } else if (sizeof (bool) == sizeof (int)) {
            *datatype = MPI_INT;
        } else {
            /* not supported -- fixme */
            assert (0);
        }
        break;
    case MCA_BASE_VAR_TYPE_DOUBLE:
        *datatype = MPI_DOUBLE;
        break;
    default:
        /* not supported -- fixme */
        assert (0);
        break;
    }

    return OMPI_SUCCESS;
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
