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

#include "ompi/mpi/tool/mpit-internal.h"

opal_mutex_t mpit_big_lock;

volatile uint32_t mpit_init_count = 0;
volatile int32_t initted = 0;

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
        *datatype = MPI_INT;
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

int ompit_opal_to_mpit_error (int rc)
{
    switch (rc) {
    case OPAL_SUCCESS:
        return MPI_SUCCESS;
    case OPAL_ERR_OUT_OF_RESOURCE:
        return MPI_T_ERR_MEMORY;
    case OPAL_ERR_VALUE_OUT_OF_BOUNDS:
    case OPAL_ERR_NOT_BOUND:
        return MPI_T_ERR_INVALID_HANDLE;
    default:
        return MPI_ERR_UNKNOWN;
    }
}
