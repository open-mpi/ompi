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

static const char FUNC_NAME[] = "MPI_T_cvar_write";

int MPI_T_cvar_write (MPI_T_cvar_handle handle, const void *buf)
{
    mca_base_var_storage_t value;
    int rc = MPI_SUCCESS;

    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    if (MPI_PARAM_CHECK && NULL == buf) {
        return MPI_ERR_ARG;
    }

    mpit_lock ();

    do {
        if (MCA_BASE_VAR_SCOPE_CONSTANT == handle->var->mbv_scope ||
            MCA_BASE_VAR_SCOPE_READONLY == handle->var->mbv_scope) {
            rc = MPI_T_ERR_CVAR_SET_NEVER;
            break;
        }

        if (!(MCA_BASE_VAR_FLAG_SETTABLE & handle->var->mbv_flags)) {
            rc = MPI_T_ERR_CVAR_SET_NOT_NOW;
            break;
        }

        switch (handle->var->mbv_type) {
        case MCA_BASE_VAR_TYPE_STRING:
            value.stringval = (char *) buf;

            break;
        case MCA_BASE_VAR_TYPE_INT:
        case MCA_BASE_VAR_TYPE_UNSIGNED_INT:
            value.intval = ((int *) buf)[0];

            break;
        case MCA_BASE_VAR_TYPE_BOOL:
            /* we expose boolean values as integers */
            value.boolval = !!(((int *) buf)[0]);
            break;
        case MCA_BASE_VAR_TYPE_UNSIGNED_LONG_LONG:
            value.ullval = ((unsigned long long *) buf)[0];
            break;
        case MCA_BASE_VAR_TYPE_SIZE_T:
            if (sizeof(size_t) == sizeof(unsigned long long)) {
                value.ullval = ((unsigned long long *) buf)[0];
            } else {
                value.intval = ((int *) buf)[0];
            }
            break;
        default:
            rc = MPI_T_ERR_CVAR_SET_NEVER;
            break;
        }
    } while (0);

    if (MPI_SUCCESS == rc) {
        rc = mca_base_var_set_value(handle->var->mbv_index, &value, sizeof(value), MCA_BASE_VAR_SOURCE_SET, NULL);
        if (OPAL_SUCCESS != rc) {
            rc = MPI_T_ERR_CVAR_SET_NOT_NOW;
        }
    }

    mpit_unlock ();

    return rc;
}
