/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2017      IBM Corporation. All rights reserved.
 * Copyright (c) 2020      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2021      Amazon.com, Inc. or its affiliates.  All Rights
 *                         reserved.
 * Copyright (c) 2025      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/tool/mpit-internal.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_T_cvar_handle_alloc = PMPI_T_cvar_handle_alloc
#endif
#define MPI_T_cvar_handle_alloc PMPI_T_cvar_handle_alloc
#endif

int MPI_T_cvar_handle_alloc (int cvar_index, void *obj_handle,
                             MPI_T_cvar_handle *handle, int *count)
{
    ompi_mpit_cvar_handle_t *new_handle;
    int rc = MPI_SUCCESS;;

    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    if (MPI_PARAM_CHECK && (NULL == handle || NULL == count)) {
        return MPI_T_ERR_INVALID;
    }

    if (NULL != obj_handle) {
        if (ompit_obj_invalid(obj_handle)) {
            return MPI_T_ERR_INVALID_HANDLE;
        }
    }

    ompi_mpit_lock ();

    *handle = NULL;

    do {
        new_handle = (ompi_mpit_cvar_handle_t *) malloc (sizeof (ompi_mpit_cvar_handle_t));
        if (NULL == new_handle) {
            rc = MPI_T_ERR_MEMORY;
            break;
        }

        rc = mca_base_var_get(cvar_index, &new_handle->var);
        if (OPAL_SUCCESS != rc) {
            rc = (OPAL_ERR_VALUE_OUT_OF_BOUNDS == rc || OPAL_ERR_NOT_FOUND == rc) ? MPI_T_ERR_INVALID_INDEX:
                MPI_T_ERR_INVALID;
            free (new_handle);
            break;
        }

        new_handle->bound_object = obj_handle;

        if (MCA_BASE_VAR_TYPE_STRING == new_handle->var->mbv_type) {
            /* Arbitrary string limit. Is there a better way to do this? */
            *count = 2048;
        } else {
            /* MCA only supports a single integer at this time. Change me if
               this assumption changes. */
            *count = 1;
        }

        *handle = (MPI_T_cvar_handle) new_handle;
    } while (0);

    ompi_mpit_unlock ();

    return rc;
}
