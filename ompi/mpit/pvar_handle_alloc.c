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

#include "mpit-internal.h"

static const char FUNC_NAME[] = "MPI_T_pvar_handle_alloc";

int MPI_T_pvar_handle_alloc(MPI_T_pvar_session session, int pvar_index,
                            void *obj_handle, MPI_T_pvar_handle *handle, int *count)
{
    const mca_base_pvar_t *pvar;
    int ret;

    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    mpit_lock ();

    do {
        /* Find the performance variable. mca_base_pvar_get() handles the
           bounds checking. */
        ret = mca_base_pvar_get (pvar_index, &pvar);
        if (OMPI_SUCCESS != ret) {
            break;
        }

        /* Check the variable binding is something sane */
        if (pvar->bind > MPI_T_BIND_MPI_INFO || pvar->bind < MPI_T_BIND_NO_OBJECT) {
            /* This variable specified an invalid binding (not an MPI object). */
            ret = MPI_T_ERR_INVALID_INDEX;
            break;
        }

        ret = mca_base_pvar_handle_alloc (session, pvar_index, obj_handle,
                                          handle, count);
        if (OPAL_ERR_OUT_OF_RESOURCE == ret) {
            ret = MPI_T_ERR_MEMORY;
        } else if (OPAL_ERR_VALUE_OUT_OF_BOUNDS == ret) {
            ret = MPI_T_ERR_INVALID_HANDLE;
        } else if (OPAL_SUCCESS != ret) {
            ret = MPI_ERR_UNKNOWN;
        }
    } while (0);

    mpit_unlock ();

    return ret;
}
