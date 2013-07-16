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

static const char FUNC_NAME[] = "MPI_T_pvar_write";

int MPI_T_pvar_write(MPI_T_pvar_session session, MPI_T_pvar_handle handle,
                     const void* buf)
{
    int ret;

    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    mpit_lock ();

    ret = mca_base_pvar_handle_write_value (handle, buf);

    mpit_unlock ();

    return ret;
}
