/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-213 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi/mpit/mpit-internal.h"

static const char FUNC_NAME[] = "MPI_T_category_get_pvars";

int MPI_T_category_get_pvars(int cat_index, int len, int indices[])
{
    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }
    /* XXX -- TODO -- add pvar support */

    return MPI_ERR_UNSUPPORTED_OPERATION;
}
