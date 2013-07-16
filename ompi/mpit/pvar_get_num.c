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

static const char FUNC_NAME[] = "MPI_T_pvar_get_num";

int MPI_T_pvar_get_num(int *num_pvar)
{
    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    if (MPI_PARAM_CHECK && NULL == num_pvar) {
        return MPI_ERR_ARG;
    }

    return mca_base_pvar_get_count (num_pvar);
}

