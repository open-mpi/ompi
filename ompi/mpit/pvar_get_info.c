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

static const char FUNC_NAME[] = "MPI_T_pvar_get_info";

int MPI_T_pvar_get_info(int pvar_index, char *name, int *name_len,
                        int *verbosity, int *var_class, MPI_Datatype *datatype,
                        MPI_T_enum *enumtype, char *desc, int *desc_len, int *bind,
                        int *readonly, int *continuous, int *atomic)
{
    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    /* XXX -- TODO -- Implement me */
    return MPI_T_ERR_INVALID_INDEX;
}
