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

static const char FUNC_NAME[] = "MPI_T_enum_get_info";

int MPI_T_enum_get_info(MPI_T_enum enumtype, int *num, char *name, int *name_len)
{
    int rc;

    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    mpit_lock ();

    do {
        if (num) {
            rc = enumtype->get_count (enumtype, num);
            if (OPAL_SUCCESS != rc) {
                rc = MPI_ERR_OTHER;
                break;
            }
        }

        mpit_copy_string (name, name_len, enumtype->enum_name);
    } while (0);

    mpit_unlock ();

    return rc;
}
