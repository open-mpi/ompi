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
#pragma weak MPI_T_enum_get_item = PMPI_T_enum_get_item
#endif
#define MPI_T_enum_get_item PMPI_T_enum_get_item
#endif

int MPI_T_enum_get_item(MPI_T_enum enumtype, int index, int *value, char *name,
                        int *name_len)
{
    const char *tmp;
    int rc, count;

    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    ompi_mpit_lock ();

    do {
        rc = enumtype->get_count (enumtype, &count);
        if (OPAL_SUCCESS != rc) {
            rc = MPI_T_ERR_INVALID;
            break;
        }

        if (index >= count) {
            rc = MPI_T_ERR_INVALID_INDEX;
            break;
        }

        rc = enumtype->get_value(enumtype, index, value, &tmp);
        if (OPAL_SUCCESS != rc) {
            rc = MPI_T_ERR_INVALID;
            break;
        }

        mpit_copy_string(name, name_len, tmp);
    } while (0);

    ompi_mpit_unlock ();

    return rc;
}
