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
#pragma weak MPI_T_enum_get_info = PMPI_T_enum_get_info
#endif
#define MPI_T_enum_get_info PMPI_T_enum_get_info
#endif

int MPI_T_enum_get_info(MPI_T_enum enumtype, int *num, char *name, int *name_len)
{
    int rc = MPI_SUCCESS;

    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    ompi_mpit_lock ();

    do {
        if (num) {
            rc = enumtype->get_count (enumtype, num);
            if (OPAL_SUCCESS != rc) {
                rc = MPI_T_ERR_INVALID;
                break;
            }
        }

        mpit_copy_string (name, name_len, enumtype->enum_name);
    } while (0);

    ompi_mpit_unlock ();

    return rc;
}
