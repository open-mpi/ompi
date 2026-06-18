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
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
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
#pragma weak MPI_T_category_get_num_events = PMPI_T_category_get_num_events
#endif
#define MPI_T_category_get_num_events PMPI_T_category_get_num_events
#endif

int MPI_T_category_get_num_events (int cat_index, int *num_events)
{
    const mca_base_var_group_t *group;
    int rc = MPI_SUCCESS;

    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    if (MPI_PARAM_CHECK && NULL == num_events) {
        return MPI_T_ERR_INVALID;
    }

    ompi_mpit_lock ();

    do {
        /* Look up the category to validate cat_index, consistent with the
           sibling MPI_T_category_get_events function. */
        rc = mca_base_var_group_get (cat_index, &group);
        if (0 > rc) {
            rc = (OPAL_ERR_NOT_FOUND == rc) ? MPI_T_ERR_INVALID_INDEX : MPI_T_ERR_INVALID;
            break;
        }

        /* Open MPI does not yet register any MPI_T events. */
        *num_events = 0;
        rc = MPI_SUCCESS;
    } while (0);

    ompi_mpit_unlock ();

    return rc;
}
