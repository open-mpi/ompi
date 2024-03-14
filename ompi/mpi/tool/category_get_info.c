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
#pragma weak MPI_T_category_get_info = PMPI_T_category_get_info
#endif
#define MPI_T_category_get_info PMPI_T_category_get_info
#endif

int MPI_T_category_get_info(int cat_index, char *name, int *name_len,
                            char *desc, int *desc_len, int *num_cvars,
                            int *num_pvars, int *num_categories)
{
    const mca_base_var_group_t *group;
    int rc = MPI_SUCCESS;

    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    ompi_mpit_lock ();

    do {
        rc = mca_base_var_group_get (cat_index, &group);
        if (0 > rc) {
            rc = (OPAL_ERR_NOT_FOUND == rc) ? MPI_T_ERR_INVALID_INDEX : MPI_T_ERR_INVALID;
            break;
        }

        if (NULL != num_pvars) {
            *num_pvars = opal_value_array_get_size ((opal_value_array_t *) &group->group_pvars);
        }

        if (NULL != num_cvars) {
            *num_cvars = opal_value_array_get_size ((opal_value_array_t *) &group->group_vars);
        }

        if (NULL != num_categories) {
            *num_categories = opal_value_array_get_size ((opal_value_array_t *) &group->group_subgroups);
        }

        mpit_copy_string (name, name_len, group->group_full_name);
        mpit_copy_string (desc, desc_len, group->group_description);
    } while (0);

    ompi_mpit_unlock ();

    return rc;
}
