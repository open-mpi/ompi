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
#pragma weak MPI_T_cvar_get_info = PMPI_T_cvar_get_info
#endif
#define MPI_T_cvar_get_info PMPI_T_cvar_get_info
#endif

int MPI_T_cvar_get_info(int cvar_index, char *name, int *name_len, int *verbosity,
			MPI_Datatype *datatype, MPI_T_enum *enumtype, char *desc,
			int *desc_len, int *bind, int *scope)
{
    const mca_base_var_t *var;
    int rc = MPI_SUCCESS;

    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    ompi_mpit_lock ();

    do {
        rc = mca_base_var_get (cvar_index, &var);
        if (OPAL_SUCCESS != rc) {
            rc = (OPAL_ERR_VALUE_OUT_OF_BOUNDS == rc || OPAL_ERR_NOT_FOUND == rc) ? MPI_T_ERR_INVALID_INDEX :
                MPI_T_ERR_INVALID;
            break;
        }

        mpit_copy_string (name, name_len, var->mbv_full_name);
        mpit_copy_string (desc, desc_len, var->mbv_description);

        /* find the corresponding mpi type for an mca type */
        rc = ompit_var_type_to_datatype (var->mbv_type, datatype);
        if (OMPI_SUCCESS != rc) {
            rc = MPI_T_ERR_INVALID;  /* can't really happen as MPI_SUCCESS is the only
                                        possible return from ompit_var_type_to_datatype */
            break;
        }

        if (NULL != enumtype) {
            *enumtype = var->mbv_enumerator ? (MPI_T_enum) var->mbv_enumerator : MPI_T_ENUM_NULL;
        }

        if (NULL != scope) {
            *scope = var->mbv_scope;
        }

        /* XXX -- TODO -- All bindings are currently 0. Add support for variable binding. */
        if (NULL != bind) {
            *bind = var->mbv_bind;
        }

        if (NULL != verbosity) {
            *verbosity = var->mbv_info_lvl;
        }
    } while (0);

    ompi_mpit_unlock ();

    return rc;
}
