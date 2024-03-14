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
#pragma weak MPI_T_pvar_get_info = PMPI_T_pvar_get_info
#endif
#define MPI_T_pvar_get_info PMPI_T_pvar_get_info
#endif

int MPI_T_pvar_get_info(int pvar_index, char *name, int *name_len,
                        int *verbosity, int *var_class, MPI_Datatype *datatype,
                        MPI_T_enum *enumtype, char *desc, int *desc_len, int *bind,
                        int *readonly, int *continuous, int *atomic)
{
    const mca_base_pvar_t *pvar;
    int ret;

    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    ompi_mpit_lock ();

    do {
        /* Find the performance variable. mca_base_pvar_get() handles the
           bounds checking. */
        ret = mca_base_pvar_get (pvar_index, &pvar);
        if (OMPI_SUCCESS != ret) {
            ret = (OPAL_ERR_NOT_FOUND == ret) ? MPI_T_ERR_INVALID_INDEX : MPI_T_ERR_INVALID;
            break;
        }

        /* Check the variable binding is something sane */
        if (pvar->bind > MPI_T_BIND_MPI_INFO || pvar->bind < MPI_T_BIND_NO_OBJECT) {
            /* This variable specified an invalid binding (not an MPI object). */
            ret = MPI_T_ERR_INVALID_INDEX;
            break;
        }

        /* Copy name an description */
        mpit_copy_string (name, name_len, pvar->name);
        mpit_copy_string (desc, desc_len, pvar->description);

        if (verbosity) {
            *verbosity = pvar->verbosity;
        }

        if (var_class) {
            *var_class = pvar->var_class;
        }

        ret = ompit_var_type_to_datatype (pvar->type, datatype);
        if (OMPI_SUCCESS != ret) {
            ret = MPI_T_ERR_INVALID;  /* can't really happen as MPI_SUCCESS is the only
                                         possible return from ompit_var_type_to_datatype */
        }

        if (NULL != enumtype) {
            *enumtype = pvar->enumerator ? (MPI_T_enum) pvar->enumerator : MPI_T_ENUM_NULL;
        }

        if (NULL != bind) {
            *bind = pvar->bind;
        }

        if (NULL != readonly) {
            *readonly = mca_base_pvar_is_readonly (pvar);
        }

        if (NULL != continuous) {
            *continuous = mca_base_pvar_is_continuous (pvar);
        }

        if (NULL != atomic) {
            *atomic = mca_base_pvar_is_atomic (pvar);
        }
    } while (0);

    ompi_mpit_unlock ();

    return ret;
}
