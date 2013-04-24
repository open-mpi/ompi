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

static const char FUNC_NAME[] = "MPI_T_cvar_get_info";

int MPI_T_cvar_get_info(int cvar_index, char *name, int *name_len, int *verbosity,
			MPI_Datatype *datatype, MPI_T_enum *enumtype, char *desc,
			int *desc_len, int *bind, int *scope)
{
    const mca_base_var_t *var;
    int rc = MPI_SUCCESS;

    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    mpit_lock ();

    do {
        rc = mca_base_var_get (cvar_index, &var);
        if (OPAL_SUCCESS != rc) {
            rc = (OPAL_ERR_VALUE_OUT_OF_BOUNDS == rc) ? MPI_T_ERR_INVALID_INDEX :
                MPI_ERR_OTHER;
            break;
        }

        mpit_copy_string (name, name_len, var->mbv_full_name);
        mpit_copy_string (desc, desc_len, var->mbv_description);

        /* find the corresponding mpi type for an mca type */
        switch (var->mbv_type) {
        case MCA_BASE_VAR_TYPE_INT:
        case MCA_BASE_VAR_TYPE_BOOL:
            *datatype = MPI_INT;
            break;
        case MCA_BASE_VAR_TYPE_UNSIGNED_INT:
            *datatype = MPI_UNSIGNED;
            break;
        case MCA_BASE_VAR_TYPE_UNSIGNED_LONG_LONG:
            *datatype = MPI_UNSIGNED_LONG_LONG;
            break;        
        case MCA_BASE_VAR_TYPE_SIZE_T:
            if (sizeof(size_t) == sizeof (unsigned long long)) {
                *datatype = MPI_UNSIGNED_LONG_LONG;
            } else {
                *datatype = MPI_UNSIGNED;
            }
        case MCA_BASE_VAR_TYPE_STRING:
            *datatype = MPI_CHAR;
            break;
        default:
            /* Internal error! Did the MCA variable system change? */
            assert (0);
            mpit_unlock ();
            return MPI_ERR_OTHER;
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
    } while (0);

    mpit_unlock ();

    return rc;
}
