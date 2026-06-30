/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2017      IBM Corporation. All rights reserved.
 * Copyright (c) 2019      Google, LLC. All rights reserved.
 * Copyright (c) 2019-2025 Triad National Security, LLC. All rights
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
#include "ompi/info/info.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_T_event_callback_get_info = PMPI_T_event_callback_get_info
#endif
#define MPI_T_event_callback_get_info PMPI_T_event_callback_get_info
#endif


int MPI_T_event_callback_get_info (MPI_T_event_registration event_registration,
                                   MPI_T_cb_safety cb_safety, MPI_Info *info_used)
{
    opal_info_t *opal_info_used = NULL;
    int rc;

    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    if (MPI_PARAM_CHECK && (NULL == info_used)) {
        return MPI_ERR_ARG;
    }

    ompi_mpit_lock ();
    rc = mca_base_event_callback_get_info (ompit_event_reg (event_registration),
                                           (mca_base_event_cb_safety_t) cb_safety,
                                           &opal_info_used);
    ompi_mpit_unlock ();

    if (OPAL_SUCCESS != rc) {
        return MPI_T_ERR_INVALID_HANDLE;
    }

    /* OPAL handed back a fresh, OPAL-owned opal_info_t; copy its contents into
       a user-freeable MPI_Info (mirrors MPI_Comm_get_info) and release OPAL's. */
    *info_used = ompi_info_allocate ();
    if (NULL == *info_used) {
        OBJ_RELEASE (opal_info_used);
        return MPI_T_ERR_MEMORY;
    }

    opal_info_t *opal_dst = &(*info_used)->super;
    rc = opal_info_dup (opal_info_used, &opal_dst);
    OBJ_RELEASE (opal_info_used);
    if (OPAL_SUCCESS != rc) {
        OBJ_RELEASE (*info_used);
        *info_used = MPI_INFO_NULL;
        return MPI_T_ERR_MEMORY;
    }

    return MPI_SUCCESS;
}
