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
#pragma weak MPI_T_event_handle_set_info = PMPI_T_event_handle_set_info
#endif
#define MPI_T_event_handle_set_info PMPI_T_event_handle_set_info
#endif

int MPI_T_event_handle_set_info (MPI_T_event_registration event_registration,
                                 MPI_Info info)
{
    opal_info_t *opal_info;
    int rc;

    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    /* MPI_Info is an ompi_info_t whose first member is an opal_info_t. */
    opal_info = (MPI_INFO_NULL == info) ? NULL : &info->super;

    ompi_mpit_lock ();
    rc = mca_base_event_handle_set_info (ompit_event_reg (event_registration), opal_info);
    ompi_mpit_unlock ();

    return (OPAL_SUCCESS == rc) ? MPI_SUCCESS : MPI_T_ERR_INVALID_HANDLE;
}
