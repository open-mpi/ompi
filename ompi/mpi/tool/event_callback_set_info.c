/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2017      IBM Corporation. All rights reserved.
 * Copyright (c) 2019      Google, LLC. All rights reserved.
 * Copyright (c) 2019-2025 Triad National Security, LLC. All rights
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
#pragma weak MPI_T_event_callback_set_info = PMPI_T_event_callback_set_info
#endif
#define MPI_T_event_callback_set_info PMPI_T_event_callback_set_info
#endif

int MPI_T_event_callback_set_info (MPI_T_event_registration event_registration,
                                   MPI_T_cb_safety cb_safety, MPI_Info info)
{
    int ret;

    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    /* mca_base_cb_safety_t and MPI_T_cb_safety must be kept in sync for this to work */
    ret = mca_base_event_callback_set_info (event_registration, (mca_base_cb_safety_t) cb_safety, &info->super);

    return ompit_opal_to_mpit_error(ret);
}
