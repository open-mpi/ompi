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
#pragma weak MPI_T_event_handle_get_info = PMPI_T_event_handle_get_info
#endif
#define MPI_T_event_handle_get_info PMPI_T_event_handle_get_info
#endif

int MPI_T_event_handle_get_info (MPI_T_event_registration event_registration,
                                 MPI_Info *info_used)
{
    ompi_info_t *info;
    int ret;

    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    info = OBJ_NEW(ompi_info_t);
    if (NULL == info) {
        return ompit_opal_to_mpit_error(OMPI_ERR_OUT_OF_RESOURCE);
    }

    ret = mca_base_event_handle_get_info (event_registration, &info->super);
    if (OPAL_SUCCESS != ret) {
        OBJ_RELEASE(info);
    } else {
        *info_used = info;
    }

    return ompit_opal_to_mpit_error(ret);
}
