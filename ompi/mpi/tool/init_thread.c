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

#include "ompi/runtime/ompi_info_support.h"
#include "opal/include/opal/sys/atomic.h"
#include "opal/runtime/opal.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_T_init_thread = PMPI_T_init_thread
#endif
#define MPI_T_init_thread PMPI_T_init_thread
#endif

extern opal_mutex_t ompi_mpit_big_lock;

extern volatile uint32_t ompi_mpit_init_count;
extern volatile int32_t initted;


int MPI_T_init_thread (int required, int *provided)
{
    int rc = MPI_SUCCESS;

    ompi_mpit_lock ();

    do {
        if (0 != ompi_mpit_init_count++) {
            break;
        }

        /* call opal_init_util to intialize the MCA system */
        rc = opal_init_util (NULL, NULL);
        if (OPAL_SUCCESS != rc) {
            rc = MPI_T_ERR_INVALID;
            break;
        }

        /* register all parameters */
        rc = ompi_info_register_framework_params (NULL);
        if (OMPI_SUCCESS != rc) {
            rc = MPI_T_ERR_INVALID;
            break;
        }

        /* determine the thread level. TODO -- this might
           be wrong */
        ompi_mpi_thread_level (required, provided);
    } while (0);

    ompi_mpit_unlock ();

    return rc;
}
