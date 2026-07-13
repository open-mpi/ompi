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
#ifdef OMPI_NO_MPI_PROTOTYPES
#include "ompi/mpi/c/abi.h"
#endif

#include "ompi/runtime/ompi_info_support.h"
#include "opal/include/opal/sys/atomic.h"
#include "opal/runtime/opal.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_ALIASES
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

        /* call opal_init_util to initialize the MCA system */
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

#if OMPI_BUILD_MPI_PROFILING && !OPAL_HAVE_WEAK_ALIASES
/*
 * Mach-O cannot express a weak *alias* -- there is no way to mark a ".set"
 * alias as a weak definition -- so where weak aliases are unavailable the
 * public MPI_* symbol is defined here as a weak function that forwards to the
 * strong PMPI_* one.  That is what lets these bindings be compiled exactly
 * once: this translation unit provides both the strong PMPI_* symbol
 * (above) and the weak MPI_* symbol (here).
 */
#undef MPI_T_init_thread
__opal_attribute_weak__ int MPI_T_init_thread(int required, int *provided)
{
    return PMPI_T_init_thread(required, provided);
}
#endif
