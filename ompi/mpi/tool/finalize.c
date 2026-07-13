/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014-2018 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2017      IBM Corporation. All rights reserved.
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
#pragma weak MPI_T_finalize = PMPI_T_finalize
#endif
#define MPI_T_finalize PMPI_T_finalize
#else
/*
 * The MPI Forum ABI requires that the public MPI_* symbols be *weak*
 * definitions.  An application built against another implementation's
 * libmpi_abi imports them as weak definitions, and (at least on macOS)
 * the loader will only satisfy such an import from another weak
 * definition -- a strong one is rejected.  When the bindings are compiled
 * separately (i.e., when weak aliases are unavailable), this is the only
 * definition of the symbol in libmpi_abi, so mark it weak here.
 */
#pragma weak MPI_T_finalize
#endif

int MPI_T_finalize (void)
{
    ompi_mpit_lock ();

    if (!mpit_is_initialized ()) {
        ompi_mpit_unlock ();
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    if (0 == --ompi_mpit_init_count) {
        (void) ompi_info_close_components ();

        int32_t state = ompi_mpi_state;
        if ((state < OMPI_MPI_STATE_INIT_COMPLETED ||
             state >= OMPI_MPI_STATE_FINALIZE_PAST_COMM_SELF_DESTRUCT) &&
            (NULL != ompi_mpi_main_thread)) {
            /* we are not between MPI_Init and MPI_Finalize so we
             * have to free the ompi_mpi_main_thread */
            OBJ_RELEASE(ompi_mpi_main_thread);
            ompi_mpi_main_thread = NULL;
        }

        (void) opal_finalize_util ();
    }

    ompi_mpit_unlock ();

    return MPI_SUCCESS;
}
