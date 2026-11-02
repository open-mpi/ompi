/*
 * Copyright (c) 2012-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2024-2026 NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#include "ompi_config.h"
#include "opal/util/bit_ops.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"

#include "coll_ftagree.h"
#include "coll_ftagree_era.h"

/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this component to disqualify itself if it doesn't support the
 * required level of thread support.
 */
int
mca_coll_ftagree_init_query(bool enable_progress_threads,
                            bool enable_mpi_threads)
{
    if( mca_coll_ftagree_algorithm == COLL_FTAGREE_EARLY_RETURNING ) {
        return mca_coll_ftagree_era_init();
    }

    return OMPI_SUCCESS;
}

/*
 * Init/Fini module on the communicator
 */
/* Install only what this module provides, and on the way out remove only
 * what it installed.  It does not provide every agreement on every
 * communicator - there is no non-blocking one for an intercommunicator, and
 * the NOFT algorithm provides neither - and installing a NULL would replace
 * the fallback coll/basic put there, leaving the communicator with no
 * implementation at all. */
#define FTAGREE_INSTALL_COLL_API(__comm, __module, __api)                         \
    do {                                                                          \
        if (NULL != (__module)->coll_##__api) {                                   \
            MCA_COLL_INSTALL_API(__comm, __api, (__module)->coll_##__api,         \
                                 __module, "ftagree");                            \
        }                                                                         \
    } while (0)

#define FTAGREE_UNINSTALL_COLL_API(__comm, __module, __api)                       \
    do {                                                                          \
        if ((__comm)->c_coll->coll_##__api##_module == (__module)) {              \
            MCA_COLL_INSTALL_API(__comm, __api, NULL, NULL, "ftagree");           \
        }                                                                         \
    } while (0)

static int
mca_coll_ftagree_module_enable(mca_coll_base_module_t *module,
                             struct ompi_communicator_t *comm)
{
    FTAGREE_INSTALL_COLL_API(comm, module, agree);
    FTAGREE_INSTALL_COLL_API(comm, module, iagree);

    /* All done */
    return OMPI_SUCCESS;
}

static int
mca_coll_ftagree_module_disable(mca_coll_base_module_t *module,
                                struct ompi_communicator_t *comm)
{
    FTAGREE_UNINSTALL_COLL_API(comm, module, agree);
    FTAGREE_UNINSTALL_COLL_API(comm, module, iagree);

    /* All done */
    return OMPI_SUCCESS;
}

/*
 * Invoked when there's a new communicator that has been created.
 * Look at the communicator and decide which set of functions and
 * priority we want to return.
 */
mca_coll_base_module_t *
mca_coll_ftagree_comm_query(struct ompi_communicator_t *comm,
                            int *priority)
{
    mca_coll_base_module_agree_fn_t agree = NULL;
    mca_coll_base_module_iagree_fn_t iagree = NULL;
    bool era = false;
    int size;
    mca_coll_ftagree_module_t *ftagree_module;

    /* No FT, no need for fancy agreements */
    if( !ompi_ftmpi_enabled ) return NULL;

    /* Decide which agreements this component can provide on this
     * communicator.  Not every algorithm provides every agreement: NOFT
     * provides neither, and there is no non-blocking one for an
     * intercommunicator.
     */
    switch( mca_coll_ftagree_algorithm ) {
    case COLL_FTAGREE_NOFT:
        /* These are set as default in coll_basic */
        break;
    case COLL_FTAGREE_EARLY_TERMINATION:
        if( !OMPI_COMM_IS_INTER(comm) ) {
            agree = mca_coll_ftagree_eta_intra;
        }
        break;
    default: /* Manages the COLL_FTAGREE_EARLY_RETURNING as default case too */
        era = true;
        if( OMPI_COMM_IS_INTER(comm) ) {
            agree  = mca_coll_ftagree_era_inter;
        } else {
            agree  = mca_coll_ftagree_era_intra;
            iagree = mca_coll_ftagree_iera_intra;
        }
        break;
    }

    /* Nothing to provide, nothing to select: leave the communicator to
     * coll/basic's shims rather than hold state we will never use. */
    if( NULL == agree && NULL == iagree ) {
        return NULL;
    }

    ftagree_module = OBJ_NEW(mca_coll_ftagree_module_t);
    if (NULL == ftagree_module) return NULL;

    *priority = mca_coll_ftagree_priority;

    /* Allocate the data that hangs off the communicator */
    if (OMPI_COMM_IS_INTER(comm)) {
        size = ompi_comm_remote_size(comm)+ompi_comm_size(comm);
    } else {
        size = ompi_comm_size(comm);
    }
    ftagree_module->mccb_num_reqs = size * 2;
    ftagree_module->mccb_reqs = (ompi_request_t**)
        malloc(sizeof(ompi_request_t *) * ftagree_module->mccb_num_reqs);

    ftagree_module->mccb_num_statuses = size * 2; /* x2 for alltoall */
    ftagree_module->mccb_statuses = (ompi_status_public_t*)
        malloc(sizeof(ompi_status_public_t) * ftagree_module->mccb_num_statuses);

    ftagree_module->super.coll_module_enable = mca_coll_ftagree_module_enable;
    ftagree_module->super.coll_module_disable = mca_coll_ftagree_module_disable;

    /* This component does not provide any base collectives,
     * just the FT collectives.
     * Other function pointers are zeroed by the module constructor.
     */
    ftagree_module->super.coll_agree  = agree;
    ftagree_module->super.coll_iagree = iagree;

    /* Init the agreement function */
    if( era ) {
        mca_coll_ftagree_era_comm_init(comm, ftagree_module);
    }

    return &(ftagree_module->super);
}

