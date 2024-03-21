/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2017 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2024      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <stdio.h>

#include "coll_sync.h"

#include "mpi.h"

#include "opal/util/show_help.h"
#include "ompi/runtime/ompi_rte.h"

#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"
#include "coll_sync.h"

static int
mca_coll_sync_module_enable(mca_coll_base_module_t *module,
                            struct ompi_communicator_t *comm);
static int
mca_coll_sync_module_disable(mca_coll_base_module_t *module,
                             struct ompi_communicator_t *comm);

static void mca_coll_sync_module_construct(mca_coll_sync_module_t *module)
{
    memset(&(module->c_coll), 0, sizeof(module->c_coll));
    module->before_num_operations = 0;
    module->after_num_operations = 0;
    module->in_operation = false;
}

static void mca_coll_sync_module_destruct(mca_coll_sync_module_t *module)
{
    OBJ_RELEASE(module->c_coll.coll_bcast_module);
    OBJ_RELEASE(module->c_coll.coll_gather_module);
    OBJ_RELEASE(module->c_coll.coll_gatherv_module);
    OBJ_RELEASE(module->c_coll.coll_reduce_module);
    OBJ_RELEASE(module->c_coll.coll_reduce_scatter_module);
    OBJ_RELEASE(module->c_coll.coll_scatter_module);
    OBJ_RELEASE(module->c_coll.coll_scatterv_module);
    /* If the exscan module is not NULL, then this was an
       intracommunicator, and therefore scan will have a module as
       well. */
    if (NULL != module->c_coll.coll_exscan_module) {
        OBJ_RELEASE(module->c_coll.coll_exscan_module);
        OBJ_RELEASE(module->c_coll.coll_scan_module);
    }
}

OBJ_CLASS_INSTANCE(mca_coll_sync_module_t, mca_coll_base_module_t,
                   mca_coll_sync_module_construct,
                   mca_coll_sync_module_destruct);


/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this component to disqualify itself if it doesn't support the
 * required level of thread support.
 */
int mca_coll_sync_init_query(bool enable_progress_threads,
                             bool enable_mpi_threads)
{
    /* Nothing to do */
    return OMPI_SUCCESS;
}


/*
 * Invoked when there's a new communicator that has been created.
 * Look at the communicator and decide which set of functions and
 * priority we want to return.
 */
mca_coll_base_module_t *
mca_coll_sync_comm_query(struct ompi_communicator_t *comm,
                         int *priority)
{
    mca_coll_sync_module_t *sync_module;

    /* If both MCA params are 0, then disqualify us */
    if (0 == mca_coll_sync_component.barrier_before_nops &&
        0 == mca_coll_sync_component.barrier_after_nops) {
        return NULL;
    }

    sync_module = OBJ_NEW(mca_coll_sync_module_t);
    if (NULL == sync_module) {
        return NULL;
    }

    *priority = mca_coll_sync_component.priority;

    /* Choose whether to use [intra|inter] */
    sync_module->super.coll_module_enable = mca_coll_sync_module_enable;
    sync_module->super.coll_module_disable = mca_coll_sync_module_disable;

    /* The "all" versions are already synchronous.  So no need for an
       additional barrier there. */
    sync_module->super.coll_bcast      = mca_coll_sync_bcast;
    sync_module->super.coll_exscan     = mca_coll_sync_exscan;
    sync_module->super.coll_gather     = mca_coll_sync_gather;
    sync_module->super.coll_gatherv    = mca_coll_sync_gatherv;
    sync_module->super.coll_reduce     = mca_coll_sync_reduce;
    sync_module->super.coll_reduce_scatter = mca_coll_sync_reduce_scatter;
    sync_module->super.coll_scan       = mca_coll_sync_scan;
    sync_module->super.coll_scatter    = mca_coll_sync_scatter;
    sync_module->super.coll_scatterv   = mca_coll_sync_scatterv;

    return &(sync_module->super);
}

#define SYNC_INSTALL_COLL_API(__comm, __module, __api)                                                                   \
    do                                                                                                                   \
    {                                                                                                                    \
        /* save the current selected collective */                                                                       \
        MCA_COLL_SAVE_API(__comm, __api, __module->c_coll.coll_##__api, __module->c_coll.coll_##__api##_module, "sync"); \
        /* install our own */                                                                                            \
        MCA_COLL_INSTALL_API(__comm, __api, mca_coll_sync_##__api, &__module->super, "sync");                            \
    } while (0)

#define SYNC_UNINSTALL_COLL_API(__comm, __module, __api)                                                                \
    do                                                                                                                  \
    {                                                                                                                   \
        if (__comm->c_coll->coll_##__api##_module == &__module->super)                                                  \
        {                                                                                                               \
            MCA_COLL_INSTALL_API(__comm, __api, mca_coll_sync_##__api, __module->c_coll.coll_##__api##_module, "sync"); \
            __module->c_coll.coll_##__api = NULL;                                                                       \
            __module->c_coll.coll_##__api##_module = NULL;                                                              \
        }                                                                                                               \
    } while (0)

/*
 * Init module on the communicator
 */
static int
mca_coll_sync_module_enable(mca_coll_base_module_t *module,
                            struct ompi_communicator_t *comm)
{
    mca_coll_sync_module_t *sync_module = (mca_coll_sync_module_t*) module;

    /* The "all" versions are already synchronous.  So no need for an
       additional barrier there. */
    SYNC_INSTALL_COLL_API(comm, sync_module, bcast);
    SYNC_INSTALL_COLL_API(comm, sync_module, gather);
    SYNC_INSTALL_COLL_API(comm, sync_module, gatherv);
    SYNC_INSTALL_COLL_API(comm, sync_module, reduce);
    SYNC_INSTALL_COLL_API(comm, sync_module, reduce_scatter);
    SYNC_INSTALL_COLL_API(comm, sync_module, scatter);
    SYNC_INSTALL_COLL_API(comm, sync_module, scatterv);

    if (!OMPI_COMM_IS_INTER(comm)) {
        /* MPI does not define scan/exscan on intercommunicators */
        SYNC_INSTALL_COLL_API(comm, sync_module, scan);
        SYNC_INSTALL_COLL_API(comm, sync_module, exscan);
    }

    return OMPI_SUCCESS;
}

static int
mca_coll_sync_module_disable(mca_coll_base_module_t *module,
                             struct ompi_communicator_t *comm)
{
    mca_coll_sync_module_t *sync_module = (mca_coll_sync_module_t*) module;

    /* Save the prior layer of coll functions */
    sync_module->c_coll = *comm->c_coll;

    /* The "all" versions are already synchronous.  So no need for an
       additional barrier there. */
    SYNC_UNINSTALL_COLL_API(comm, sync_module, bcast);
    SYNC_UNINSTALL_COLL_API(comm, sync_module, gather);
    SYNC_UNINSTALL_COLL_API(comm, sync_module, gatherv);
    SYNC_UNINSTALL_COLL_API(comm, sync_module, reduce);
    SYNC_UNINSTALL_COLL_API(comm, sync_module, reduce_scatter);
    SYNC_UNINSTALL_COLL_API(comm, sync_module, scatter);
    SYNC_UNINSTALL_COLL_API(comm, sync_module, scatterv);

    if (!OMPI_COMM_IS_INTER(comm)) {
        /* MPI does not define scan/exscan on intercommunicators */
        SYNC_INSTALL_COLL_API(comm, sync_module, scan);
        SYNC_INSTALL_COLL_API(comm, sync_module, exscan);
    }

    return OMPI_SUCCESS;
}
