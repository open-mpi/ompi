/*
 * Copyright (c) 2024      NVIDIA Corporation. All rights reserved.
 * Copyright (c) 2014-2017 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2014-2024 NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2023      Advanced Micro Devices, Inc. All rights reserved.
 * Copyright (c) 2024      Triad National Security, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>
#include <stdio.h>

#include "coll_accelerator.h"

#include "mpi.h"


#include "opal/util/show_help.h"

#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"
#include "coll_accelerator.h"

static int
mca_coll_accelerator_module_enable(mca_coll_base_module_t *module,
                                   struct ompi_communicator_t *comm);
static int
mca_coll_accelerator_module_disable(mca_coll_base_module_t *module,
                                    struct ompi_communicator_t *comm);

static void mca_coll_accelerator_module_construct(mca_coll_accelerator_module_t *module)
{
    memset(&(module->c_coll), 0, sizeof(module->c_coll));
}

OBJ_CLASS_INSTANCE(mca_coll_accelerator_module_t, mca_coll_base_module_t,
                   mca_coll_accelerator_module_construct,
                   NULL);


/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this component to disqualify itself if it doesn't support the
 * required level of thread support.
 */
int mca_coll_accelerator_init_query(bool enable_progress_threads,
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
mca_coll_accelerator_comm_query(struct ompi_communicator_t *comm,
                         int *priority)
{
    mca_coll_accelerator_module_t *accelerator_module;

    if (0 == strcmp(opal_accelerator_base_selected_component.base_version.mca_component_name,
                    "null")) {
        opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                          "coll:accelerator:comm_query: accelerator component is null: disqualifying myself");
        return NULL;
    }

    accelerator_module = OBJ_NEW(mca_coll_accelerator_module_t);
    if (NULL == accelerator_module) {
        return NULL;
    }

    *priority = mca_coll_accelerator_component.priority;

    /* Choose whether to use [intra|inter] */
    accelerator_module->super.coll_module_enable = mca_coll_accelerator_module_enable;
    accelerator_module->super.coll_module_disable = mca_coll_accelerator_module_disable;

    accelerator_module->super.coll_allreduce  = mca_coll_accelerator_allreduce;
    accelerator_module->super.coll_reduce     = mca_coll_accelerator_reduce;
    accelerator_module->super.coll_reduce_local         = mca_coll_accelerator_reduce_local;
    accelerator_module->super.coll_reduce_scatter_block = mca_coll_accelerator_reduce_scatter_block;
    if (!OMPI_COMM_IS_INTER(comm)) {
        accelerator_module->super.coll_scan       = mca_coll_accelerator_scan;
        accelerator_module->super.coll_exscan     = mca_coll_accelerator_exscan;
    }

    return &(accelerator_module->super);
}


#define  ACCELERATOR_INSTALL_COLL_API(__comm, __module, __api) \
    do \
    { \
        if ((__comm)->c_coll->coll_##__api) \
        { \
            MCA_COLL_SAVE_API(__comm, __api, (__module)->c_coll.coll_##__api, (__module)->c_coll.coll_##__api##_module, "accelerator"); \
            MCA_COLL_INSTALL_API(__comm, __api, mca_coll_accelerator_##__api, &__module->super, "accelerator"); \
        } \
        else \
        { \
            opal_show_help("help-mca-coll-base.txt", "comm-select:missing collective", true, \
                           "cuda", #__api, ompi_process_info.nodename, \
                           mca_coll_accelerator_component.priority); \
        } \
    } while (0)

#define ACCELERATOR_UNINSTALL_COLL_API(__comm, __module, __api) \
    do \
    { \
        if (&(__module)->super == (__comm)->c_coll->coll_##__api##_module) { \
            MCA_COLL_INSTALL_API(__comm, __api, (__module)->c_coll.coll_##__api, (__module)->c_coll.coll_##__api##_module, "accelerator"); \
            (__module)->c_coll.coll_##__api##_module = NULL; \
            (__module)->c_coll.coll_##__api = NULL; \
        } \
    } while (0)

/*
 * Init/Fini module on the communicator
 */
static int
mca_coll_accelerator_module_enable(mca_coll_base_module_t *module,
                                   struct ompi_communicator_t *comm)
{
    mca_coll_accelerator_module_t *s = (mca_coll_accelerator_module_t*) module;

    ACCELERATOR_INSTALL_COLL_API(comm, s, allreduce);
    ACCELERATOR_INSTALL_COLL_API(comm, s, reduce);
    ACCELERATOR_INSTALL_COLL_API(comm, s, reduce_local);
    ACCELERATOR_INSTALL_COLL_API(comm, s, reduce_scatter_block);
    if (!OMPI_COMM_IS_INTER(comm)) {
        /* MPI does not define scan/exscan on intercommunicators */
        ACCELERATOR_INSTALL_COLL_API(comm, s, exscan);
        ACCELERATOR_INSTALL_COLL_API(comm, s, scan);
    }

    return OMPI_SUCCESS;
}

static int
mca_coll_accelerator_module_disable(mca_coll_base_module_t *module,
                                    struct ompi_communicator_t *comm)
{
    mca_coll_accelerator_module_t *s = (mca_coll_accelerator_module_t*) module;

    ACCELERATOR_UNINSTALL_COLL_API(comm, s, allreduce);
    ACCELERATOR_UNINSTALL_COLL_API(comm, s, reduce);
    ACCELERATOR_UNINSTALL_COLL_API(comm, s, reduce_local);
    ACCELERATOR_UNINSTALL_COLL_API(comm, s, reduce_scatter_block);
    if (!OMPI_COMM_IS_INTER(comm))
    {
        /* MPI does not define scan/exscan on intercommunicators */
        ACCELERATOR_UNINSTALL_COLL_API(comm, s, exscan);
        ACCELERATOR_UNINSTALL_COLL_API(comm, s, scan);
    }

    return OMPI_SUCCESS;
}
