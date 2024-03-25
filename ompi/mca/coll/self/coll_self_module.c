/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
 * Copyright (c) 2024      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_self.h"

#include <stdio.h>

#include "mpi.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "coll_self.h"

static int
mca_coll_self_module_enable(mca_coll_base_module_t *module,
                            struct ompi_communicator_t *comm);
static int
mca_coll_self_module_disable(mca_coll_base_module_t *module,
                             struct ompi_communicator_t *comm);

/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this module to indicate what level of thread support it provides.
 */
int mca_coll_self_init_query(bool enable_progress_threads,
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
mca_coll_self_comm_query(struct ompi_communicator_t *comm,
                         int *priority)
{
    mca_coll_self_module_t *module;

    /* We only work on intracommunicators of size 1 */

    if (!OMPI_COMM_IS_INTER(comm) && 1 == ompi_comm_size(comm)) {
        *priority = ompi_coll_self_priority;

        module = OBJ_NEW(mca_coll_self_module_t);
        if (NULL == module) return NULL;

        module->super.coll_module_enable = mca_coll_self_module_enable;
        module->super.coll_module_disable = mca_coll_self_module_disable;

        module->super.coll_reduce_local = mca_coll_base_reduce_local;

        return &(module->super);
    }

    return NULL;
}

#define SELF_INSTALL_COLL_API(__comm, __module, __api)                                                 \
    do                                                                                                 \
    {                                                                                                  \
        MCA_COLL_INSTALL_API(__comm, __api, mca_coll_self_##__api##_intra, &__module->super, "self");  \
    } while (0)

#define SELF_UNINSTALL_COLL_API(__comm, __module, __api)                \
    do                                                                  \
    {                                                                   \
        if (__comm->c_coll->coll_##__api##_module == &__module->super)  \
        {                                                               \
            MCA_COLL_INSTALL_API(__comm, __api, NULL, NULL, "self");    \
        }                                                               \
    } while (0)

/*
 * Init module on the communicator
 */
static int
mca_coll_self_module_enable(mca_coll_base_module_t *module,
                            struct ompi_communicator_t *comm)
{
    mca_coll_self_module_t *sm_module = (mca_coll_self_module_t*)module;

    SELF_INSTALL_COLL_API(comm, sm_module, allgather);
    SELF_INSTALL_COLL_API(comm, sm_module, allgatherv);
    SELF_INSTALL_COLL_API(comm, sm_module, allreduce);
    SELF_INSTALL_COLL_API(comm, sm_module, alltoall);
    SELF_INSTALL_COLL_API(comm, sm_module, alltoallv);
    SELF_INSTALL_COLL_API(comm, sm_module, alltoallw);
    SELF_INSTALL_COLL_API(comm, sm_module, barrier);
    SELF_INSTALL_COLL_API(comm, sm_module, bcast);
    SELF_INSTALL_COLL_API(comm, sm_module, exscan);
    SELF_INSTALL_COLL_API(comm, sm_module, gather);
    SELF_INSTALL_COLL_API(comm, sm_module, gatherv);
    SELF_INSTALL_COLL_API(comm, sm_module, reduce);
    SELF_INSTALL_COLL_API(comm, sm_module, reduce_scatter);
    SELF_INSTALL_COLL_API(comm, sm_module, scan);
    SELF_INSTALL_COLL_API(comm, sm_module, scatter);
    SELF_INSTALL_COLL_API(comm, sm_module, scatterv);

    MCA_COLL_INSTALL_API(comm, reduce_local, mca_coll_base_reduce_local, module, "self");
    return OMPI_SUCCESS;
}

static int
mca_coll_self_module_disable(mca_coll_base_module_t *module,
                             struct ompi_communicator_t *comm)
{
    mca_coll_self_module_t *sm_module = (mca_coll_self_module_t *)module;

    SELF_UNINSTALL_COLL_API(comm, sm_module, allgather);
    SELF_UNINSTALL_COLL_API(comm, sm_module, allgatherv);
    SELF_UNINSTALL_COLL_API(comm, sm_module, allreduce);
    SELF_UNINSTALL_COLL_API(comm, sm_module, alltoall);
    SELF_UNINSTALL_COLL_API(comm, sm_module, alltoallv);
    SELF_UNINSTALL_COLL_API(comm, sm_module, alltoallw);
    SELF_UNINSTALL_COLL_API(comm, sm_module, barrier);
    SELF_UNINSTALL_COLL_API(comm, sm_module, bcast);
    SELF_UNINSTALL_COLL_API(comm, sm_module, exscan);
    SELF_UNINSTALL_COLL_API(comm, sm_module, gather);
    SELF_UNINSTALL_COLL_API(comm, sm_module, gatherv);
    SELF_UNINSTALL_COLL_API(comm, sm_module, reduce);
    SELF_UNINSTALL_COLL_API(comm, sm_module, reduce_scatter);
    SELF_UNINSTALL_COLL_API(comm, sm_module, scan);
    SELF_UNINSTALL_COLL_API(comm, sm_module, scatter);
    SELF_UNINSTALL_COLL_API(comm, sm_module, scatterv);

    return OMPI_SUCCESS;
}
