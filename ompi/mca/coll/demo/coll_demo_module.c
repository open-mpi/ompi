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
 * Copyright (c) 2024      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_demo.h"

#include <stdio.h>

#include "mpi.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"
#include "coll_demo.h"

/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this component to disqualify itself if it doesn't support the
 * required level of thread support.
 */
int mca_coll_demo_init_query(bool enable_progress_threads,
                             bool enable_mpi_threads)
{
    /* Nothing to do */

    return OMPI_SUCCESS;
}

static int
mca_coll_demo_module_enable(mca_coll_base_module_t *module,
                            struct ompi_communicator_t *comm);
static int
mca_coll_demo_module_disable(mca_coll_base_module_t *module,
                             struct ompi_communicator_t *comm);

/*
 * Invoked when there's a new communicator that has been created.
 * Look at the communicator and decide which set of functions and
 * priority we want to return.
 */
mca_coll_base_module_t *
mca_coll_demo_comm_query(struct ompi_communicator_t *comm, int *priority)
{
    mca_coll_demo_module_t *demo_module;

    demo_module = OBJ_NEW(mca_coll_demo_module_t);
    if (NULL == demo_module) return NULL;

    *priority = mca_coll_demo_priority;

    demo_module->super.coll_module_enable = mca_coll_demo_module_enable;
    demo_module->super.coll_module_disable = mca_coll_demo_module_disable;

    return &(demo_module->super);
}

#define DEMO_INSTALL_COLL_API(__comm, __module, __api, __func)                                                                   \
    do                                                                                                                           \
    {                                                                                                                            \
        if (__comm->c_coll->coll_##__api)                                                                                        \
        {                                                                                                                        \
            /* save the current selected collective */                                                                           \
            MCA_COLL_SAVE_API(__comm, __api, (__module)->c_coll.coll_##__api, (__module)->c_coll.coll_##__api##_module, "demo"); \
            /* install our own */                                                                                                \
            MCA_COLL_INSTALL_API(__comm, __api, __func, &__module->super, "demo");                                               \
        }                                                                                                                        \
        else                                                                                                                     \
        {                                                                                                                        \
            opal_show_help("help-mca-coll-base.txt", "comm-select:missing collective", true,                                     \
                           "demo", #__api, ompi_process_info.nodename,                                                           \
                           mca_coll_demo_priority);                                                                              \
        }                                                                                                                        \
    }    while (0)

#define DEMO_UNINSTALL_COLL_API(__comm, __module, __api)                                                                        \
    do                                                                                                                          \
    {                                                                                                                           \
        if (&(__module)->super == __comm->c_coll->coll_##__api##_module)                                                        \
        {                                                                                                                       \
            /* put back the original collective */                                                                              \
            MCA_COLL_INSTALL_API(__comm, __api, __module->c_coll.coll_##__api, __module->c_coll.coll_##__api##_module, "demo"); \
        }                                                                                                                       \
     } while (0)

int
mca_coll_demo_module_enable(mca_coll_base_module_t *module,
                            struct ompi_communicator_t *comm)
{
    mca_coll_demo_module_t *demo_module = (mca_coll_demo_module_t*) module;

    if (mca_coll_demo_verbose > 0) {
        printf("Hello!  This is the \"demo\" coll component.  I'll be your coll component\ntoday.  Please tip your waitresses well.\n");
    }

    if (OMPI_COMM_IS_INTRA(comm))
    {
        DEMO_INSTALL_COLL_API(comm, demo_module, allgather, mca_coll_demo_allgather_intra);
        DEMO_INSTALL_COLL_API(comm, demo_module, allgatherv, mca_coll_demo_allgatherv_intra);
        DEMO_INSTALL_COLL_API(comm, demo_module, allreduce, mca_coll_demo_allreduce_intra);
        DEMO_INSTALL_COLL_API(comm, demo_module, alltoall, mca_coll_demo_alltoall_intra);
        DEMO_INSTALL_COLL_API(comm, demo_module, alltoallv, mca_coll_demo_alltoallv_intra);
        DEMO_INSTALL_COLL_API(comm, demo_module, alltoallw, mca_coll_demo_alltoallw_intra);
        DEMO_INSTALL_COLL_API(comm, demo_module, barrier, mca_coll_demo_barrier_intra);
        DEMO_INSTALL_COLL_API(comm, demo_module, bcast, mca_coll_demo_bcast_intra);
        DEMO_INSTALL_COLL_API(comm, demo_module, exscan, mca_coll_demo_exscan_intra);
        DEMO_INSTALL_COLL_API(comm, demo_module, gather, mca_coll_demo_gather_intra);
        DEMO_INSTALL_COLL_API(comm, demo_module, gatherv, mca_coll_demo_gatherv_intra);
        DEMO_INSTALL_COLL_API(comm, demo_module, reduce, mca_coll_demo_reduce_intra);
        DEMO_INSTALL_COLL_API(comm, demo_module, reduce_scatter, mca_coll_demo_reduce_scatter_intra);
        DEMO_INSTALL_COLL_API(comm, demo_module, scan, mca_coll_demo_scan_intra);
        DEMO_INSTALL_COLL_API(comm, demo_module, scatter, mca_coll_demo_scatter_intra);
        DEMO_INSTALL_COLL_API(comm, demo_module, scatterv, mca_coll_demo_scatterv_intra);
    }
    else
    {
        DEMO_INSTALL_COLL_API(comm, demo_module, allgather, mca_coll_demo_allgather_inter);
        DEMO_INSTALL_COLL_API(comm, demo_module, allgatherv, mca_coll_demo_allgatherv_inter);
        DEMO_INSTALL_COLL_API(comm, demo_module, allreduce, mca_coll_demo_allreduce_inter);
        DEMO_INSTALL_COLL_API(comm, demo_module, alltoall, mca_coll_demo_alltoall_inter);
        DEMO_INSTALL_COLL_API(comm, demo_module, alltoallv, mca_coll_demo_alltoallv_inter);
        DEMO_INSTALL_COLL_API(comm, demo_module, alltoallw, mca_coll_demo_alltoallw_inter);
        DEMO_INSTALL_COLL_API(comm, demo_module, barrier, mca_coll_demo_barrier_inter);
        DEMO_INSTALL_COLL_API(comm, demo_module, bcast, mca_coll_demo_bcast_inter);
        /* Skip the exscan for inter-comms, it is not supported */
        DEMO_INSTALL_COLL_API(comm, demo_module, gather, mca_coll_demo_gather_inter);
        DEMO_INSTALL_COLL_API(comm, demo_module, gatherv, mca_coll_demo_gatherv_inter);
        DEMO_INSTALL_COLL_API(comm, demo_module, reduce, mca_coll_demo_reduce_inter);
        DEMO_INSTALL_COLL_API(comm, demo_module, reduce_scatter, mca_coll_demo_reduce_scatter_inter);
        /* Skip the scan for inter-comms, it is not supported */
        DEMO_INSTALL_COLL_API(comm, demo_module, scatter, mca_coll_demo_scatter_inter);
        DEMO_INSTALL_COLL_API(comm, demo_module, scatterv, mca_coll_demo_scatterv_inter);
    }
    return OMPI_SUCCESS;
}

static int
mca_coll_demo_module_disable(mca_coll_base_module_t *module,
                             struct ompi_communicator_t *comm)
{
    mca_coll_demo_module_t *demo_module = (mca_coll_demo_module_t*) module;

    /* put back the old pointers */
    DEMO_UNINSTALL_COLL_API(comm, demo_module, allgather);
    DEMO_UNINSTALL_COLL_API(comm, demo_module, allgatherv);
    DEMO_UNINSTALL_COLL_API(comm, demo_module, allreduce);
    DEMO_UNINSTALL_COLL_API(comm, demo_module, alltoall);
    DEMO_UNINSTALL_COLL_API(comm, demo_module, alltoallv);
    DEMO_UNINSTALL_COLL_API(comm, demo_module, alltoallw);
    DEMO_UNINSTALL_COLL_API(comm, demo_module, barrier);
    DEMO_UNINSTALL_COLL_API(comm, demo_module, bcast);
    DEMO_UNINSTALL_COLL_API(comm, demo_module, exscan);
    DEMO_UNINSTALL_COLL_API(comm, demo_module, gather);
    DEMO_UNINSTALL_COLL_API(comm, demo_module, gatherv);
    DEMO_UNINSTALL_COLL_API(comm, demo_module, reduce);
    DEMO_UNINSTALL_COLL_API(comm, demo_module, reduce_scatter);
    DEMO_UNINSTALL_COLL_API(comm, demo_module, scan);
    DEMO_UNINSTALL_COLL_API(comm, demo_module, scatter);
    DEMO_UNINSTALL_COLL_API(comm, demo_module, scatterv);

    return OMPI_SUCCESS;
}
