/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010-2012 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2012      Sandia National Laboratories. All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016-2017 IBM Corporation.  All rights reserved.
 * Copyright (c) 2024      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_basic.h"

#include <stdio.h>

#include "mpi.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"
#include "coll_basic.h"


static int
mca_coll_basic_module_enable(mca_coll_base_module_t *module,
                             struct ompi_communicator_t *comm);
static int
mca_coll_basic_module_disable(mca_coll_base_module_t *module,
                             struct ompi_communicator_t *comm);

/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this component to disqualify itself if it doesn't support the
 * required level of thread support.
 */
int
mca_coll_basic_init_query(bool enable_progress_threads,
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
mca_coll_basic_comm_query(struct ompi_communicator_t *comm,
                          int *priority)
{
    mca_coll_basic_module_t *basic_module;

    basic_module = OBJ_NEW(mca_coll_basic_module_t);
    if (NULL == basic_module) return NULL;

    *priority = mca_coll_basic_priority;

    /* Choose whether to use [intra|inter], and [linear|log]-based
     * algorithms. */
    basic_module->super.coll_module_enable = mca_coll_basic_module_enable;
    basic_module->super.coll_module_disable = mca_coll_basic_module_disable;

    return &(basic_module->super);
}


#define BASIC_INSTALL_COLL_API(__comm, __module, __api, __coll)                 \
    do                                                                          \
    {                                                                           \
        (__module)->super.coll_##__api = __coll;                                \
        MCA_COLL_INSTALL_API(__comm, __api, __coll, &__module->super, "basic"); \
    } while (0)

/*
 * Init module on the communicator
 */
int
mca_coll_basic_module_enable(mca_coll_base_module_t *module,
                             struct ompi_communicator_t *comm)
{
    mca_coll_basic_module_t *basic_module = (mca_coll_basic_module_t*)module;

    /* prepare the placeholder for the array of request* */
    module->base_data = OBJ_NEW(mca_coll_base_comm_t);
    if (NULL == module->base_data) {
        return OMPI_ERROR;
    }

    if (OMPI_COMM_IS_INTER(comm))
    {
        BASIC_INSTALL_COLL_API(comm, basic_module, allgather, mca_coll_basic_allgather_inter);
        BASIC_INSTALL_COLL_API(comm, basic_module, allgatherv, mca_coll_basic_allgatherv_inter);
        BASIC_INSTALL_COLL_API(comm, basic_module, allreduce, mca_coll_basic_allreduce_inter);
        BASIC_INSTALL_COLL_API(comm, basic_module, alltoall, mca_coll_basic_alltoall_inter);
        BASIC_INSTALL_COLL_API(comm, basic_module, alltoallv, mca_coll_basic_alltoallv_inter);
        BASIC_INSTALL_COLL_API(comm, basic_module, alltoallw, mca_coll_basic_alltoallw_inter);
        BASIC_INSTALL_COLL_API(comm, basic_module, barrier, mca_coll_basic_barrier_inter_lin);
        BASIC_INSTALL_COLL_API(comm, basic_module, bcast, mca_coll_basic_bcast_lin_inter);
        BASIC_INSTALL_COLL_API(comm, basic_module, gather, mca_coll_basic_gather_inter);
        BASIC_INSTALL_COLL_API(comm, basic_module, gatherv, mca_coll_basic_gatherv_inter);
        BASIC_INSTALL_COLL_API(comm, basic_module, reduce, mca_coll_basic_reduce_lin_inter);
        BASIC_INSTALL_COLL_API(comm, basic_module, reduce_scatter_block, mca_coll_basic_reduce_scatter_block_inter);
        BASIC_INSTALL_COLL_API(comm, basic_module, reduce_scatter, mca_coll_basic_reduce_scatter_inter);
        BASIC_INSTALL_COLL_API(comm, basic_module, scatter, mca_coll_basic_scatter_inter);
        BASIC_INSTALL_COLL_API(comm, basic_module, scatterv, mca_coll_basic_scatterv_inter);
    }
    else {
        if (ompi_comm_size(comm) <= mca_coll_basic_crossover) {
            BASIC_INSTALL_COLL_API(comm, basic_module, barrier, ompi_coll_base_barrier_intra_basic_linear);
            BASIC_INSTALL_COLL_API(comm, basic_module, bcast, ompi_coll_base_bcast_intra_basic_linear);
            BASIC_INSTALL_COLL_API(comm, basic_module, reduce, ompi_coll_base_reduce_intra_basic_linear);
        } else {
            BASIC_INSTALL_COLL_API(comm, basic_module, barrier, mca_coll_basic_barrier_intra_log);
            BASIC_INSTALL_COLL_API(comm, basic_module, bcast, mca_coll_basic_bcast_log_intra);
            BASIC_INSTALL_COLL_API(comm, basic_module, reduce, mca_coll_basic_reduce_log_intra);
        }
        BASIC_INSTALL_COLL_API(comm, basic_module, allgather, ompi_coll_base_allgather_intra_basic_linear);
        BASIC_INSTALL_COLL_API(comm, basic_module, allgatherv, ompi_coll_base_allgatherv_intra_basic_default);
        BASIC_INSTALL_COLL_API(comm, basic_module, allreduce, mca_coll_basic_allreduce_intra);
        BASIC_INSTALL_COLL_API(comm, basic_module, alltoall, ompi_coll_base_alltoall_intra_basic_linear);
        BASIC_INSTALL_COLL_API(comm, basic_module, alltoallv, ompi_coll_base_alltoallv_intra_basic_linear);
        BASIC_INSTALL_COLL_API(comm, basic_module, alltoallw, mca_coll_basic_alltoallw_intra);
        BASIC_INSTALL_COLL_API(comm, basic_module, exscan, ompi_coll_base_exscan_intra_linear);
        BASIC_INSTALL_COLL_API(comm, basic_module, gather, ompi_coll_base_gather_intra_basic_linear);
        BASIC_INSTALL_COLL_API(comm, basic_module, gatherv, mca_coll_basic_gatherv_intra);
        BASIC_INSTALL_COLL_API(comm, basic_module, reduce_scatter_block, mca_coll_basic_reduce_scatter_block_intra);
        BASIC_INSTALL_COLL_API(comm, basic_module, reduce_scatter, mca_coll_basic_reduce_scatter_intra);
        BASIC_INSTALL_COLL_API(comm, basic_module, scan, ompi_coll_base_scan_intra_linear);
        BASIC_INSTALL_COLL_API(comm, basic_module, scatter, ompi_coll_base_scatter_intra_basic_linear);
        BASIC_INSTALL_COLL_API(comm, basic_module, scatterv, mca_coll_basic_scatterv_intra);
    }
    /* These functions will return an error code if comm does not have a virtual topology */
    BASIC_INSTALL_COLL_API(comm, basic_module, neighbor_allgather, mca_coll_basic_neighbor_allgather);
    BASIC_INSTALL_COLL_API(comm, basic_module, neighbor_allgatherv, mca_coll_basic_neighbor_allgatherv);
    BASIC_INSTALL_COLL_API(comm, basic_module, neighbor_alltoall, mca_coll_basic_neighbor_alltoall);
    BASIC_INSTALL_COLL_API(comm, basic_module, neighbor_alltoallv, mca_coll_basic_neighbor_alltoallv);
    BASIC_INSTALL_COLL_API(comm, basic_module, neighbor_alltoallw, mca_coll_basic_neighbor_alltoallw);

    /* Default to some shim mappings over allreduce */
    BASIC_INSTALL_COLL_API(comm, basic_module, agree, ompi_coll_base_agree_noft);
    BASIC_INSTALL_COLL_API(comm, basic_module, iagree, ompi_coll_base_iagree_noft);

    BASIC_INSTALL_COLL_API(comm, basic_module, reduce_local, mca_coll_base_reduce_local);

    /* All done */
    return OMPI_SUCCESS;
}

#define BASIC_UNINSTALL_COLL_API(__comm, __module, __api)                 \
    do                                                                    \
    {                                                                     \
        if (__comm->c_coll->coll_##__api##_module == &__module->super ) { \
            MCA_COLL_INSTALL_API(__comm, __api, NULL, NULL, "basic");     \
        }                                                                 \
    } while (0)
    
int
mca_coll_basic_module_disable(mca_coll_base_module_t *module,
                              struct ompi_communicator_t *comm)
{
    mca_coll_basic_module_t *basic_module = (mca_coll_basic_module_t*)module;

    if (OMPI_COMM_IS_INTER(comm))
    {
        BASIC_UNINSTALL_COLL_API(comm, basic_module, allgather);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, allgatherv);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, allreduce);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, alltoall);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, alltoallv);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, alltoallw);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, barrier);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, bcast);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, gather);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, gatherv);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, reduce);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, reduce_scatter_block);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, reduce_scatter);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, scatter);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, scatterv);
    }
    else if (ompi_comm_size(comm) <= mca_coll_basic_crossover)
    {
        BASIC_UNINSTALL_COLL_API(comm, basic_module, allgather);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, allgatherv);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, allreduce);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, alltoall);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, alltoallv);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, alltoallw);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, barrier);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, bcast);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, exscan);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, gather);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, gatherv);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, reduce);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, reduce_scatter_block);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, reduce_scatter);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, scan);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, scatter);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, scatterv);
    }
    else
    {
        BASIC_UNINSTALL_COLL_API(comm, basic_module, allgather);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, allgatherv);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, allreduce);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, alltoall);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, alltoallv);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, alltoallw);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, barrier);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, bcast);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, exscan);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, gather);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, gatherv);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, reduce);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, reduce_scatter_block);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, reduce_scatter);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, scan);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, scatter);
        BASIC_UNINSTALL_COLL_API(comm, basic_module, scatterv);
    }

    /* These functions will return an error code if comm does not have a virtual topology */
    BASIC_UNINSTALL_COLL_API(comm, basic_module, neighbor_allgather);
    BASIC_UNINSTALL_COLL_API(comm, basic_module, neighbor_allgatherv);
    BASIC_UNINSTALL_COLL_API(comm, basic_module, neighbor_alltoall);
    BASIC_UNINSTALL_COLL_API(comm, basic_module, neighbor_alltoallv);
    BASIC_UNINSTALL_COLL_API(comm, basic_module, neighbor_alltoallw);

    BASIC_UNINSTALL_COLL_API(comm, basic_module, agree);
    BASIC_UNINSTALL_COLL_API(comm, basic_module, iagree);

    BASIC_UNINSTALL_COLL_API(comm, basic_module, reduce_local);
    /* All done */
    return OMPI_SUCCESS;
}
