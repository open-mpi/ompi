/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010-2012 Oak Ridge National Laboratory.
 *                         All rights reserved.
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
 * Copyright (c) 2017      FUJITSU LIMITED.  All rights reserved.
 * Copyright (c) 2020      BULL S.A.S. All rights reserved.
 * Copyright (c) 2024      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "ompi/mca/mca.h"
#include "opal/mca/base/base.h"

#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/coll/base/coll_base_util.h"

#if OPAL_ENABLE_DEBUG
#define CHECK_CLEAN_COLL(comm, func)                             \
    do {                                                         \
        if (NULL != comm->c_coll->coll_ ## func ## _module ||    \
            NULL != comm->c_coll->coll_ ## func ) {              \
            opal_output_verbose(10, ompi_coll_base_framework.framework_output,                        \
                "coll:base:comm_unselect: Comm %p (%s) has a left over %s collective during cleanup", \
                (void*)comm, comm->c_name, #func);               \
        }                                                        \
    } while (0)
#else
#define CHECK_CLEAN_COLL(comm, func)
#endif  /* OPAL_ENABLE_DEBUG */

int mca_coll_base_comm_unselect(ompi_communicator_t * comm)
{
    opal_list_item_t *item;

    /* Call module disable in the reverse order in which enable has been called
     * in order to allow the modules to properly chain themselves.
     */
    for (item = opal_list_remove_last(comm->c_coll->module_list);
         NULL != item; item = opal_list_remove_last(comm->c_coll->module_list)) {
        mca_coll_base_avail_coll_t *avail = (mca_coll_base_avail_coll_t *) item;

        if(avail->ac_module) {
            if (NULL != avail->ac_module->coll_module_disable ) {
                avail->ac_module->coll_module_disable(avail->ac_module, comm);
            }
            OBJ_RELEASE(avail->ac_module);
        }
        OBJ_RELEASE(avail);
    }
    OBJ_RELEASE(comm->c_coll->module_list);

    CHECK_CLEAN_COLL(comm, allgather);
    CHECK_CLEAN_COLL(comm, allgatherv);
    CHECK_CLEAN_COLL(comm, allreduce);
    CHECK_CLEAN_COLL(comm, alltoall);
    CHECK_CLEAN_COLL(comm, alltoallv);
    CHECK_CLEAN_COLL(comm, alltoallw);
    CHECK_CLEAN_COLL(comm, barrier);
    CHECK_CLEAN_COLL(comm, bcast);
    CHECK_CLEAN_COLL(comm, exscan);
    CHECK_CLEAN_COLL(comm, gather);
    CHECK_CLEAN_COLL(comm, gatherv);
    CHECK_CLEAN_COLL(comm, reduce);
    CHECK_CLEAN_COLL(comm, reduce_scatter_block);
    CHECK_CLEAN_COLL(comm, reduce_scatter);
    CHECK_CLEAN_COLL(comm, scan);
    CHECK_CLEAN_COLL(comm, scatter);
    CHECK_CLEAN_COLL(comm, scatterv);

    CHECK_CLEAN_COLL(comm, iallgather);
    CHECK_CLEAN_COLL(comm, iallgatherv);
    CHECK_CLEAN_COLL(comm, iallreduce);
    CHECK_CLEAN_COLL(comm, ialltoall);
    CHECK_CLEAN_COLL(comm, ialltoallv);
    CHECK_CLEAN_COLL(comm, ialltoallw);
    CHECK_CLEAN_COLL(comm, ibarrier);
    CHECK_CLEAN_COLL(comm, ibcast);
    CHECK_CLEAN_COLL(comm, iexscan);
    CHECK_CLEAN_COLL(comm, igather);
    CHECK_CLEAN_COLL(comm, igatherv);
    CHECK_CLEAN_COLL(comm, ireduce);
    CHECK_CLEAN_COLL(comm, ireduce_scatter_block);
    CHECK_CLEAN_COLL(comm, ireduce_scatter);
    CHECK_CLEAN_COLL(comm, iscan);
    CHECK_CLEAN_COLL(comm, iscatter);
    CHECK_CLEAN_COLL(comm, iscatterv);

    CHECK_CLEAN_COLL(comm, allgather_init);
    CHECK_CLEAN_COLL(comm, allgatherv_init);
    CHECK_CLEAN_COLL(comm, allreduce_init);
    CHECK_CLEAN_COLL(comm, alltoall_init);
    CHECK_CLEAN_COLL(comm, alltoallv_init);
    CHECK_CLEAN_COLL(comm, alltoallw_init);
    CHECK_CLEAN_COLL(comm, barrier_init);
    CHECK_CLEAN_COLL(comm, bcast_init);
    CHECK_CLEAN_COLL(comm, exscan_init);
    CHECK_CLEAN_COLL(comm, gather_init);
    CHECK_CLEAN_COLL(comm, gatherv_init);
    CHECK_CLEAN_COLL(comm, reduce_init);
    CHECK_CLEAN_COLL(comm, reduce_scatter_block_init);
    CHECK_CLEAN_COLL(comm, reduce_scatter_init);
    CHECK_CLEAN_COLL(comm, scan_init);
    CHECK_CLEAN_COLL(comm, scatter_init);
    CHECK_CLEAN_COLL(comm, scatterv_init);

    CHECK_CLEAN_COLL(comm, neighbor_allgather);
    CHECK_CLEAN_COLL(comm, neighbor_allgatherv);
    CHECK_CLEAN_COLL(comm, neighbor_alltoall);
    CHECK_CLEAN_COLL(comm, neighbor_alltoallv);
    CHECK_CLEAN_COLL(comm, neighbor_alltoallw);

    CHECK_CLEAN_COLL(comm, ineighbor_allgather);
    CHECK_CLEAN_COLL(comm, ineighbor_allgatherv);
    CHECK_CLEAN_COLL(comm, ineighbor_alltoall);
    CHECK_CLEAN_COLL(comm, ineighbor_alltoallv);
    CHECK_CLEAN_COLL(comm, ineighbor_alltoallw);

    CHECK_CLEAN_COLL(comm, neighbor_allgather_init);
    CHECK_CLEAN_COLL(comm, neighbor_allgatherv_init);
    CHECK_CLEAN_COLL(comm, neighbor_alltoall_init);
    CHECK_CLEAN_COLL(comm, neighbor_alltoallv_init);
    CHECK_CLEAN_COLL(comm, neighbor_alltoallw_init);

    CHECK_CLEAN_COLL(comm, reduce_local);

#if OPAL_ENABLE_FT_MPI
    CHECK_CLEAN_COLL(comm, agree);
    CHECK_CLEAN_COLL(comm, iagree);
#endif

    free(comm->c_coll);
    comm->c_coll = NULL;

    /* All done */
    return OMPI_SUCCESS;
}

