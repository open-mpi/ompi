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

#if 0

/*
 * Linear set of collective algorithms
 */
static const mca_coll_base_module_1_0_0_t intra = {

    /* Initialization / finalization functions */

    mca_coll_demo_module_init,
    mca_coll_demo_module_finalize,

    /* Collective function pointers */

    mca_coll_demo_allgather_intra,
    mca_coll_demo_allgatherv_intra,
    mca_coll_demo_allreduce_intra,
    mca_coll_demo_alltoall_intra,
    mca_coll_demo_alltoallv_intra,
    mca_coll_demo_alltoallw_intra,
    mca_coll_demo_barrier_intra,
    mca_coll_demo_bcast_intra,
    NULL, /* Leave exscan blank just to force basic to be used */
    mca_coll_demo_gather_intra,
    mca_coll_demo_gatherv_intra,
    mca_coll_demo_reduce_intra,
    mca_coll_demo_reduce_scatter_intra,
    mca_coll_demo_scan_intra,
    mca_coll_demo_scatter_intra,
    mca_coll_demo_scatterv_intra,
    mca_coll_demo_ft_event
};


/*
 * Linear set of collective algorithms for intercommunicators
 */
static const mca_coll_base_module_1_0_0_t inter = {

    /* Initialization / finalization functions */

    mca_coll_demo_module_init,
    mca_coll_demo_module_finalize,

    /* Collective function pointers */

    mca_coll_demo_allgather_inter,
    mca_coll_demo_allgatherv_inter,
    mca_coll_demo_allreduce_inter,
    mca_coll_demo_alltoall_inter,
    mca_coll_demo_alltoallv_inter,
    mca_coll_demo_alltoallw_inter,
    mca_coll_demo_barrier_inter,
    mca_coll_demo_bcast_inter,
    mca_coll_demo_exscan_inter,
    mca_coll_demo_gather_inter,
    mca_coll_demo_gatherv_inter,
    mca_coll_demo_reduce_inter,
    mca_coll_demo_reduce_scatter_inter,
    NULL,
    mca_coll_demo_scatter_inter,
    mca_coll_demo_scatterv_inter,
    mca_coll_demo_ft_event
};

#endif

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

    if (OMPI_SUCCESS != 
        mca_base_param_lookup_int(mca_coll_demo_priority_param, priority)) {
        return NULL;
    }

    demo_module->super.coll_module_enable = mca_coll_demo_module_enable;
    demo_module->super.ft_event = mca_coll_demo_ft_event;

    if (OMPI_COMM_IS_INTRA(comm)) {
        demo_module->super.coll_allgather  = mca_coll_demo_allgather_intra;
        demo_module->super.coll_allgatherv = mca_coll_demo_allgatherv_intra;
        demo_module->super.coll_allreduce  = mca_coll_demo_allreduce_intra;
        demo_module->super.coll_alltoall   = mca_coll_demo_alltoall_intra;
        demo_module->super.coll_alltoallv  = mca_coll_demo_alltoallv_intra;
        demo_module->super.coll_alltoallw  = mca_coll_demo_alltoallw_intra;
        demo_module->super.coll_barrier    = mca_coll_demo_barrier_intra;
        demo_module->super.coll_bcast      = mca_coll_demo_bcast_intra;
        demo_module->super.coll_exscan     = mca_coll_demo_exscan_intra;
        demo_module->super.coll_gather     = mca_coll_demo_gather_intra;
        demo_module->super.coll_gatherv    = mca_coll_demo_gatherv_intra;
        demo_module->super.coll_reduce     = mca_coll_demo_reduce_intra;
        demo_module->super.coll_reduce_scatter = mca_coll_demo_reduce_scatter_intra;
        demo_module->super.coll_scan       = mca_coll_demo_scan_intra;
        demo_module->super.coll_scatter    = mca_coll_demo_scatter_intra;
        demo_module->super.coll_scatterv   = mca_coll_demo_scatterv_intra;
    } else {
        demo_module->super.coll_allgather  = mca_coll_demo_allgather_inter;
        demo_module->super.coll_allgatherv = mca_coll_demo_allgatherv_inter;
        demo_module->super.coll_allreduce  = mca_coll_demo_allreduce_inter;
        demo_module->super.coll_alltoall   = mca_coll_demo_alltoall_inter;
        demo_module->super.coll_alltoallv  = mca_coll_demo_alltoallv_inter;
        demo_module->super.coll_alltoallw  = mca_coll_demo_alltoallw_inter;
        demo_module->super.coll_barrier    = mca_coll_demo_barrier_inter;
        demo_module->super.coll_bcast      = mca_coll_demo_bcast_inter;
        demo_module->super.coll_exscan     = NULL;
        demo_module->super.coll_gather     = mca_coll_demo_gather_inter;
        demo_module->super.coll_gatherv    = mca_coll_demo_gatherv_inter;
        demo_module->super.coll_reduce     = mca_coll_demo_reduce_inter;
        demo_module->super.coll_reduce_scatter = mca_coll_demo_reduce_scatter_inter;
        demo_module->super.coll_scan       = NULL;
        demo_module->super.coll_scatter    = mca_coll_demo_scatter_inter;
        demo_module->super.coll_scatterv   = mca_coll_demo_scatterv_inter;
    }

    return &(demo_module->super);
}

#define COPY(comm, module, func)                                        \
    do {                                                                \
        module->underlying.coll_ ## func  = comm->c_coll.coll_ ## func; \
        module->underlying.coll_ ## func  = comm->c_coll.coll_ ## func; \
        if (NULL != module->underlying.coll_ ## func ## _module) {      \
            OBJ_RETAIN(module->underlying.coll_ ## func ## _module);    \
        }                                                               \
    } while (0)

int
mca_coll_demo_module_enable(mca_coll_base_module_t *module,
                            struct ompi_communicator_t *comm)
{
    mca_coll_demo_module_t *demo_module = (mca_coll_demo_module_t*) module;

    mca_base_param_lookup_int(mca_coll_demo_verbose_param,
                              &mca_coll_demo_verbose);
    if (mca_coll_demo_verbose > 0) {
        printf("Hello!  This is the \"demo\" coll component.  I'll be your coll component\ntoday.  Please tip your waitresses well.\n");
    }

    /* save the old pointers */
    COPY(comm, demo_module, allgather); 
    COPY(comm, demo_module, allgatherv); 
    COPY(comm, demo_module, allreduce); 
    COPY(comm, demo_module, alltoall); 
    COPY(comm, demo_module, alltoallv); 
    COPY(comm, demo_module, alltoallw); 
    COPY(comm, demo_module, barrier); 
    COPY(comm, demo_module, bcast); 
    COPY(comm, demo_module, exscan); 
    COPY(comm, demo_module, gather); 
    COPY(comm, demo_module, gatherv); 
    COPY(comm, demo_module, reduce); 
    COPY(comm, demo_module, reduce_scatter); 
    COPY(comm, demo_module, scan); 
    COPY(comm, demo_module, scatter); 
    COPY(comm, demo_module, scatterv); 

    return OMPI_SUCCESS;
}


int mca_coll_demo_ft_event(int state) {
    if(OPAL_CRS_CHECKPOINT == state) {
        ;
    }
    else if(OPAL_CRS_CONTINUE == state) {
        ;
    }
    else if(OPAL_CRS_RESTART == state) {
        ;
    }
    else if(OPAL_CRS_TERM == state ) {
        ;
    }
    else {
        ;
    }

    return OMPI_SUCCESS;
}
