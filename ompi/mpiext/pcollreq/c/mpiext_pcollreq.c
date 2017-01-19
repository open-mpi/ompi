/*
 * Copyright (c) 2017      FUJITSU LIMITED.  All rights reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "ompi_config.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "ompi/communicator/communicator.h"

#include "mpiext_pcollreq_c.h"

#define INFO_REQ_ARGS ompi_info_t *info, ompi_request_t **request

int MPIX_Allgather_init(ALLGATHER_BASE_ARGS, INFO_REQ_ARGS)
{
    return comm->c_coll->coll_allgather_init(
               ALLGATHER_BASE_ARG_NAMES, info, request,
               comm->c_coll->coll_allgather_init_module);
}

int MPIX_Allgatherv_init(ALLGATHERV_BASE_ARGS, INFO_REQ_ARGS)
{
    return comm->c_coll->coll_allgatherv_init(
               ALLGATHERV_BASE_ARG_NAMES, info, request,
               comm->c_coll->coll_allgatherv_init_module);
}

int MPIX_Allreduce_init(ALLREDUCE_BASE_ARGS, INFO_REQ_ARGS)
{
    return comm->c_coll->coll_allreduce_init(
               ALLREDUCE_BASE_ARG_NAMES, info, request,
               comm->c_coll->coll_allreduce_init_module);
}

int MPIX_Alltoall_init(ALLTOALL_BASE_ARGS, INFO_REQ_ARGS)
{
    return comm->c_coll->coll_alltoall_init(
               ALLTOALL_BASE_ARG_NAMES, info, request,
               comm->c_coll->coll_alltoall_init_module);
}

int MPIX_Alltoallv_init(ALLTOALLV_BASE_ARGS, INFO_REQ_ARGS)
{
    return comm->c_coll->coll_alltoallv_init(
               ALLTOALLV_BASE_ARG_NAMES, info, request,
               comm->c_coll->coll_alltoallv_init_module);
}

int MPIX_Alltoallw_init(ALLTOALLW_BASE_ARGS, INFO_REQ_ARGS)
{
    return comm->c_coll->coll_alltoallw_init(
               ALLTOALLW_BASE_ARG_NAMES, info, request,
               comm->c_coll->coll_alltoallw_init_module);
}

int MPIX_Barrier_init(BARRIER_BASE_ARGS, INFO_REQ_ARGS)
{
    return comm->c_coll->coll_barrier_init(
               BARRIER_BASE_ARG_NAMES, info, request,
               comm->c_coll->coll_barrier_init_module);
}

int MPIX_Bcast_init(BCAST_BASE_ARGS, INFO_REQ_ARGS)
{
    return comm->c_coll->coll_bcast_init(
               BCAST_BASE_ARG_NAMES, info, request,
               comm->c_coll->coll_bcast_init_module);
}

int MPIX_Exscan_init(EXSCAN_BASE_ARGS, INFO_REQ_ARGS)
{
    return comm->c_coll->coll_exscan_init(
               EXSCAN_BASE_ARG_NAMES, info, request,
               comm->c_coll->coll_exscan_init_module);
}

int MPIX_Gather_init(GATHER_BASE_ARGS, INFO_REQ_ARGS)
{
    return comm->c_coll->coll_gather_init(
               GATHER_BASE_ARG_NAMES, info, request,
               comm->c_coll->coll_gather_init_module);
}

int MPIX_Gatherv_init(GATHERV_BASE_ARGS, INFO_REQ_ARGS)
{
    return comm->c_coll->coll_gatherv_init(
               GATHERV_BASE_ARG_NAMES, info, request,
               comm->c_coll->coll_gatherv_init_module);
}

int MPIX_Reduce_init(REDUCE_BASE_ARGS, INFO_REQ_ARGS)
{
    return comm->c_coll->coll_reduce_init(
               REDUCE_BASE_ARG_NAMES, info, request,
               comm->c_coll->coll_reduce_init_module);
}

int MPIX_Reduce_scatter_init(REDUCESCATTER_BASE_ARGS, INFO_REQ_ARGS)
{
    return comm->c_coll->coll_reduce_scatter_init(
               REDUCESCATTER_BASE_ARG_NAMES, info, request,
               comm->c_coll->coll_reduce_scatter_init_module);
}

int MPIX_Reduce_scatter_block_init(REDUCESCATTERBLOCK_BASE_ARGS, INFO_REQ_ARGS)
{
    return comm->c_coll->coll_reduce_scatter_block_init(
               REDUCESCATTERBLOCK_BASE_ARG_NAMES, info, request,
               comm->c_coll->coll_reduce_scatter_block_init_module);
}

int MPIX_Scan_init(SCAN_BASE_ARGS, INFO_REQ_ARGS)
{
    return comm->c_coll->coll_scan_init(
               SCAN_BASE_ARG_NAMES, info, request,
               comm->c_coll->coll_scan_init_module);
}

int MPIX_Scatter_init(SCATTER_BASE_ARGS, INFO_REQ_ARGS)
{
    return comm->c_coll->coll_scatter_init(
               SCATTER_BASE_ARG_NAMES, info, request,
               comm->c_coll->coll_scatter_init_module);
}

int MPIX_Scatterv_init(SCATTERV_BASE_ARGS, INFO_REQ_ARGS)
{
    return comm->c_coll->coll_scatterv_init(
               SCATTERV_BASE_ARG_NAMES, info, request,
               comm->c_coll->coll_scatterv_init_module);
}

int MPIX_Neighbor_allgather_init(NEIGHBOR_ALLGATHER_BASE_ARGS, INFO_REQ_ARGS)
{
    return comm->c_coll->coll_neighbor_allgather_init(
               NEIGHBOR_ALLGATHER_BASE_ARG_NAMES, info, request,
               comm->c_coll->coll_neighbor_allgather_init_module);
}

int MPIX_Neighbor_allgatherv_init(NEIGHBOR_ALLGATHERV_BASE_ARGS, INFO_REQ_ARGS)
{
    return comm->c_coll->coll_neighbor_allgatherv_init(
               NEIGHBOR_ALLGATHERV_BASE_ARG_NAMES, info, request,
               comm->c_coll->coll_neighbor_allgatherv_init_module);
}

int MPIX_Neighbor_alltoall_init(NEIGHBOR_ALLTOALL_BASE_ARGS, INFO_REQ_ARGS)
{
    return comm->c_coll->coll_neighbor_alltoall_init(
               NEIGHBOR_ALLTOALL_BASE_ARG_NAMES, info, request,
               comm->c_coll->coll_neighbor_alltoall_init_module);
}

int MPIX_Neighbor_alltoallv_init(NEIGHBOR_ALLTOALLV_BASE_ARGS, INFO_REQ_ARGS)
{
    return comm->c_coll->coll_neighbor_alltoallv_init(
               NEIGHBOR_ALLTOALLV_BASE_ARG_NAMES, info, request,
               comm->c_coll->coll_neighbor_alltoallv_init_module);
}

int MPIX_Neighbor_alltoallw_init(NEIGHBOR_ALLTOALLW_BASE_ARGS, INFO_REQ_ARGS)
{
    return comm->c_coll->coll_neighbor_alltoallw_init(
               NEIGHBOR_ALLTOALLW_BASE_ARG_NAMES, info, request,
               comm->c_coll->coll_neighbor_alltoallw_init_module);
}
