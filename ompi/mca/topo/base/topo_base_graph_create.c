/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012-2013 Inria.  All rights reserved.
 * Copyright (c) 2014      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/mca/topo/base/base.h"
#include "ompi/mca/topo/topo.h"

static int mca_topo_base_graph_allocate (ompi_group_t *group, int nnodes, const int *index, const int *edges,
                                         int *num_procs, mca_topo_base_comm_graph_2_2_0_t **graph_out)
{
    mca_topo_base_comm_graph_2_2_0_t *graph;

    *num_procs = group->grp_proc_count;

    if (*num_procs < nnodes) {
        return MPI_ERR_DIMS;
    }

    if (*num_procs > nnodes) {
        *num_procs = nnodes;
    }

    if (group->grp_my_rank > (nnodes - 1) || MPI_UNDEFINED == group->grp_my_rank) {
        *graph_out = NULL;
        return OMPI_SUCCESS;
    }

    graph = OBJ_NEW(mca_topo_base_comm_graph_2_2_0_t);
    if( NULL == graph ) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    graph->nnodes = nnodes;
    graph->index = (int *) malloc (sizeof (int) * nnodes);
    graph->edges = (int *) malloc (sizeof (int) * index[nnodes-1]);
    if (OPAL_UNLIKELY(NULL == graph->index || NULL == graph->edges)) {
        OBJ_RELEASE(graph);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    memcpy(graph->index, index, nnodes * sizeof(int));
    memcpy(graph->edges, edges, index[nnodes-1] * sizeof(int));

    *graph_out = graph;

    return OMPI_SUCCESS;
}

/*
 *
 * function - makes a new communicator to which topology information
 *            has been attached
 *
 * @param comm_old input communicator without topology (handle)
 * @param nnodes number of nodes in graph (integer)
 * @param index array of integers describing node degrees (see below)
 * @param edges array of integers describing graph edges (see below)
 * @param reorder ranking may be reordered (true) or not (false) (logical)
 * @param comm_graph communicator with graph topology added (handle)
 *
 * @retval MPI_SUCCESS
 * @retval MPI_ERR_OUT_OF_RESOURCE
 */

int mca_topo_base_graph_create (mca_topo_base_module_t *topo, ompi_communicator_t *old_comm,
                                int nnodes, const int *index, const int *edges, bool reorder,
                                ompi_communicator_t **comm_topo)
{
    mca_topo_base_comm_graph_2_2_0_t *graph;
    ompi_group_t *c_local_group;
    int num_procs, ret;

    assert(topo->type == OMPI_COMM_GRAPH);

    *comm_topo = MPI_COMM_NULL;

    ret = mca_topo_base_graph_allocate (old_comm->c_local_group, nnodes, index, edges, &num_procs,
                                        &graph);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    c_local_group = ompi_group_flatten (old_comm->c_local_group, nnodes);
    if (OPAL_UNLIKELY(NULL == c_local_group)) {
        OBJ_RELEASE(graph);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    ret = ompi_comm_create (old_comm, c_local_group, comm_topo);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        OBJ_RELEASE(graph);
        return ret;
    }

    if (MPI_COMM_NULL != *comm_topo) {
        (*comm_topo)->c_topo            = topo;
        (*comm_topo)->c_topo->mtc.graph = graph;
        (*comm_topo)->c_flags          |= OMPI_COMM_GRAPH;
        (*comm_topo)->c_topo->reorder   = reorder;
    }

    return OMPI_SUCCESS;
}

static void mca_topo_base_comm_graph_2_2_0_construct(mca_topo_base_comm_graph_2_2_0_t * graph) {
    graph->nnodes = 0;
    graph->index = NULL;
    graph->edges = NULL;
}

static void mca_topo_base_comm_graph_2_2_0_destruct(mca_topo_base_comm_graph_2_2_0_t * graph) {
    if (NULL != graph->index) {
        free(graph->index);
    }
    if (NULL != graph->edges) {
        free(graph->edges);
    }
}

OBJ_CLASS_INSTANCE(mca_topo_base_comm_graph_2_2_0_t, opal_object_t,
                   mca_topo_base_comm_graph_2_2_0_construct,
                   mca_topo_base_comm_graph_2_2_0_destruct);
