/*
 * Copyright (c) 2008      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2011-2013 Inria.  All rights reserved.
 * Copyright (c) 2011-2013 Université Bordeaux 1
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 */

#include "ompi_config.h"

#include "ompi/communicator/communicator.h"
#include "ompi/info/info.h"
#include "ompi/mca/topo/base/base.h"


int mca_topo_base_dist_graph_create_adjacent(mca_topo_base_module_t* module,
                                             ompi_communicator_t *comm_old,
                                             int indegree, int sources[],
                                             int sourceweights[], 
                                             int outdegree,
                                             int destinations[], 
                                             int destweights[],
                                             ompi_info_t *info, int reorder,
                                             ompi_communicator_t **newcomm)
{
    mca_topo_base_comm_dist_graph_2_2_0_t *topo = NULL;
    int err;

    if( OMPI_SUCCESS != (err = ompi_comm_create(comm_old,
                                                comm_old->c_local_group,
                                                newcomm)) ) {
        return err;
    }
    err = OMPI_ERR_OUT_OF_RESOURCE;  /* suppose by default something bad will happens */

    assert( NULL == (*newcomm)->c_topo );

    topo = OBJ_NEW(mca_topo_base_comm_dist_graph_2_2_0_t);
    if( NULL == topo ) {
        goto bail_out;
    }
    topo->in = topo->inw = NULL;
    topo->out = topo->outw = NULL;
    topo->indegree = indegree;
    topo->outdegree = outdegree;
    topo->weighted = !((MPI_UNWEIGHTED == sourceweights) && (MPI_UNWEIGHTED == destweights));

    if (topo->indegree > 0) {
        topo->in = (int*)malloc(sizeof(int) * topo->indegree);
        if (NULL == topo->in) {
            goto bail_out;
        }
        memcpy(topo->in, sources, sizeof(int) * topo->indegree);
        if (MPI_UNWEIGHTED != sourceweights) {
            topo->inw = (int*)malloc(sizeof(int) * topo->indegree);
            if( NULL == topo->inw ) {
                goto bail_out;
            }
            memcpy( topo->inw, sourceweights, sizeof(int) * topo->indegree );
        }
    }

    if (topo->outdegree > 0) {
        topo->out = (int*)malloc(sizeof(int) * topo->outdegree);
        if (NULL == topo->out) {
            goto bail_out;
        }
        memcpy(topo->out, destinations, sizeof(int) * topo->outdegree);
        topo->outw = NULL;
        if (MPI_UNWEIGHTED != destweights) {
            if (topo->outdegree > 0) {
                topo->outw = (int*)malloc(sizeof(int) * topo->outdegree);
                if (NULL == topo->outw) {
                    goto bail_out;
                }
                memcpy(topo->outw, destweights, sizeof(int) * topo->outdegree);
            }
        }
    }

    (*newcomm)->c_topo                 = module;
    (*newcomm)->c_topo->mtc.dist_graph = topo;
    (*newcomm)->c_topo->reorder        = reorder;
    (*newcomm)->c_flags               |= OMPI_COMM_DIST_GRAPH;

    return OMPI_SUCCESS;

 bail_out:
    if (NULL != topo) {
        if( NULL != topo->in ) free(topo->in);
        if( MPI_UNWEIGHTED != sourceweights ) {
            if( NULL != topo->inw ) free(topo->inw);
        }
        if( NULL != topo->out ) free(topo->out);
        if( MPI_UNWEIGHTED != destweights ) {
            if( NULL != topo->outw ) free(topo->outw);
        }
        free(topo);
    }
    ompi_comm_free(newcomm);
    return err;
}
