/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2013-2014 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mca/coll/ml/coll_ml.h"
#include "ompi/include/ompi/constants.h"
#include "ompi/mca/coll/ml/coll_ml_functions.h"
#include "ompi/mca/coll/ml/coll_ml_hier_algorithms_common_setup.h"
#include "ompi/patterns/net/netpatterns_knomial_tree.h"

#define SMALL_MSG_RANGE 1
#define LARGE_MSG_RANGE 5

static int mca_coll_ml_build_allgather_schedule(mca_coll_ml_topology_t *topo_info,
                                                mca_coll_ml_collective_operation_description_t **coll_desc, int bcol_func_index)
{
    int ret; /* exit code in case of error */
    int nfn = 0;
    int i;
    int *scratch_indx = NULL,
        *scratch_num = NULL;

    mca_coll_ml_collective_operation_description_t  *schedule = NULL;
    mca_coll_ml_compound_functions_t *comp_fn;
    mca_coll_ml_schedule_hier_info_t h_info;

    ML_VERBOSE(9, ("Setting hierarchy, inputs : n_levels %d, hiest %d ",
                   topo_info->n_levels, topo_info->global_highest_hier_group_index));
    MCA_COLL_ML_INIT_HIER_INFO(h_info, topo_info->n_levels,
                               topo_info->global_highest_hier_group_index, topo_info);

    ret = mca_coll_ml_schedule_init_scratch(topo_info, &h_info,
                                            &scratch_indx, &scratch_num);
    if (OMPI_SUCCESS != ret) {
        ML_ERROR(("Can't mca_coll_ml_schedule_init_scratch."));
        goto Error;
    }
    assert(NULL != scratch_indx);
    assert(NULL != scratch_num);

    schedule = *coll_desc =
        mca_coll_ml_schedule_alloc(&h_info);
    if (NULL == schedule) {
        ML_ERROR(("Can't allocate memory."));
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto Error;
    }
    /* Setting topology information */
    schedule->topo_info = topo_info;

    /* Set dependencies equal to number of hierarchies */
    for (i = 0; i < h_info.num_up_levels; i++) {
        int query_conf[MCA_COLL_ML_QUERY_SIZE];
        MCA_COLL_ML_SET_QUERY(query_conf, DATA_SRC_KNOWN, BLOCKING, BCOL_GATHER, bcol_func_index, 0, 0);
        comp_fn = &schedule->component_functions[i];
        MCA_COLL_ML_SET_COMP_FN(comp_fn, i, topo_info,
                                i, scratch_indx, scratch_num, query_conf, "GATHER_DATA");
    }

    nfn = i;
    if (h_info.call_for_top_function) {
        int query_conf[MCA_COLL_ML_QUERY_SIZE];
        MCA_COLL_ML_SET_QUERY(query_conf, DATA_SRC_KNOWN, NON_BLOCKING, BCOL_ALLGATHER, bcol_func_index, 0, 0);
        comp_fn = &schedule->component_functions[nfn];
        MCA_COLL_ML_SET_COMP_FN(comp_fn, nfn, topo_info,
                                nfn, scratch_indx, scratch_num, query_conf, "ALLGATHER_DATA");
        ++nfn;
    }

    /* coming down the hierarchy */
    for (i = h_info.num_up_levels - 1; i >= 0; i--, nfn++) {
        int query_conf[MCA_COLL_ML_QUERY_SIZE];
        MCA_COLL_ML_SET_QUERY(query_conf, DATA_SRC_KNOWN, NON_BLOCKING, BCOL_BCAST, bcol_func_index, 0, 0);
        comp_fn = &schedule->component_functions[nfn];
        MCA_COLL_ML_SET_COMP_FN(comp_fn, i, topo_info,
                                nfn, scratch_indx, scratch_num, query_conf, "BCAST_DATA");
    }

    /* Fill the rest of constant data */
    mca_coll_ml_call_types(&h_info, schedule);

    MCA_COLL_ML_SET_SCHEDULE_ORDER_INFO(schedule);

    free(scratch_num);
    free(scratch_indx);

    return OMPI_SUCCESS;

 Error:
    if (NULL != scratch_indx) {
        free(scratch_indx);
    }
    if (NULL != scratch_num) {
        free(scratch_num);
    }

    return ret;
}

int ml_coll_hier_allgather_setup(mca_coll_ml_module_t *ml_module)
{
    /* Hierarchy Setup */
    int ret, topo_index, alg;
    mca_coll_ml_topology_t *topo_info = ml_module->topo_list;

    ML_VERBOSE(10,("entering allgather setup"));

#if 0
    /* used to validate the recursive k - ing allgather tree */
    {
        /* debug print */
        int ii, jj;
        netpatterns_k_exchange_node_t exchange_node;

        ret = netpatterns_setup_recursive_knomial_allgather_tree_node(8, 3, 3, &exchange_node);
        fprintf(stderr,"log tree order %d tree_order %d\n", exchange_node.log_tree_order,exchange_node.tree_order);
        if( EXCHANGE_NODE == exchange_node.node_type){
            if( exchange_node.n_extra_sources > 0){
                fprintf(stderr,"Receiving data from extra rank %d\n",exchange_node.rank_extra_sources_array[0]);
            }
            for( ii = 0; ii < exchange_node.log_tree_order; ii++){
                for( jj = 0; jj < (exchange_node.tree_order-1); jj++) {
                    if( exchange_node.rank_exchanges[ii][jj] >= 0){
                        fprintf(stderr,"level %d I send %d bytes to %d from offset %d \n",ii+1,
                                exchange_node.payload_info[ii][jj].s_len,
                                exchange_node.rank_exchanges[ii][jj],
                                exchange_node.payload_info[ii][jj].s_offset);
                        fprintf(stderr,"level %d I receive %d bytes from %d at offset %d\n",ii+1,
                                exchange_node.payload_info[ii][jj].r_len,
                                exchange_node.rank_exchanges[ii][jj],
                                exchange_node.payload_info[ii][jj].r_offset);
                    }
                }
            }
            fprintf(stderr,"exchange_node.n_extra_sources %d\n",exchange_node.n_extra_sources);
            fprintf(stderr,"exchange_node.myid_reindex %d\n",exchange_node.reindex_myid);
            if( exchange_node.n_extra_sources > 0){
                fprintf(stderr,"Sending back data to extra rank %d\n",exchange_node.rank_extra_sources_array[0]);
            }
        } else {
            fprintf(stderr,"I am an extra and send to proxy %d\n",
                    exchange_node.rank_extra_sources_array[0]);
        }
    }
#endif

    alg = mca_coll_ml_component.coll_config[ML_ALLGATHER][ML_SMALL_MSG].algorithm_id;
    topo_index = ml_module->collectives_topology_map[ML_ALLGATHER][alg];
    if (ML_UNDEFINED == alg || ML_UNDEFINED == topo_index) {
        ML_ERROR(("No topology index or algorithm was defined"));
        topo_info->hierarchical_algorithms[ML_ALLGATHER] = NULL;
        return OMPI_ERROR;
    }

    ret = mca_coll_ml_build_allgather_schedule(&ml_module->topo_list[topo_index],
                                               &ml_module->coll_ml_allgather_functions[alg],
                                               SMALL_MSG_RANGE);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        ML_VERBOSE(10, ("Failed to setup static alltoall"));
        return ret;
    }

    alg = mca_coll_ml_component.coll_config[ML_ALLGATHER][ML_LARGE_MSG].algorithm_id;
    topo_index = ml_module->collectives_topology_map[ML_ALLGATHER][alg];
    if (ML_UNDEFINED == alg || ML_UNDEFINED == topo_index) {
        ML_ERROR(("No topology index or algorithm was defined"));
        topo_info->hierarchical_algorithms[ML_ALLGATHER] = NULL;
        return OMPI_ERROR;
    }

    ret = mca_coll_ml_build_allgather_schedule(&ml_module->topo_list[topo_index],
                                               &ml_module->coll_ml_allgather_functions[alg],
                                               LARGE_MSG_RANGE);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        ML_VERBOSE(10, ("Failed to setup static alltoall"));
        return ret;
    }

    return OMPI_SUCCESS;
}

void ml_coll_hier_allgather_cleanup(mca_coll_ml_module_t *ml_module)
{
    /* Hierarchy Setup */
    int topo_index, alg;
    mca_coll_ml_topology_t *topo_info = ml_module->topo_list;

    alg = mca_coll_ml_component.coll_config[ML_ALLGATHER][ML_SMALL_MSG].algorithm_id;
    topo_index = ml_module->collectives_topology_map[ML_ALLGATHER][alg];
    if (ML_UNDEFINED == alg || ML_UNDEFINED == topo_index) {
        ML_ERROR(("No topology index or algorithm was defined"));
        topo_info->hierarchical_algorithms[ML_ALLGATHER] = NULL;
        return;
    }

    if (NULL == ml_module->coll_ml_allgather_functions[alg]) {
        return;
    }

    if (ml_module->coll_ml_allgather_functions[alg]->component_functions) {
        free(ml_module->coll_ml_allgather_functions[alg]->component_functions);
        ml_module->coll_ml_allgather_functions[alg]->component_functions = NULL;
    }

    if (ml_module->coll_ml_allgather_functions[alg]) {
        free(ml_module->coll_ml_allgather_functions[alg]);
        ml_module->coll_ml_allgather_functions[alg] = NULL;
    }

    alg = mca_coll_ml_component.coll_config[ML_ALLGATHER][ML_LARGE_MSG].algorithm_id;
    topo_index = ml_module->collectives_topology_map[ML_ALLGATHER][alg];
    if (ML_UNDEFINED == alg || ML_UNDEFINED == topo_index) {
        ML_ERROR(("No topology index or algorithm was defined"));
        topo_info->hierarchical_algorithms[ML_ALLGATHER] = NULL;
        return;
    }

    if (ml_module->coll_ml_allgather_functions[alg]->component_functions) {
        free(ml_module->coll_ml_allgather_functions[alg]->component_functions);
        ml_module->coll_ml_allgather_functions[alg]->component_functions = NULL;
    }

    if (ml_module->coll_ml_allgather_functions[alg]) {
        free(ml_module->coll_ml_allgather_functions[alg]);
        ml_module->coll_ml_allgather_functions[alg] = NULL;
    }
}
