/*
 * Copyright (c) 2014-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2022      IBM Corporation. All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "coll_adapt.h"
#include "coll_adapt_topocache.h"

#include "ompi/communicator/communicator.h"

static void destruct_topology_cache(ompi_coll_adapt_topology_cache_item_t *item)
{
    if (NULL != item->tree) {
        ompi_coll_base_topo_destroy_tree(&item->tree);
    }
}

OBJ_CLASS_INSTANCE(ompi_coll_adapt_topology_cache_item_t, opal_list_item_t,
                   NULL, &destruct_topology_cache);

static ompi_coll_tree_t *create_topology(
    ompi_coll_adapt_algorithm_t algorithm,
    int root,
    struct ompi_communicator_t *comm)
{
    switch(algorithm) {
        case OMPI_COLL_ADAPT_ALGORITHM_TUNED:
        {
            return NULL;
        }
        case OMPI_COLL_ADAPT_ALGORITHM_BINOMIAL:
        {
            return ompi_coll_base_topo_build_bmtree(comm, root);
        }
        case OMPI_COLL_ADAPT_ALGORITHM_IN_ORDER_BINOMIAL:
        {
            return ompi_coll_base_topo_build_in_order_bmtree(comm, root);
        }
        case OMPI_COLL_ADAPT_ALGORITHM_BINARY:
        {
            return ompi_coll_base_topo_build_tree(2, comm, root);
        }
        case OMPI_COLL_ADAPT_ALGORITHM_PIPELINE:
        {
            return ompi_coll_base_topo_build_chain(1, comm, root);
        }
        case OMPI_COLL_ADAPT_ALGORITHM_CHAIN:
        {
            return ompi_coll_base_topo_build_chain(4, comm, root);
        }
        case OMPI_COLL_ADAPT_ALGORITHM_LINEAR:
        {
            int fanout = ompi_comm_size(comm) - 1;
            ompi_coll_tree_t *tree;
            if (fanout <= 1) {
                tree = ompi_coll_base_topo_build_chain(1, comm, root);
            } else if (fanout <= MAXTREEFANOUT) {
                tree = ompi_coll_base_topo_build_tree(ompi_comm_size(comm) - 1, comm, root);
            } else {
                tree = ompi_coll_base_topo_build_tree(MAXTREEFANOUT, comm, root);
            }
            return tree;
        }
        default:
            printf("WARN: unknown topology %d\n", algorithm);
            return NULL;
    }
}

ompi_coll_tree_t* ompi_coll_adapt_module_cached_topology(
    mca_coll_base_module_t *module,
    struct ompi_communicator_t *comm,
    int root,
    ompi_coll_adapt_algorithm_t algorithm)
{
    mca_coll_adapt_module_t *adapt_module = (mca_coll_adapt_module_t*)module;
    ompi_coll_adapt_topology_cache_item_t *item;
    ompi_coll_tree_t * tree;
    if (NULL != adapt_module->topo_cache) {
        OPAL_LIST_FOREACH(item, adapt_module->topo_cache, ompi_coll_adapt_topology_cache_item_t) {
            if (item->root == root && item->algorithm == algorithm) {
                return item->tree;
            }
        }
    } else {
        adapt_module->topo_cache = OBJ_NEW(opal_list_t);
    }

    /* topology not found, create one */
    tree = create_topology(algorithm, root, comm);

    item = OBJ_NEW(ompi_coll_adapt_topology_cache_item_t);
    item->tree = tree;
    item->root = root;
    item->algorithm = algorithm;
    opal_list_prepend(adapt_module->topo_cache, &item->super);
    return tree;
}

