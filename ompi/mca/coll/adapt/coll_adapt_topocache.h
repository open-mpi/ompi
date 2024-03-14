/*
 * Copyright (c) 2014-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2022      IBM Corporation. All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_COLL_ADAPT_TOPOCACHE_H
#define MCA_COLL_ADAPT_TOPOCACHE_H

#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/adapt/coll_adapt.h"
#include "opal/class/opal_list.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_base_topo.h"

typedef struct adapt_topology_cache_item_t {
    opal_list_item_t super;
    ompi_coll_tree_t *tree;
    int root;
    ompi_coll_adapt_algorithm_t algorithm;
} ompi_coll_adapt_topology_cache_item_t;

OBJ_CLASS_DECLARATION(ompi_coll_adapt_topology_cache_item_t);


OMPI_DECLSPEC ompi_coll_tree_t* ompi_coll_adapt_module_cached_topology(
    mca_coll_base_module_t *module,
    struct ompi_communicator_t *comm,
    int root,
    ompi_coll_adapt_algorithm_t algorithm);

#endif /* MCA_COLL_ADAPT_TOPOCACHE_H */
