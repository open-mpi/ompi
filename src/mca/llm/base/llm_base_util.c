/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "mca/llm/llm.h"
#include "mca/llm/base/base.h"
#include "mca/llm/base/base_internal.h"


void
mca_llm_base_deallocate(ompi_list_t *hostlist)
{
    ompi_list_item_t *item;

    if (NULL == hostlist) return;

    while (NULL != (item = ompi_list_remove_first(hostlist))) {
        OBJ_RELEASE(item);
    }

    OBJ_RELEASE(hostlist);
}


static
ompi_rte_node_allocation_t*
get_allocation_for_size(int count, ompi_list_t *nodelist)
{
    ompi_list_item_t *nodeitem;
    ompi_rte_node_allocation_t *node;
    mca_llm_base_hostfile_data_t *data;

    for (nodeitem = ompi_list_get_first(nodelist) ;
         nodeitem != ompi_list_get_end(nodelist) ;
         nodeitem = ompi_list_get_next(nodeitem) ) {
        node = (ompi_rte_node_allocation_t*) nodeitem;

        if (node->count == count) {
            return node;
        }
    }

    /* no joy... make one and put it in the list */
    node = OBJ_NEW(ompi_rte_node_allocation_t);
    node->count = count;
    ompi_list_append(nodelist, (ompi_list_item_t*) node);

    data = OBJ_NEW(mca_llm_base_hostfile_data_t);
    node->data = (ompi_rte_node_allocation_data_t*) data;

    return node;
}


ompi_list_t*
mca_llm_base_create_node_allocation(ompi_list_t *hostlist)
{
    ompi_list_t *nodelist;
    mca_llm_base_hostfile_node_t *host;
    mca_llm_base_hostfile_data_t *data;
    ompi_rte_node_allocation_t *node;
    ompi_list_item_t *hostitem, *nodeitem;
    int start_count = 0;

    nodelist = OBJ_NEW(ompi_list_t);

    /* This is going to be slow as molasses in January in
     * Alaska. Iterate through the list of hosts and group them in
     * ompi_rte_node_allocation_t structures.  Then take those and
     * iterate through, setting the start numbers.  So nothing too
     * horrible, right?
     */

    /* on with the partitioning */
    while (NULL != (hostitem = ompi_list_remove_first(hostlist))) {
        host = (mca_llm_base_hostfile_node_t*) hostitem;
        node = get_allocation_for_size(host->count, nodelist);
        data = (mca_llm_base_hostfile_data_t*) node->data;
        node->nodes++;

        ompi_list_append(data->hostlist, (ompi_list_item_t*) host);
    }

    /* and fix the start numbers */
    start_count = 0;
    for (nodeitem = ompi_list_get_first(nodelist) ;
         nodeitem != ompi_list_get_end(nodelist) ;
         nodeitem = ompi_list_get_next(nodeitem) ) {
        node = (ompi_rte_node_allocation_t*) nodeitem;
        node->start = start_count;
        start_count += (node->nodes * node->count);
    }

    return nodelist;
}
