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
#include "llm_bjs.h"

#include "mca/llm/llm.h"
#include "mca/llm/base/base.h"
#include "mca/llm/base/base_internal.h"
#include "mca/ns/ns.h"
#include "util/output.h"

#include <stdio.h>

ompi_list_t*
mca_llm_bjs_allocate_resources(mca_llm_base_module_t *me,
                                    mca_ns_base_jobid_t jobid, 
                                    int nodes, int procs)
{
    ompi_list_t *hostlist = NULL;
    ompi_list_t *nodelist = NULL;
    int ret, i, j, nodes_len;
    char *node_string, *tmp;
    mca_llm_base_hostfile_node_t *newnode;

    hostlist = OBJ_NEW(ompi_list_t);

    tmp = getenv("NODES");
    if (NULL == tmp) return NULL;

    node_string = strdup(tmp);
    if (NULL == node_string) return NULL;

    for (i = 0 ; i < nodes_len ; ++i) {
        /* find the end of this entry and change the comma to end of string */
        for (j = 0 ; j + i < nodes_len ; ++j) {
            if (node_string[i + j] == ',') {
                node_string[i + j] = '\0';
                break;
            }
        }

        newnode = OBJ_NEW(mca_llm_base_hostfile_node_t);
        strcpy(newnode->hostname, node_string + i);
        newnode->given_count = 1;
        ompi_list_append(hostlist, (ompi_list_item_t*) newnode);

        i += j;
    }

    ret = mca_llm_base_map_resources(nodes, procs, hostlist);
    if (OMPI_SUCCESS != ret) {
        mca_llm_base_deallocate(hostlist);
        return NULL;
    }

    nodelist = mca_llm_base_create_node_allocation(hostlist);
    if (OMPI_SUCCESS != ret) {
        mca_llm_base_deallocate(hostlist);
        return NULL;
    }

    return nodelist;
}


int
mca_llm_bjs_deallocate_resources(mca_llm_base_module_t *me,
                                      mca_ns_base_jobid_t jobid,
                                      ompi_list_t *nodelist)
{
    ompi_list_item_t *item;

    /* pop off all the ompi_ret_node_allocatoin_t instances and free
     * them.  Their destructors will kill the
     * mca_llm_base_hostfile_data_t, who's destructor will kill the
     * mca_llm_base_hostfile_node_t instances associated with the
     * node_allocation.  In other words, everything goes "bye-bye"
     */
    while (NULL != (item = ompi_list_remove_first(nodelist))) {
        OBJ_RELEASE(item);
    }

    OBJ_RELEASE(nodelist);

    return OMPI_SUCCESS;
}
