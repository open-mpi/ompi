/* -*- C -*-
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "mca/llm/llm.h"
#include "mca/llm/base/base.h"
#include "mca/llm/base/base_internal.h"

#include <stdio.h>

int
mca_llm_base_map_resources(int nodes,
                           int procs,
                           ompi_list_t *hostlist)
{
    ompi_rte_node_allocation_t *node;
    ompi_list_item_t *nodeitem, *tmp;

    if (NULL == hostlist) {
        return OMPI_ERROR;
    }

    /* do the four cases of allocation */
    if (0 == nodes && 0 == procs) {
        /* allocate as much as we can, so we don't change the list at
           all */

    } else if (0 == nodes && 0 != procs) { 
        /* allocate procs process count as dense as possible */
        int alloc_procs = 0;

        for (nodeitem = ompi_list_get_first(hostlist);
             nodeitem != ompi_list_get_end(hostlist);
             nodeitem = ompi_list_get_next(nodeitem)) {
            node = (ompi_rte_node_allocation_t*) nodeitem;

            if (alloc_procs >= procs) { 
                /* we've allocated enough - release this guy from the
                   list */
                tmp = ompi_list_remove_item(hostlist, nodeitem);
                OBJ_RELEASE(nodeitem);
                nodeitem = tmp;
            } else if (alloc_procs + node->count < procs) {
                /* the entire host allocation is needed... */
                alloc_procs += node->count;
            } else {
                /* the entire host allocation isn't needed.  dump the
                   unneeded parts */
                node->count = procs - alloc_procs;
                alloc_procs = procs;
            }
        }

    } else if (0 != nodes && 0 == procs) {
        /* allocate as many nodes as possible with each node having 
           one slot */

        for (nodeitem = ompi_list_get_first(hostlist);
             nodeitem != ompi_list_get_end(hostlist);
             nodeitem = ompi_list_get_next(nodeitem)) {
            node = (ompi_rte_node_allocation_t*) nodeitem;
            node->count = 1;
        }

    } else if (0 != nodes && 0 != procs) {
        /* allocate as best we can */
        /* BWB - implement me */
        return OMPI_ERR_FATAL;
 
    } else {
        return OMPI_ERR_FATAL;
    }

    return OMPI_SUCCESS;
}
