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
    mca_llm_base_hostfile_node_t *node;
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
        int iters = 0;

        /* loop until we are done */
        for (iters = 1 ; alloc_procs < procs ; ++iters) {
            for (nodeitem = ompi_list_get_first(hostlist);
                 nodeitem != ompi_list_get_end(hostlist);
                 nodeitem = ompi_list_get_next(nodeitem)) {
                node = (mca_llm_base_hostfile_node_t*) nodeitem;

                if (alloc_procs >= procs) { 
                    /* we've allocated enough.  If we are on first
                    loop, remove from list.  Otherwise, break out of
                    loop */
                    if (1 == iters) {
                        tmp = ompi_list_remove_item(hostlist, nodeitem);
                        OBJ_RELEASE(nodeitem);
                        nodeitem = tmp;
                    } else {
                        break;
                    }
                } else if (alloc_procs + node->given_count <= procs) {
                    /* the entire host allocation is needed... */
                    node->count += node->given_count;
                    alloc_procs += node->given_count;
                } else {
                    /* the entire host allocation isn't needed.  dump the
                       unneeded parts */
                    node->count += procs - alloc_procs;
                    alloc_procs = procs;
                }
            }
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
