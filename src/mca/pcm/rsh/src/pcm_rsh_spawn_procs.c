/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include "mca/pcm/pcm.h"
#include "mca/pcm/rsh/src/pcm_rsh.h"
#include "runtime/runtime_types.h"

int
mca_pcm_rsh_spawn_procs(int jobid, ompi_list_t *schedlist)
{
    ompi_list_item_t *sched_item, *node_item;
    ompi_rte_node_schedule_t *sched;
    ompi_rte_node_allocation_t *node;
    ompi_list_t launch;

    OBJ_CONSTRUCT(&launch, ompi_list_t);

    for (sched_item = ompi_list_get_first(schedlist) ;
         sched_item != ompi_list_get_end(schedlist) ;
         sched_item = ompi_list_get_next(sched_item)) {
        sched = (ompi_rte_node_schedule_t*) sched_item;

        /* when we start doing tree based, more logic here... */
        for (node_item = ompi_list_get_first(sched->nodelist) ;
             node_item != ompi_list_get_end(sched->nodelist) ;
             node_item = ompi_list_get_next(node_item)) {
            node = (ompi_rte_node_allocation_t*) node_item;

            /* we don't need to push nodes down to the compute places,
               so don't do it... */
            
        }            
    }

    OBJ_DESTRUCT(&launch);

    return OMPI_SUCCESS;
}
