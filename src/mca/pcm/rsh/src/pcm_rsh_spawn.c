/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include "mca/pcm/pcm.h"
#include "mca/pcm/rsh/src/pcm_rsh.h"
#include "runtime/runtime_types.h"


static int internal_spawn_proc(int jobid, ompi_rte_node_schedule_t *sched,
                               ompi_list_t *nodelist);


bool
mca_pcm_rsh_can_spawn(void)
{
    /* we can always try to rsh some more...  Might not always work as
     * the caller hopes
     */
    return true;
}


int
mca_pcm_rsh_spawn_procs(int jobid, ompi_list_t *schedlist)
{
    ompi_list_item_t *sched_item, *node_item;
    ompi_rte_node_schedule_t *sched;
    ompi_rte_node_allocation_t *node;
    ompi_list_t launch;
    ompi_list_t done;
    int ret, i;
    int width = 1;

    OBJ_CONSTRUCT(&launch, ompi_list_t);
    OBJ_CONSTRUCT(&done, ompi_list_t);

    for (sched_item = ompi_list_get_first(schedlist) ;
         sched_item != ompi_list_get_end(schedlist) ;
         sched_item = ompi_list_get_next(sched_item)) {
        sched = (ompi_rte_node_schedule_t*) sched_item;

        /*
         * make sure I'm the first node in the list and then start our
         * deal.  We rsh me just like everyone else so that we don't
         * have any unexpected environment oddities...
         */
        /* BWB - do front of list check! */
        node_item = ompi_list_get_first(sched->nodelist);

        while (node_item != ompi_list_get_end(sched->nodelist)) {
            /* find enough entries for this slice to go */
            for (i = 0 ;
                 i < width && node_item != ompi_list_get_end(sched->nodelist) ;
                 node_item = ompi_list_get_next(node_item)) { }
            /* if we don't have anyone, get us out of here.. */
            if (i ==  0) {
                continue;
            }

            /* make a launch list */
            ompi_list_splice(&launch, ompi_list_get_end(&launch),
                             sched->nodelist,
                             ompi_list_get_first(sched->nodelist),
                             node_item);

            /* do the launch to the first node in the list, passing
               him the rest of the list */
            ret = internal_spawn_proc(jobid, sched, &launch);
            if  (OMPI_SUCCESS != ret) {
                /* well, crap!  put ourselves back together, I guess.
                   Should call killjob */
                ompi_list_join(&done, ompi_list_get_end(&done), &launch);
                ompi_list_join(sched->nodelist, 
                               ompi_list_get_first(sched->nodelist),
                               &done);
                return ret;
            }

            /* copy the list over to the done part */
            ompi_list_join(&done, ompi_list_get_end(&done), &launch);
        }
    }

    OBJ_DESTRUCT(&done);
    OBJ_DESTRUCT(&launch);

    return OMPI_SUCCESS;
}


static int
internal_spawn_proc(int jobid, ompi_rte_node_schedule_t *sched,
                    ompi_list_t *nodelist)
{
    /* ok, we rsh to the first guy in the list, then pass the whole
       nodelist */
    


    return OMPI_SUCCESS;
}
