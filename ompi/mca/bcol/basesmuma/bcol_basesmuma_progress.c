/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 */

#include "ompi_config.h"
#include "ompi/constants.h"

#include "bcol_basesmuma.h"

/* the progress function to be called from the opal progress function
 */
int bcol_basesmuma_progress(void)
{
    /* local variables */
    volatile int32_t *cntr;
    mca_bcol_basesmuma_component_t *cs = &mca_bcol_basesmuma_component;

    /* check to see if release of memory blocks needs to be done */
    if( opal_list_get_size(&(cs->nb_admin_barriers)) ) {
        sm_nbbar_desc_t *item_ptr;
        opal_list_t *list=&(cs->nb_admin_barriers);
        /* process only if the list is non-empty */
        if( !OPAL_THREAD_TRYLOCK(&cs->nb_admin_barriers_mutex)) {

            for (item_ptr = (sm_nbbar_desc_t*) opal_list_get_first(list);
                    item_ptr != (sm_nbbar_desc_t*) opal_list_get_end(list);
                    item_ptr = (sm_nbbar_desc_t*) opal_list_get_next(item_ptr) )
            {
                bcol_basesmuma_rd_nb_barrier_progress_admin(item_ptr);
                /* check to see if an complete */
                if( NB_BARRIER_DONE == item_ptr->collective_phase ) {
                    /* barrier is complete - remove from the list.  No need
                     * to put it on another list, as it is part of the memory
                     * bank control structure, and will be picked up
                     * again when needed.
                     */
                    int index=
                        item_ptr->pool_index;
                    /* old way - ctl_struct specific */
                    /* 
                       volatile uint64_t *cntr= (volatile uint64_t *)
                       &(item_ptr->sm_module->colls_no_user_data.
                       ctl_buffs_mgmt[index].bank_gen_counter);
                     */

                    cntr= (volatile int32_t *) &(item_ptr->coll_buff->
                                ctl_buffs_mgmt[index].bank_gen_counter);
                    item_ptr=(sm_nbbar_desc_t*)opal_list_remove_item((opal_list_t *)list,
                            ( opal_list_item_t *)item_ptr);
                    /* increment the generation number */
                    OPAL_THREAD_ADD32(cntr,1);
                }
            }

            OPAL_THREAD_UNLOCK(&cs->nb_admin_barriers_mutex);
        }

    }
    return OMPI_SUCCESS;

}
