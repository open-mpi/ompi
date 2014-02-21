/*
 * Copyright (c) 2009-2012 UT-Battelle, LLC. All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
/* we need make cleanup with all these includes  START */
#include <unistd.h>
#include <sys/types.h>

#include "ompi/constants.h"
#include "ompi/mca/bcol/bcol.h"
#include "bcol_basesmuma.h"
#include "opal/sys/atomic.h"
#include "ompi/patterns/net/netpatterns.h"
#include "ompi/mca/bcol/base/base.h"

/*
 * Initialize nonblocking barrier.  This is code specific for handling
 * the recycling of data, and uses only a single set of control buffers.
 * It also assumes that for a given process, only a single outstanding 
 * barrier operation will occur for a given control structure, 
 * with the sequence number being used for potential overlap in time
 * between succesive barrier calls on different processes.
 */
int bcol_basesmuma_rd_nb_barrier_init_admin( 
        sm_nbbar_desc_t *sm_desc)

{
    /* local variables */
    int ret=OMPI_SUCCESS, idx, leading_dim, loop_cnt, exchange;
    int pair_rank;
    mca_bcol_basesmuma_ctl_struct_t **ctl_structs;
    netpatterns_pair_exchange_node_t *my_exchange_node;
    int extra_rank, my_rank;
    mca_bcol_basesmuma_ctl_struct_t volatile *partner_ctl;
    mca_bcol_basesmuma_ctl_struct_t volatile *my_ctl;
    int64_t bank_genaration;
    bool found;
    int pool_index=sm_desc->pool_index;
    mca_bcol_basesmuma_module_t *bcol_module=sm_desc->sm_module;

    /* get the pointer to the segment of control structures */
    idx=sm_desc->coll_buff->number_of_buffs+pool_index;
    leading_dim=sm_desc->coll_buff->size_of_group;
    idx=SM_ARRAY_INDEX(leading_dim,idx,0);
    ctl_structs=(mca_bcol_basesmuma_ctl_struct_t **)
        sm_desc->coll_buff->ctl_buffs+idx;
    bank_genaration= sm_desc->coll_buff->ctl_buffs_mgmt[pool_index].bank_gen_counter;
    
	my_exchange_node=&(bcol_module->recursive_doubling_tree);
    my_rank=bcol_module->super.sbgp_partner_module->my_index;
    my_ctl=ctl_structs[my_rank];
    /* debug print */
    /*
    {
	    int ii;
	    for(ii = 0; ii < 6; ii++) {
		    fprintf(stderr,"UUU ctl_struct[%d] := %p\n",ii,
			    bcol_module->colls_no_user_data.ctl_buffs[ii]);
		    fflush(stderr);
	    }
    }
    */
    /* end debug */

    /* signal that I have arrived */
    my_ctl->flag = -1;

    opal_atomic_wmb ();

	/* don't need to set this flag anymore */
    my_ctl->sequence_number = bank_genaration;

    if(0 < my_exchange_node->n_extra_sources) {
        if (EXCHANGE_NODE == my_exchange_node->node_type) {
            volatile int64_t *partner_sn;
            /* I will participate in the exchange - wait for signal from extra
             ** process */
            extra_rank = my_exchange_node->rank_extra_source;
            partner_ctl=ctl_structs[extra_rank];
            partner_sn=(volatile int64_t *)&(partner_ctl->sequence_number);

            /* spin n iterations until partner registers */
            loop_cnt=0;
            found=false;
            while( loop_cnt < bcol_module->super.n_poll_loops ) 
            {
                if( *partner_sn >= bank_genaration ) {
                    found=true;
                    break;
                }
                loop_cnt++;
            }
            if( !found ) {
                /* set restart parameters */
                sm_desc->collective_phase=NB_PRE_PHASE;
                return OMPI_SUCCESS;
            }

        }  else {

            /* Nothing to do, already registared that I am here */
        }
    }

    for(exchange = 0; exchange < my_exchange_node->n_exchanges; exchange++) {

        volatile int64_t *partner_sn;
        volatile int *partner_flag;

        /* rank of exchange partner */
        pair_rank = my_rank ^ ( 1 SHIFT_UP exchange );
        partner_ctl=ctl_structs[pair_rank];
        partner_sn=(volatile int64_t *)&(partner_ctl->sequence_number);
        partner_flag=(volatile int *)&(partner_ctl->flag);
		
        /* signal that I am at iteration exchange of the algorithm */
        my_ctl->flag = exchange;

        /* check to see if the partner has arrived */

        /* spin n iterations until partner registers */
        loop_cnt=0;
        found=false;
        while( loop_cnt < bcol_module->super.n_poll_loops ) 
        {
            if( (*partner_sn > bank_genaration) ||
                    ( *partner_sn == bank_genaration && 
                      *partner_flag >= exchange ) ) {
                found=true;
                break;
            }
			
             loop_cnt++;
        	
		}
        if( !found ) {
            /* set restart parameters */
            sm_desc->collective_phase=NB_RECURSIVE_DOUBLING;
            sm_desc->recursive_dbl_iteration=exchange;
            return OMPI_SUCCESS;
        }

    }

    if(0 < my_exchange_node->n_extra_sources)  {
        if ( EXTRA_NODE == my_exchange_node->node_type ) {
            volatile int64_t *partner_sn;
            volatile int *partner_flag;

            /* I will not participate in the exchange - 
             *   wait for signal from extra partner */
            extra_rank = my_exchange_node->rank_extra_source;
            partner_ctl=ctl_structs[extra_rank];
            partner_sn=(volatile int64_t *)&(partner_ctl->sequence_number);
            partner_flag=(volatile int *)&(partner_ctl->flag);

            /* spin n iterations until partner registers */
            loop_cnt=0;
            found=false;
            while( loop_cnt < bcol_module->super.n_poll_loops ) 
            {
                if( (*partner_sn > bank_genaration) ||
                        ( (*partner_sn == bank_genaration) && 
                        (*partner_flag == (my_exchange_node->log_2)) ) ) {
                    found=true;
                    break;
                }
                loop_cnt++;
			}
            if( !found ) {
                /* set restart parameters */
                sm_desc->collective_phase=NB_POST_PHASE;
                return OMPI_SUCCESS;
            }

        }  else {

            /* signal the extra rank that I am done with the recursive
             * doubling phase.
             */
            my_ctl->flag = my_exchange_node->n_exchanges;

        }
    }

    /* set the barrier as complete */
    sm_desc->collective_phase=NB_BARRIER_DONE;
    /* return */
    return ret;
}

/* admin nonblocking barrier - progress function */
int bcol_basesmuma_rd_nb_barrier_progress_admin( 
        sm_nbbar_desc_t *sm_desc)

{
    /* local variables */
    int ret=OMPI_SUCCESS, idx, leading_dim, loop_cnt, exchange;
    int pair_rank, start_index, restart_phase;
    mca_bcol_basesmuma_ctl_struct_t **ctl_structs;
    netpatterns_pair_exchange_node_t *my_exchange_node;
    int extra_rank, my_rank;
    mca_bcol_basesmuma_ctl_struct_t volatile *partner_ctl;
    mca_bcol_basesmuma_ctl_struct_t volatile *my_ctl;
    int64_t bank_genaration;
    int pool_index=sm_desc->pool_index;
    bool found;
    mca_bcol_basesmuma_module_t *bcol_module=sm_desc->sm_module;

    /* get the pointer to the segment of control structures */
    idx = sm_desc->coll_buff->number_of_buffs+pool_index;
    leading_dim = sm_desc->coll_buff->size_of_group;
    idx = SM_ARRAY_INDEX(leading_dim,idx,0);
    ctl_structs = (mca_bcol_basesmuma_ctl_struct_t **)
        sm_desc->coll_buff->ctl_buffs+idx;
    bank_genaration = sm_desc->coll_buff->ctl_buffs_mgmt[pool_index].bank_gen_counter;

    my_exchange_node=&(bcol_module->recursive_doubling_tree);
    my_rank=bcol_module->super.sbgp_partner_module->my_index;
    my_ctl=ctl_structs[my_rank];

    /* check to make sure that this should be progressed */
    if( ( sm_desc->collective_phase == NB_BARRIER_INACTIVE ) ||
        ( sm_desc->collective_phase == NB_BARRIER_DONE ) ) 
    {
        return OMPI_SUCCESS;
    }

    /* set the restart up - and jump to the correct place in the algorithm */
    restart_phase=sm_desc->collective_phase;
    if ( NB_PRE_PHASE == restart_phase ) {
        start_index=0;
    } else if ( NB_RECURSIVE_DOUBLING == restart_phase ) {
        start_index=sm_desc->recursive_dbl_iteration;
        goto Exchange_phase;
    } else {
        goto Post_phase;
    }

    if(0 < my_exchange_node->n_extra_sources) {
        if (EXCHANGE_NODE == my_exchange_node->node_type) {
            volatile int64_t *partner_sn;
            /* I will participate in the exchange - wait for signal from extra
             ** process */
            extra_rank = my_exchange_node->rank_extra_source;
            partner_ctl=ctl_structs[extra_rank];
            partner_sn=(volatile int64_t *)&(partner_ctl->sequence_number);

            /* spin n iterations until partner registers */
            loop_cnt=0;
            while( loop_cnt < bcol_module->super.n_poll_loops ) 
            {
                found=false;
                if( *partner_sn >= bank_genaration ) {
                    found=true;
                    break;
                }
                loop_cnt++;
            }
            if( !found ) {
                /* set restart parameters */
                sm_desc->collective_phase=NB_PRE_PHASE;
                return OMPI_SUCCESS;
            }

        }  else {

            /* Nothing to do, already registared that I am here */
        }
    }

Exchange_phase:

    for(exchange = start_index; 
        exchange < my_exchange_node->n_exchanges; exchange++) {

        volatile int64_t *partner_sn;
        volatile int *partner_flag;

        /* rank of exchange partner */
        pair_rank = my_rank ^ ( 1 SHIFT_UP exchange );
        partner_ctl=ctl_structs[pair_rank];
        partner_sn=(volatile int64_t *)&(partner_ctl->sequence_number);
        partner_flag=(volatile int *)&(partner_ctl->flag);

        /* signal that I am at iteration exchange of the algorithm */
        my_ctl->flag = exchange;

        /* check to see if the partner has arrived */

        /* spin n iterations until partner registers */
        loop_cnt=0;
        found=false;
        while( loop_cnt < bcol_module->super.n_poll_loops ) 
        {
            if( (*partner_sn > bank_genaration) ||
                    ( (*partner_sn == bank_genaration) && 
                      (*partner_flag >= exchange) ) ) {
                found=true;
                break;
            }
            loop_cnt++;
        }
        if( !found ) {
            /* set restart parameters */
            sm_desc->collective_phase=NB_RECURSIVE_DOUBLING;
            sm_desc->recursive_dbl_iteration=exchange;
            return OMPI_SUCCESS;
        }

    }

Post_phase:
    if(0 < my_exchange_node->n_extra_sources)  {
        if ( EXTRA_NODE == my_exchange_node->node_type ) {
            volatile int64_t *partner_sn;
            volatile int *partner_flag;

            /* I will not participate in the exchange - 
             *   wait for signal from extra partner */
            extra_rank = my_exchange_node->rank_extra_source;
            partner_ctl=ctl_structs[extra_rank];
            partner_sn=(volatile int64_t *)&(partner_ctl->sequence_number);
            partner_flag=(volatile int *)&(partner_ctl->flag);

            /* spin n iterations until partner registers */
            loop_cnt=0;
            found=false;
            while( loop_cnt < bcol_module->super.n_poll_loops ) 
            {
                if( (*partner_sn > bank_genaration) ||
                        ( *partner_sn == bank_genaration && 
                        *partner_flag == (my_exchange_node->log_2) ) ) {
                    found=true;
                    break;
                }
                loop_cnt++;
            }
            if( !found ) {
                /* set restart parameters */
                sm_desc->collective_phase=NB_POST_PHASE;
                return OMPI_SUCCESS;
            }

        }  else {

            /* signal the extra rank that I am done with the recursive
             * doubling phase.
             */
            my_ctl->flag = my_exchange_node->n_exchanges;

        }
    }

    /* set the barrier as complete */
    sm_desc->collective_phase=NB_BARRIER_DONE;

    /* return */
    return ret;
}

static int bcol_basesmuma_memsync(bcol_function_args_t *input_args,
                mca_bcol_base_function_t *c_input_args)
{
    int rc;
    int memory_bank = input_args->root;

    mca_bcol_basesmuma_module_t* bcol_module =
        (mca_bcol_basesmuma_module_t *)c_input_args->bcol_module;
    sm_buffer_mgmt *buff_block = &(bcol_module->colls_with_user_data);
    sm_nbbar_desc_t *sm_desc = &(buff_block->ctl_buffs_mgmt[memory_bank].nb_barrier_desc);

    sm_desc->coll_buff = buff_block;
    /*
    printf("XXX SYNC call\n");
    */

    rc = bcol_basesmuma_rd_nb_barrier_init_admin( 
            sm_desc);
    if (OMPI_SUCCESS != rc) {
        return rc;
    }
    
    if (NB_BARRIER_DONE != sm_desc->collective_phase) {
        mca_bcol_basesmuma_component_t *cs = &mca_bcol_basesmuma_component;
        opal_list_t *list=&(cs->nb_admin_barriers);
        opal_list_item_t *append_item;

        /* put this onto the progression list */
        OPAL_THREAD_LOCK(&(cs->nb_admin_barriers_mutex));
        append_item=(opal_list_item_t *)
            &(buff_block->ctl_buffs_mgmt[memory_bank].nb_barrier_desc);
        opal_list_append(list,append_item);
        OPAL_THREAD_UNLOCK(&(cs->nb_admin_barriers_mutex));
        /* progress communications so that resources can be freed up */
        return BCOL_FN_STARTED;
    }

    /* Done - bump the counter */
    (buff_block->ctl_buffs_mgmt[memory_bank].bank_gen_counter)++;
    /*
    printf("XXX SYNC call done \n");
    */
    return BCOL_FN_COMPLETE;
}

static int bcol_basesmuma_memsync_progress(bcol_function_args_t *input_args,
                mca_bcol_base_function_t *c_input_args)
{
    int memory_bank = input_args->root;

    mca_bcol_basesmuma_module_t* bcol_module =
        (mca_bcol_basesmuma_module_t *)c_input_args->bcol_module;
    sm_buffer_mgmt *buff_block = &(bcol_module->colls_with_user_data);
    sm_nbbar_desc_t *sm_desc = &(buff_block->ctl_buffs_mgmt[memory_bank].nb_barrier_desc);

    /* I do not have to do anything, since the 
       progress done by basesmuma progress engine */

    if (NB_BARRIER_DONE != sm_desc->collective_phase) {
        return BCOL_FN_STARTED;
    }

    return BCOL_FN_COMPLETE;
}

int bcol_basesmuma_memsync_init(mca_bcol_base_module_t *super)
{
    mca_bcol_base_coll_fn_comm_attributes_t comm_attribs;
    mca_bcol_base_coll_fn_invoke_attributes_t inv_attribs;

    comm_attribs.bcoll_type = BCOL_SYNC;

    comm_attribs.comm_size_min = 0;
    comm_attribs.comm_size_max = 1024 * 1024;
    comm_attribs.waiting_semantics = NON_BLOCKING;

    inv_attribs.bcol_msg_min = 0;
    inv_attribs.bcol_msg_max = 20000; /* range 1 */

    inv_attribs.datatype_bitmap = 0xffffffff;
    inv_attribs.op_types_bitmap = 0xffffffff;

    comm_attribs.data_src = DATA_SRC_KNOWN;

    mca_bcol_base_set_attributes(super,
            &comm_attribs, &inv_attribs,
            bcol_basesmuma_memsync,
            bcol_basesmuma_memsync_progress);

    return OMPI_SUCCESS;
}
