/*
 * Copyright (c) 2009-2013 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2013-2014 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/* Recursive doubling blocking barrier */

#include "ompi_config.h"
#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/bcol/bcol.h"
#include "ompi/patterns/net/netpatterns.h"

#include "opal/sys/atomic.h"

#include "bcol_basesmuma.h"

#if 0
int bcol_basesmuma_recursive_double_barrier(bcol_function_args_t *input_args,
                                            mca_bcol_base_function_t *c_input_args)
{

    /* local variables */
    int ret=OMPI_SUCCESS, idx, leading_dim, loop_cnt, exchange, flag_to_set;
    int pair_rank, flag_offset;
    mca_bcol_basesmuma_ctl_struct_t **ctl_structs;
    netpatterns_pair_exchange_node_t *my_exchange_node;
    int extra_rank, my_rank, pow_2;
    volatile mca_bcol_basesmuma_ctl_struct_t *partner_ctl;
    volatile mca_bcol_basesmuma_ctl_struct_t *my_ctl;
    int64_t sequence_number;
    bool found;
    int buff_index, first_instance=0;
    mca_bcol_basesmuma_module_t* bcol_module =
        (mca_bcol_basesmuma_module_t *)c_input_args->bcol_module;
#if 0
    fprintf(stderr,"Entering the sm rd barrier\n");
    fflush(stderr);
#endif

    /* get the pointer to the segment of control structures */
    my_exchange_node=&(bcol_module->recursive_doubling_tree);
    my_rank=bcol_module->super.sbgp_partner_module->my_index;
    pow_2=bcol_module->super.sbgp_partner_module->pow_2;

    /* figure out what instance of the basesmuma bcol I am */
    leading_dim=bcol_module->colls_no_user_data.size_of_group;
    sequence_number=input_args->sequence_num - c_input_args->bcol_module->squence_number_offset;

    buff_index=sequence_number & (bcol_module->colls_no_user_data.mask);

    idx=SM_ARRAY_INDEX(leading_dim,buff_index,0);
    ctl_structs=(mca_bcol_basesmuma_ctl_struct_t **)
        bcol_module->colls_no_user_data.ctl_buffs+idx;
    my_ctl=ctl_structs[my_rank];
    if( my_ctl->sequence_number < sequence_number ) {
        first_instance=1;
    }

    /* get the pool index */
    if( first_instance ) {
        idx = -1;
        while( idx == -1 ) {

            idx=bcol_basesmuma_get_buff_index(
                &(bcol_module->colls_no_user_data),sequence_number);
        }
        if( -1 == idx ){
            return ORTE_ERR_TEMP_OUT_OF_RESOURCE;
        }
        my_ctl->index=1;
        /* this does not need to use any flag values , so only need to
         * set the value for subsequent values that may need this */
        my_ctl->starting_flag_value=0;
        flag_offset=0;
    } else {
        /* only one thread at a time will be making progress on this
         *   collective, so no need to make this atomic */
        my_ctl->index++;
        flag_offset=my_ctl->starting_flag_value;
    }

    /* signal that I have arrived */
    my_ctl->flag = -1;
    /* don't need to set this flag anymore */
    my_ctl->sequence_number = sequence_number;
    /* opal_atomic_wmb ();*/

    if(0 < my_exchange_node->n_extra_sources) {
        if (EXCHANGE_NODE == my_exchange_node->node_type) {
            volatile int64_t *partner_sn;
            int cnt=0;

            /* I will participate in the exchange - wait for signal from extra
            ** process */
            extra_rank = my_exchange_node->rank_extra_source;
            partner_ctl=(volatile mca_bcol_basesmuma_ctl_struct_t *)ctl_structs[extra_rank];

            /*partner_ctl=ctl_structs[extra_rank];*/
            partner_sn=(volatile int64_t *)&(partner_ctl->sequence_number);

            /* spin n iterations until partner registers */
            loop_cnt=0;
            found=false;
            while( !found )
            {
                if( *partner_sn >= sequence_number ) {
                    found=true;
                }
                cnt++;
                if( cnt == 1000 ) {
                    opal_progress();
                    cnt=0;
                }
            }

        }  else {

            /* Nothing to do, already registared that I am here */
        }
    }

    for(exchange = 0; exchange < my_exchange_node->n_exchanges; exchange++) {

        volatile int64_t *partner_sn;
        volatile int *partner_flag;
        int cnt=0;

        /* rank of exchange partner */
        pair_rank = my_rank ^ ( 1 SHIFT_UP exchange );
        partner_ctl=ctl_structs[pair_rank];
        partner_sn=(volatile int64_t *)&(partner_ctl->sequence_number);
        partner_flag=(volatile int *)&(partner_ctl->flag);

        /* signal that I am at iteration exchange of the algorithm */
        flag_to_set=flag_offset+exchange;
        my_ctl->flag = flag_to_set;

        /* check to see if the partner has arrived */

        /* spin n iterations until partner registers */
        found=false;
        while( !found )
        {
            if( (*partner_sn > sequence_number) ||
                ( *partner_sn == sequence_number &&
                  *partner_flag >= flag_to_set ) ) {
                found=true;
            }  else {
                cnt++;
                if( cnt == 1000 ) {
                    opal_progress();
                    cnt=0;
                }
            }
        }
    }

    if(0 < my_exchange_node->n_extra_sources)  {
        if ( EXTRA_NODE == my_exchange_node->node_type ) {
            int cnt=0;

            /* I will not participate in the exchange -
             *   wait for signal from extra partner */
            extra_rank = my_exchange_node->rank_extra_source;
            partner_ctl=ctl_structs[extra_rank];
            flag_to_set=flag_offset+my_exchange_node->log_2;

            /* spin n iterations until partner registers */
            found=false;
            while( !found )
            {
                if (IS_PEER_READY(partner_ctl, flag_to_set, sequence_number)){
                    found=true;
                } else {
                    cnt++;
                    if( cnt == 1000 ) {
                        opal_progress();
                        cnt=0;
                    }
                }
            }

        }  else {

            /* signal the extra rank that I am done with the recursive
             * doubling phase.
             */
            flag_to_set=flag_offset+my_exchange_node->log_2;
            my_ctl->flag = flag_to_set;

        }
    }

    /* if I am the last instance of a basesmuma function in this collectie,
     *   release the resrouces */
    if (IS_LAST_BCOL_FUNC(c_input_args)){
        idx=bcol_basesmuma_free_buff(
            &(bcol_module->colls_no_user_data),
            sequence_number);
    }  else {
        /* increment flag value - so next sm collective in the hierarchy
         *    will not collide with the current one, as they share the
         *    control structure */
        my_ctl->starting_flag_value+=(my_exchange_node->log_2+1);
    }

    /* return */
    return ret;
}
#endif
