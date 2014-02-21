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

#include "ompi_config.h"

/* #define __PORTALS_AVAIL__ */
#ifdef __PORTALS_AVAIL__

#define __PORTALS_ENABLE__
#include "ompi/mca/bcol/basesmuma/bcol_basesmuma.h"
#include "ompi/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "bcol_basesmuma_utils.h"

#include "bcol_basesmuma_portals.h"

/* debug */
#include <unistd.h>
/* end debug */


/**
 * Shared memory non-blocking Broadcast - K-nomial fan-out for small data buffers.
 * This routine assumes that buf (the input buffer) is a single writer
 * multi reader (SWMR) shared memory buffer owned by the calling rank
 * which is the only rank that can write to this buffers.
 * It is also assumed that the buffers are registered and fragmented
 * at the ML level and that buf is sufficiently large to hold the data.
 *
 *
 * @param buf - SWMR shared buffer within a sbgp that the
 * executing rank can write to.
 * @param count - the number of elements in the shared buffer.
 * @param dtype - the datatype of a shared buffer element.
 * @param root - the index within the sbgp of the root.
 * @param module - basesmuma module.
 */
int bcol_basesmuma_lmsg_bcast_k_nomial_anyroot(bcol_function_args_t *input_args,
    mca_bcol_base_function_t *c_input_args)
{
#if 0
		/* local variables */
    mca_bcol_basesmuma_module_t* bcol_module=
        (mca_bcol_basesmuma_module_t *)c_input_args->bcol_module;
    mca_bcol_basesmuma_component_t *cs = &mca_bcol_basesmuma_component;
    int i, matched = 0;
    int src=-1;
    int group_size;
    int my_rank, first_instance=0, flag_offset;
    int rc = OMPI_SUCCESS;
    int leading_dim, buff_idx, idx;
    int count=input_args->count;
    struct ompi_datatype_t* dtype=input_args->dtype;
    int64_t sequence_number=input_args->sequence_num;

	volatile int64_t ready_flag;
    volatile mca_bcol_basesmuma_payload_t *data_buffs;
    volatile char* parent_data_pointer;
    volatile mca_bcol_basesmuma_header_t *parent_ctl_pointer;
    volatile mca_bcol_basesmuma_header_t *my_ctl_pointer;
    void *userbuf = (void *)((unsigned char *)input_args->userbuf);

    size_t pack_len = 0, dt_size;

    struct mca_bcol_basesmuma_portal_buf_addr_t *my_lmsg_ctl_pointer = NULL;
    struct mca_bcol_basesmuma_portal_buf_addr_t *parent_lmsg_ctl_pointer = NULL;
	mca_bcol_basesmuma_portal_proc_info_t *portals_info;
	portals_info = (mca_bcol_basesmuma_portal_proc_info_t*)cs->portals_info;

    /* we will work only on packed data - so compute the length*/
    ompi_datatype_type_size(dtype, &dt_size);
    pack_len=count*dt_size;
    buff_idx = input_args->src_desc->buffer_index;

    /* Get addressing information */
    my_rank = bcol_module->super.sbgp_partner_module->my_index;
    group_size = bcol_module->colls_no_user_data.size_of_group;
    leading_dim=bcol_module->colls_no_user_data.size_of_group;
    idx=SM_ARRAY_INDEX(leading_dim,buff_idx,0);

    data_buffs=(volatile mca_bcol_basesmuma_payload_t *)
        bcol_module->colls_with_user_data.data_buffs+idx;

    /* Set pointer to current proc ctrl region */
    my_ctl_pointer = data_buffs[my_rank].ctl_struct;
	my_lmsg_ctl_pointer = (mca_bcol_basesmuma_portal_buf_addr_t*) data_buffs[my_rank].payload;

    /* setup resource recycling */
    if( my_ctl_pointer->sequence_number < sequence_number ) {
        first_instance=1;
    }

	if( first_instance ) {
        /* Signal arrival */
        my_ctl_pointer->flag = -1;
        my_ctl_pointer->index=1;
        /* this does not need to use any flag values , so only need to
         * set the value for subsequent values that may need this */
        my_ctl_pointer->starting_flag_value=0;
        flag_offset=0;

    } else {
        /* only one thread at a time will be making progress on this
         *   collective, so no need to make this atomic */
        my_ctl_pointer->index++;
    }


    /* increment the starting flag by one and return */
    flag_offset = my_ctl_pointer->starting_flag_value;
    ready_flag = flag_offset + sequence_number + 1;
    my_ctl_pointer->sequence_number = sequence_number;


	/* Construct my portal buffer address and copy to payload buffer */
	mca_bcol_basesmuma_construct_portal_address(my_lmsg_ctl_pointer,
						portals_info->portal_id.nid,
						portals_info->portal_id.pid,
						sequence_number,
						bcol_module->super.sbgp_partner_module->group_comm->c_contextid);

    /* non-blocking broadcast algorithm */

    /* If I am the root, then signal ready flag */
    if(input_args->root_flag) {
		ptl_handle_eq_t eq_h;
		ptl_event_t event;
		int ret;

        BASESMUMA_VERBOSE(10,("I am the root of the data"));

		/* create an event queue for the incoming buffer */
	ret = PtlEQAlloc(((mca_bcol_basesmuma_portal_proc_info_t*)
				cs->portals_info)->ni_h, MAX_PORTAL_EVENTS_IN_Q, PTL_EQ_HANDLER_NONE, &eq_h);

		if (ret != PTL_OK) {
	  fprintf(stderr, "PtlEQAlloc() failed: %d \n",ret);
			return OMPI_ERR_OUT_OF_RESOURCE;
	}

		/* Post the message using portal copy */

		 mca_bcol_basesmuma_portals_post_msg_nb_nopers(cs, my_lmsg_ctl_pointer, userbuf,
							pack_len, eq_h, my_lmsg_ctl_pointer->nsends);

		/*
         * signal ready flag
         */
        my_ctl_pointer->flag = ready_flag;

		/* wait for a response from the client */
	mca_bcol_basesmuma_portals_wait_event_nopers(eq_h, POST_MSG_EVENT,
					&event, my_lmsg_ctl_pointer->nsends);

		/* free the event queue */
		ret = PtlEQFree(eq_h);
		if (ret != PTL_OK) {
		    fprintf(stderr, "PtlEQFree() failed: %d )\n",ret);
		}

        /* root is finished */
        goto Release;
    }

    /* If I am not the root, then poll on possible "senders'" control structs */
    for( i = 0; i < cs->num_to_probe && 0 == matched; i++) {

        /* Shared memory iprobe */
		/*
		BCOL_BASESMUMA_SM_PROBE(bcol_module->src, bcol_module->src_size,
                my_rank, matched, src);
		*/
		do {
			int j, n_src, my_index;
			n_src = bcol_module->src_size;

			for( j = 0; j < n_src; j++) {
			parent_ctl_pointer = data_buffs[bcol_module->src[j]].ctl_struct;
			parent_lmsg_ctl_pointer = (mca_bcol_basesmuma_portal_buf_addr_t *)
											data_buffs[bcol_module->src[j]].payload;
			if (IS_DATA_READY(parent_ctl_pointer,ready_flag,sequence_number)) {

					src = bcol_module->src[j];
			matched = 1;
			break;
			}
		}
		} while(0);

    }

    /* If not matched, then hop out and put me on progress list */
    if(0 == matched ) {
        BASESMUMA_VERBOSE(10,("Shared memory probe didn't find a match"));
        return BCOL_FN_NOT_STARTED;
    }

    /* else, we found our root within the group ... */
    BASESMUMA_VERBOSE(10,("Shared memory probe was matched, the root is %d", src));

    /* receive the data from sender */
    /* get the data buff */
    /* taken care of in the macro */
    /*parent_data_pointer = data_buffs[src].payload;*/
    /* copy the data */
	mca_bcol_basesmuma_portals_get_msg(cs, parent_lmsg_ctl_pointer, userbuf, pack_len);

    /* set the memory barrier to ensure completion */
    opal_atomic_wmb ();
    /* signal that I am done */
    my_ctl_pointer->flag = ready_flag;

    /* am I the last one? If so, release buffer */

Release:
    my_ctl_pointer->starting_flag_value++;

    return BCOL_FN_COMPLETE;
#endif
}

#if 0

#define BASESMUMA_K_NOMIAL_SEND_SIGNAL(radix_mask, radix, my_relative_index,		\
		my_group_index, group_size,sm_data_buffs,sender_ready_flag,			\
				num_pending_sends) 													\
{																					\
    int k, rc;																  	    \
    int dst; 			                                               			    \
	int comm_dst;																	\
    volatile mca_bcol_basesmuma_header_t *recv_ctl_pointer = NULL;					\
	volatile mca_bcol_basesmuma_portal_buf_addr_t  *recv_lmsg_ctl_pointer = NULL;   \
                                                                                    \
    num_pending_sends = 0;													        \
    while(radix_mask > 0) {															\
        /* For each level of tree, do sends */										\
        for (k = 1;																	\
			k < radix && my_relative_index + radix_mask * k < group_size;  	  		\
			++k) {   	                                              	   			\
                                                                                    \
            dst = my_group_index + radix_mask * k;                        		    \
            if (dst >= group_size) {												\
                dst -= group_size;													\
            }                                                                	    \
			/* Signal the children to get data */									\
			recv_ctl_pointer	  = data_buffs[dst].ctl;							\
			recv_lmsg_ctl_pointer = (mca_bcol_basesmuma_portal_buf_addr_t *)		\
											data_buffs[dst].payload;				\
			recv_lmsg_ctl_pointer->src_index = my_group_index;						\
			recv_lmsg_ctl_pointer->flag = sender_ready_flag;						\
            ++num_pending_sends;													\
        }																			\
        radix_mask /= radix;														\
    }                                                                      			\
																		    \
}



int bcol_basesmuma_lmsg_bcast_k_nomial_anyroot(bcol_function_args_t *input_args,
    mca_bcol_base_function_t *c_input_args)
{
    /* local variables */
    mca_bcol_basesmuma_module_t* bcol_module=
        (mca_bcol_basesmuma_module_t *)c_input_args->bcol_module;
    mca_bcol_basesmuma_component_t *cs = &mca_bcol_basesmuma_component;
    int i, matched = 0;
    int src=-1;
    int group_size;
    int my_rank, first_instance=0, flag_offset;
    int rc = OMPI_SUCCESS;
    int leading_dim, buff_idx, idx;
    int count=input_args->count;
    struct ompi_datatype_t* dtype=input_args->dtype;
    int64_t sequence_number=input_args->sequence_num;

	volatile int64_t ready_flag;
    volatile mca_bcol_basesmuma_payload_t *data_buffs;
    volatile char* parent_data_pointer;
    volatile mca_bcol_basesmuma_header_t *parent_ctl_pointer;
    volatile mca_bcol_basesmuma_header_t *my_ctl_pointer;
    void *userbuf = (void *)((unsigned char *)input_args->userbuf);

    size_t pack_len = 0, dt_size;

    struct mca_bcol_basesmuma_portal_buf_addr_t *my_lmsg_ctl_pointer = NULL;
    struct mca_bcol_basesmuma_portal_buf_addr_t *parent_lmsg_ctl_pointer = NULL;
	mca_bcol_basesmuma_portal_proc_info_t *portals_info;
	portals_info = (mca_bcol_basesmuma_portal_proc_info_t*)cs->portals_info;

    /* we will work only on packed data - so compute the length*/
    ompi_datatype_type_size(dtype, &dt_size);
    pack_len=count*dt_size;
    buff_idx = input_args->src_desc->buffer_index;

    /* Get addressing information */
    my_rank = bcol_module->super.sbgp_partner_module->my_index;
    group_size = bcol_module->colls_no_user_data.size_of_group;
    leading_dim=bcol_module->colls_no_user_data.size_of_group;
    idx=SM_ARRAY_INDEX(leading_dim,buff_idx,0);

    data_buffs=(volatile mca_bcol_basesmuma_payload_t *)
        bcol_module->colls_with_user_data.data_buffs+idx;

    /* Set pointer to current proc ctrl region */
    my_ctl_pointer = data_buffs[my_rank].ctl_struct;
	my_lmsg_ctl_pointer = (mca_bcol_basesmuma_portal_buf_addr_t*) data_buffs[my_rank].payload;

    /* setup resource recycling */
    if( my_ctl_pointer->sequence_number < sequence_number ) {
        first_instance=1;
    }

	if( first_instance ) {
        /* Signal arrival */
        my_ctl_pointer->flag = -1;
        my_ctl_pointer->index=1;
        /* this does not need to use any flag values , so only need to
         * set the value for subsequent values that may need this */
        my_ctl_pointer->starting_flag_value=0;
        flag_offset=0;

    } else {
        /* only one thread at a time will be making progress on this
         *   collective, so no need to make this atomic */
        my_ctl_pointer->index++;
    }


    /* increment the starting flag by one and return */
    flag_offset = my_ctl_pointer->starting_flag_value;
    ready_flag = flag_offset + sequence_number + 1;
    my_ctl_pointer->sequence_number = sequence_number;


	/* Construct my portal buffer address and copy to payload buffer */
	mca_bcol_basesmuma_construct_portal_address(my_lmsg_ctl_pointer,
						portals_info->portal_id.nid,
						portals_info->portal_id.pid,
						sequence_number,
						bcol_module->super.sbgp_partner_module->group_comm->c_contextid);

	my_lmsg_ctl_pointer->userbuf = userbuff;
	my_lsmg_ctl_pointer->userbuf_length = fragment_length;
	/* create an event queue  */
	ret = PtlEQAlloc(((mca_bcol_basesmuma_portal_proc_info_t*)
				cs->portals_info)->ni_h, MAX_PORTAL_EVENTS_IN_Q, PTL_EQ_HANDLER_NONE, &eq_h);

    /* non-blocking broadcast algorithm */

    /* If I am the root, then signal ready flag */
    if(input_args->root_flag) {
		ptl_handle_eq_t eq_h;
		ptl_event_t event;
		int ret;
		int root_radix_mask = sm_module->pow_knum;

        BASESMUMA_VERBOSE(10,("I am the root of the data"));


		if (ret != PTL_OK) {
	  fprintf(stderr, "PtlEQAlloc() failed: %d \n",ret);
			return OMPI_ERR_OUT_OF_RESOURCE;
	}

		BASESMUMA_K_NOMIAL_SEND_SIGNAL(root_radix_mask, radix, 0,
			my_rank, group_size, data_buffs, ready_flag, nsends) ;

		mca_bcol_basesmuma_portals_post_msg_nb_nopers(cs, my_lmsg_ctl_pointer, userbuf,
							pack_len, eq_h, nsends);

		/* wait for a response from the client */
	mca_bcol_basesmuma_portals_wait_event_nopers(eq_h, POST_MSG_EVENT,
					&event, nsends);

        /* root is finished */
        goto Release;
    }

	/* Im not a root so wait until someone puts data and
	 * compute where to get data from */

	while (my_ctl_pointer->flag != ready_flag) ;

	my_data_source_index = lmsg_ctl_pointer->src_index;

	parent_lmsg_ctl_pointer = (mca_bcol_basesmuma_portal_buf_addr_t *)
											data_buffs[my_data_source_index].payload;

	mca_bcol_basesmuma_portals_get_msg(cs, parent_lmsg_ctl_pointer, userbuf, pack_len);




	/* I am done getting data, should I send the data to someone  */

	my_relative_index = (my_rank - my_data_source_index) < 0 ? my_rank -
		my_data_source_index  + group_size : my_rank - my_data_source_index;

	/*
     * 2. Locate myself in the tree:
     * calculate number of radix steps that we should to take
     */
    radix_mask = 1;
    while (radix_mask < group_size) {
        if (0 != my_relative_index % (radix * radix_mask)) {
            /* I found my level in tree */
            break;
        }
        radix_mask *= radix;
    }

	/* go one step back */
    radix_mask /=radix;

	BASESMUMA_K_NOMIAL_SEND_SIGNAL(radix_mask, radix, my_relative_index,
		my_rank, group_size,data_buffs,ready_flag,nsends)

	mca_bcol_basesmuma_portals_post_msg_nb_nopers(cs, my_lmsg_ctl_pointer, userbuf,
							pack_len, eq_h, nsends);

	/* wait for childrens to read */
    mca_bcol_basesmuma_portals_wait_event_nopers(eq_h, POST_MSG_EVENT,
					&event, nsends);



Release:
	/* free the event queue */
	ret = PtlEQFree(eq_h);
	if (ret != PTL_OK) {
		    fprintf(stderr, "PtlEQFree() failed: %d )\n",ret);
	}


    my_ctl_pointer->starting_flag_value++;

    return BCOL_FN_COMPLETE;
}

#endif
#endif
