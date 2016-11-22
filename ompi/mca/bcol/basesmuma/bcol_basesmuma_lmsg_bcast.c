/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#ifdef __PORTALS_AVAIL__
#define __PORTALS_ENABLE__

#include "ompi/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"

#include "bcol_basesmuma.h"
#include "bcol_basesmuma_portals.h"
#include "bcol_basesmuma_lmsg_bcast.h"
#include "bcol_basesmuma_utils.h"



/*
 * Scatter/Gather Broadcast algorithm
 *
 * Algorithm highlights:
 *
 * Uses portals for data transfer
 *
 * All processes participating in the broadcast are arranged in a
 * binmoial tree.
 *
 * Phase1: Scatter the broadcast data to all the children
 * Phase2: All processes in the tree participates in recursive doubling
 * algorithm to obtain the missing data.
 */


static int completed_scatter = 0;
#if 0
int bcol_basesmuma_lmsg_scatter_allgather_portals_bcast_old(bcol_function_args_t *input_args,
    mca_bcol_base_function_t *c_input_args)
{

    /* local variables */
    int i;
    uint64_t length;
    int my_rank, parent_rank, src =-1, matched = 0;
    int *src_list = NULL;
    int group_size = -1, dummy_group_size;
    int first_instance=0;
    int rc = OMPI_SUCCESS;
    int leading_dim, buff_idx, idx;
    int count=input_args->count;
    size_t pack_len = 0, dt_size =0 ;
	int64_t ready_flag;
    int flag_offset;
    int pow_2, pow_2_levels;
    int src_list_index = -1;
    uint64_t fragment_size;  /* user buffer size */
	int sg_matchbits = 0;
	/* Input argument variables */
	void *my_userbuf = (void*)((unsigned char*)input_args->userbuf);
	int64_t sequence_number=input_args->sequence_num;
    struct ompi_datatype_t* dtype=input_args->dtype;

	/* Extra source variables */
	bool secondary_root = false;
	int partner = -1, extra_partner = -1;

	/* Scatter Allgather offsets */
	uint64_t local_sg_offset = 0, global_sg_offset = 0, partner_offset = 0;

	/* Portals messaging relevant variables */
	mca_bcol_basesmuma_portal_proc_info_t *portals_info;
	ptl_handle_eq_t allgather_eq_h;
	ptl_event_t  allgather_event;
	bool blocked_post = false;
	bool msg_posted = false;
	int total_msg_posts = -1, scatter_posts = -1, allgather_posts = -1, extra_src_posts = -1;

	/* OMPI module and component variables */
    mca_bcol_basesmuma_component_t *cs = &mca_bcol_basesmuma_component;
    mca_bcol_basesmuma_module_t *bcol_module =
        (mca_bcol_basesmuma_module_t *) c_input_args->bcol_module;

    /* Control structure and payload variables */
    volatile mca_bcol_basesmuma_payload_t *data_buffs;
	volatile mca_bcol_basesmuma_header_t *my_ctl_pointer = NULL;
	volatile mca_bcol_basesmuma_header_t *parent_ctl_pointer = NULL;
	volatile mca_bcol_basesmuma_header_t *partner_ctl_pointer = NULL;

	struct mca_bcol_basesmuma_portal_buf_addr_t *my_lmsg_ctl_pointer = NULL;
	struct mca_bcol_basesmuma_portal_buf_addr_t *parent_lmsg_ctl_pointer = NULL;
	struct mca_bcol_basesmuma_portal_buf_addr_t *partner_lmsg_ctl_pointer = NULL;

	/* Make sure there userbuffer is not null */
	assert(my_userbuf != NULL);

    /* Get portals info*/
	portals_info = (mca_bcol_basesmuma_portal_proc_info_t*)cs->portals_info;

	/* Get addresing information */
    buff_idx = input_args->src_desc->buffer_index;
    group_size = bcol_module->colls_no_user_data.size_of_group;
    leading_dim=bcol_module->colls_no_user_data.size_of_group;
    my_rank = bcol_module->super.sbgp_partner_module->my_index;
    idx=SM_ARRAY_INDEX(leading_dim,buff_idx,0);

    /* calculate the largest power of two that is smaller than
     * or equal to the group size
     */
    pow_2_levels = pow_sm_k(2,group_size, &(dummy_group_size));
    if( group_size < (1<<pow_2_levels)) {
        pow_2_levels--;
    }
    /* power-of-two group size */
    pow_2 = 1<<pow_2_levels;


     /* we will work only on packed data - so compute the length*/
    ompi_datatype_type_size(dtype, &dt_size);
    fragment_size = count*dt_size;

    /* grab the data buffs */
    data_buffs = (mca_bcol_basesmuma_payload_t *)
        bcol_module->colls_with_user_data.data_buffs+idx;

    my_ctl_pointer = data_buffs[my_rank].ctl_struct;
	my_lmsg_ctl_pointer = (mca_bcol_basesmuma_portal_buf_addr_t*)
							data_buffs[my_rank].payload;

    if(my_ctl_pointer->sequence_number < sequence_number) {
        first_instance = 1;
    }

    if(first_instance) {
        my_ctl_pointer->flag = -1;
        my_ctl_pointer->index = 1;

        my_ctl_pointer->starting_flag_value = 0;
        flag_offset = 0;

    } else {
        my_ctl_pointer->index++;
    }

	assert( -1 == my_ctl_pointer->flag);

    /* increment the starting flag by one and return */
    flag_offset = my_ctl_pointer->starting_flag_value;
    ready_flag = flag_offset + sequence_number + 1;

    my_ctl_pointer->sequence_number = sequence_number;
	sg_matchbits = sequence_number ;

	/* Construct my portal buffer address and copy to payload buffer */
	mca_bcol_basesmuma_construct_portal_address(my_lmsg_ctl_pointer,
						portals_info->portal_id.nid,
						portals_info->portal_id.pid,
						sg_matchbits,
						bcol_module->super.sbgp_partner_module->group_comm->c_contextid);

	my_lmsg_ctl_pointer->userbuf = my_userbuf;
	my_lmsg_ctl_pointer->userbuf_length = fragment_size;


	/*
	 * If I am the root of bcast, scatter the data to my children
	 */
    if (input_args->root_flag) {
        BASESMUMA_VERBOSE(10,("I am the root of the data"));
        my_lmsg_ctl_pointer->offset = 0;
        my_lmsg_ctl_pointer->n_sends = pow_2_levels;
        my_lmsg_ctl_pointer->length = fragment_size;

	rc = PtlEQAlloc(((mca_bcol_basesmuma_portal_proc_info_t*)
				cs->portals_info)->ni_h, MAX_PORTAL_EVENTS_IN_Q,
						PTL_EQ_HANDLER_NONE, &allgather_eq_h);

		if (rc != PTL_OK) {
		BASESMUMA_VERBOSE(10,( "PtlEQAlloc() failed: %d \n",rc));
			goto Release;
	}

		/* Compute number of posts required
		 * We post the data buffer for both scatter and allgather phase at once so to avoid
		 * posting overhead
		 */
		if (my_rank >= pow_2) {
			/* I am root and my rank is greater than pow_2, I will hand
			 * over to rank (that is < pow_2) to act as secondary root
			 */
			total_msg_posts  = 1;
		}
		else {

			extra_src_posts = (my_rank + pow_2 < group_size ) ? 1: 0;
			scatter_posts = my_lmsg_ctl_pointer->n_sends;
			allgather_posts = pow_2_levels - 1;

			total_msg_posts = scatter_posts + allgather_posts + extra_src_posts ;
		}

		 mca_bcol_basesmuma_portals_post_msg(cs, my_lmsg_ctl_pointer,
						   my_userbuf, fragment_size, allgather_eq_h,
						   total_msg_posts,
						   blocked_post,
						  PTL_MD_EVENT_START_DISABLE| PTL_MD_EVENT_END_DISABLE | PTL_MD_OP_GET | PTL_MD_MANAGE_REMOTE | PTL_MD_TRUNCATE | PTL_MD_EVENT_AUTO_UNLINK_ENABLE);
		 msg_posted = true ;
        /* important that these be set before my children
         * see the ready flag raised
         */
        opal_atomic_wmb ();
        my_ctl_pointer->flag = ready_flag;

		/* Wait for my scatter partner */
		if (my_rank >= pow_2) {
			int scatter_partner = -1;
			volatile mca_bcol_basesmuma_header_t *scatter_partner_ctl_pointer = NULL;

			scatter_partner = my_rank - pow_2;
			scatter_partner_ctl_pointer =
					data_buffs[scatter_partner].ctl_struct;

			while(!IS_SG_DATA_READY(scatter_partner_ctl_pointer, ready_flag,
									sequence_number)){
					opal_progress();
			}

			goto Release;
		}
		else {
			wait_for_peers(my_rank, my_lmsg_ctl_pointer->n_sends, data_buffs,
							ready_flag, sequence_number);
		}

		goto Allgather;
    }


Extra :
    if( my_rank >= pow_2 ) {
        parent_rank = my_rank & (pow_2-1);
        parent_ctl_pointer = data_buffs[parent_rank].ctl_struct;
		parent_lmsg_ctl_pointer =
				(mca_bcol_basesmuma_portal_buf_addr_t*)data_buffs[parent_rank].payload;

		ready_flag = ready_flag + pow_2_levels;

		while(!IS_SG_DATA_READY(parent_ctl_pointer, ready_flag, sequence_number)) {

				opal_progress();

        }


		mca_bcol_basesmuma_portals_get_msg_fragment_no_eq_h(cs, my_lmsg_ctl_pointer,
								parent_lmsg_ctl_pointer, 0,
								0, fragment_size);

		my_ctl_pointer->flag = ready_flag;

		goto Release;
    }

Scatter:

    /* I am not root of bcast compute the list of possible
	 * where I will receive bcast data from.
	 */
    src_list = (int *) malloc(sizeof(int) * (pow_2_levels + 1));
    for( i = 0; i < pow_2_levels; i++) {
        src_list[i] = my_rank ^ (1<<i);
    }

	/* My source might be process > pow_2 */
	if ((my_rank + pow_2) < group_size) {
            src_list[i] = my_rank + pow_2;
    } else {
            src_list[i] = -1;
    }

Probe:

    /* If I am not the root, then poll on possible "senders'" control structs */
	/* For portals we block for now */
	while (!matched) {
      /* Shared memory iprobe */
      SG_LARGE_MSG_PROBE(src_list, pow_2_levels + 1,
                src_list_index, matched, src, data_buffs, parent_ctl_pointer,
				parent_lmsg_ctl_pointer,ready_flag, sequence_number);
    }

	/* If I am a secondary root
	 * Secondary root acts as root of bcast data when real root of data
	 * is process with group rank greater than pow_2 */
	if ((matched) && (src == pow_2 + my_rank)) {
		volatile mca_bcol_basesmuma_header_t *extra_src_ctl_pointer = NULL;
		struct mca_bcol_basesmuma_portal_buf_addr_t *extra_src_lmsg_ctl_pointer = NULL;

		secondary_root = true;
        BASESMUMA_VERBOSE(10,("I am the secondary root for the data"));
        my_lmsg_ctl_pointer->offset = 0;
        my_lmsg_ctl_pointer->n_sends = pow_2_levels;
        my_lmsg_ctl_pointer->length = fragment_size;

		extra_src_ctl_pointer = data_buffs[src].ctl_struct;
		extra_src_lmsg_ctl_pointer = (mca_bcol_basesmuma_portal_buf_addr_t*)data_buffs[src].payload;

		/* create an event queue for the incoming buffer */
	rc = PtlEQAlloc(((mca_bcol_basesmuma_portal_proc_info_t*)
				cs->portals_info)->ni_h, MAX_PORTAL_EVENTS_IN_Q,
						PTL_EQ_HANDLER_NONE, &allgather_eq_h);

		if (rc != PTL_OK) {
		BASESMUMA_VERBOSE(10,( "PtlEQAlloc() failed: %d \n",rc));
			goto Release;
	}

		mca_bcol_basesmuma_portals_get_msg_fragment_no_eq_h(cs, my_lmsg_ctl_pointer,
								extra_src_lmsg_ctl_pointer, 0,
								0, fragment_size);


		extra_src_posts = 0;
		scatter_posts = my_lmsg_ctl_pointer->n_sends;
		allgather_posts = pow_2_levels - 1;

		total_msg_posts = scatter_posts + allgather_posts + extra_src_posts ;

		mca_bcol_basesmuma_portals_post_msg(cs, my_lmsg_ctl_pointer,
						   my_userbuf, fragment_size, allgather_eq_h,
						   total_msg_posts,
						   blocked_post,
						  PTL_MD_EVENT_START_DISABLE| PTL_MD_EVENT_END_DISABLE | PTL_MD_OP_GET | PTL_MD_MANAGE_REMOTE | PTL_MD_TRUNCATE | PTL_MD_EVENT_AUTO_UNLINK_ENABLE);
		msg_posted = true ;
        /* important that these be set before my children
         * see the ready flag raised
         */
        opal_atomic_wmb ();
        my_ctl_pointer->flag = ready_flag;

		wait_for_peers(my_rank, my_lmsg_ctl_pointer->n_sends, data_buffs,
							ready_flag, sequence_number);
		goto Allgather;
    }

    /* Verify whether we got the right
	 * source of the data, by computing the source's intended
	 * destinations
     */
    for( i = 0; i < parent_lmsg_ctl_pointer->n_sends; i++) {
		uint64_t local_offset = 0;
		uint64_t remote_offset = 0;

		BASESMUMA_VERBOSE(5,("%d found it from %d \n",my_rank,src));

       if( my_rank == (src^(1<<i))) {
            parent_ctl_pointer = data_buffs[src].ctl_struct;
            parent_lmsg_ctl_pointer = (mca_bcol_basesmuma_portal_buf_addr_t*)data_buffs[src].payload;

            /* we found our root within the group ... */
            BASESMUMA_VERBOSE(10,("Shared memory probe was matched, the root is %d", src));

			my_lmsg_ctl_pointer->n_sends = i;

		    /* Am I source for other process during scatter phase */
            if ( i > 0) {

                /* compute the size of the chunk to copy */
                length = (parent_lmsg_ctl_pointer->length)/
                    (1<<(parent_lmsg_ctl_pointer->n_sends - my_lmsg_ctl_pointer->n_sends));
                my_lmsg_ctl_pointer->length = length;
                my_lmsg_ctl_pointer->offset =
				parent_lmsg_ctl_pointer->offset + length;


				local_offset = my_lmsg_ctl_pointer->offset;
				remote_offset = parent_lmsg_ctl_pointer->offset + length;

				mca_bcol_basesmuma_portals_get_msg_fragment_no_eq_h(cs, my_lmsg_ctl_pointer,
								parent_lmsg_ctl_pointer,local_offset,
								remote_offset, length);
				rc = PtlEQAlloc(((mca_bcol_basesmuma_portal_proc_info_t*)
									cs->portals_info)->ni_h, MAX_PORTAL_EVENTS_IN_Q,
													PTL_EQ_HANDLER_NONE,
													&allgather_eq_h);

				if (rc != PTL_OK) {
				BASESMUMA_VERBOSE(10,( "PtlEQAlloc() failed: %d \n",rc));
					goto Release;
			}

				/* Now post the message for other children to read */
				extra_src_posts = (my_rank + pow_2 < group_size ) ? 1: 0;
				scatter_posts = my_lmsg_ctl_pointer->n_sends;
				allgather_posts = pow_2_levels - 1;

				total_msg_posts = scatter_posts + allgather_posts + extra_src_posts ;


				mca_bcol_basesmuma_portals_post_msg(cs, my_lmsg_ctl_pointer,
						   my_userbuf, my_lmsg_ctl_pointer->userbuf_length,
						   allgather_eq_h,
						   total_msg_posts,
						   blocked_post,
						   PTL_MD_EVENT_START_DISABLE| PTL_MD_EVENT_END_DISABLE
						   | PTL_MD_OP_GET | PTL_MD_MANAGE_REMOTE | PTL_MD_TRUNCATE | PTL_MD_EVENT_AUTO_UNLINK_ENABLE
					  );
				msg_posted = true;
				/* set the memory barrier to ensure completion
				 * and signal I am done getting scatter data*/
			opal_atomic_wmb ();
			my_ctl_pointer->flag = ready_flag;

				wait_for_peers(my_rank, my_lmsg_ctl_pointer->n_sends, data_buffs,
							ready_flag, sequence_number);

            } else {
                /* takes care of first level recurssive double */
		length = parent_lmsg_ctl_pointer->length/
                    (1<<(parent_lmsg_ctl_pointer->n_sends - 1));
                my_lmsg_ctl_pointer->length = length;
                my_lmsg_ctl_pointer->offset = parent_lmsg_ctl_pointer->offset;

				local_offset = my_lmsg_ctl_pointer->offset;
				remote_offset = my_lmsg_ctl_pointer->offset;


				while(!IS_SG_DATA_READY(parent_ctl_pointer, ready_flag, sequence_number)) {
			opal_progress();
			}

				mca_bcol_basesmuma_portals_get_msg_fragment_no_eq_h(cs, my_lmsg_ctl_pointer,
								parent_lmsg_ctl_pointer,local_offset,
								remote_offset, length);

				/* signal that I am done reading data from parent */
		    opal_atomic_wmb ();
	        my_ctl_pointer->flag = ready_flag;
			}

            /* time for allgather phase */
             input_args->status = ALLGATHER;

            BASESMUMA_VERBOSE(5,("Completed %d found it from %d \n",my_rank,src));

			while(ready_flag > parent_ctl_pointer->flag);

			goto Allgather;
        }
	}

	{
	/* this is not who we are looking for,
	 * mark as false positive so we don't
	 * poll here again
	 */
		src_list[src_list_index] = -1;
	matched = 0;
	goto Probe;
     }

Allgather:

	BASESMUMA_VERBOSE(5,(" %d Completed Scatter %d times \n", my_rank, completed_scatter));

    /* zip it back up - we have already taken care of first level */
    global_sg_offset = my_lmsg_ctl_pointer->offset;

	/* first level of zip up */
    length = 2 * fragment_size/pow_2;


	if (!msg_posted) {
		rc = PtlEQAlloc(((mca_bcol_basesmuma_portal_proc_info_t*)
				cs->portals_info)->ni_h, MAX_PORTAL_EVENTS_IN_Q,
						PTL_EQ_HANDLER_NONE, &allgather_eq_h);

		/* Posting for all phases of recursive doubling */
		extra_src_posts = (my_rank + pow_2 < group_size ) ? 1: 0;
		allgather_posts = pow_2_levels - 1;
		total_msg_posts = allgather_posts + extra_src_posts ;


		mca_bcol_basesmuma_portals_post_msg(cs, my_lmsg_ctl_pointer,
						   my_userbuf, my_lmsg_ctl_pointer->userbuf_length,
						   allgather_eq_h, total_msg_posts , blocked_post,
						   PTL_MD_EVENT_START_DISABLE| PTL_MD_EVENT_END_DISABLE
						   | PTL_MD_OP_GET | PTL_MD_MANAGE_REMOTE | PTL_MD_TRUNCATE | PTL_MD_EVENT_AUTO_UNLINK_ENABLE
							);
		msg_posted = true;
	}


	ready_flag++;
    opal_atomic_wmb ();
    my_ctl_pointer->flag = ready_flag;

	/*
	 * Recursive doubling allgather implementation
	 */
    for( i = 1; i < pow_2_levels; i++) {
        /* get my partner for this level */
        partner = my_rank^(1<<i);
        partner_ctl_pointer = data_buffs[partner].ctl_struct;
        partner_lmsg_ctl_pointer = (mca_bcol_basesmuma_portal_buf_addr_t*)
											data_buffs[partner].payload;


		/* Block until partner is at this level of recursive-doubling stage */
        while(!IS_SG_DATA_READY(partner_ctl_pointer, ready_flag, sequence_number)) {
            opal_progress();
        }
        assert(partner_ctl_pointer->flag >= ready_flag);

		if (partner_lmsg_ctl_pointer->offset < my_lmsg_ctl_pointer->offset) {
			global_sg_offset -= length;
			local_sg_offset = global_sg_offset;
		} else {
			local_sg_offset = global_sg_offset + length;
		}


		BASESMUMA_VERBOSE(10,("Allgather Phase: Get message from process %d, length %d", partner, length));
		mca_bcol_basesmuma_portals_get_msg_fragment_no_eq_h(cs, my_lmsg_ctl_pointer,
								partner_lmsg_ctl_pointer,local_sg_offset,
								local_sg_offset, length);

		ready_flag++;
		opal_atomic_wmb ();
	my_ctl_pointer->flag = ready_flag;

		/* Block until partner completed this level of recursive-doubling stage */
	while(!IS_SG_DATA_READY(partner_ctl_pointer, ready_flag, sequence_number)) {
		    opal_progress();
        }

        /*
		 * Compute length for next recursive doubling phase
		 */
        length *= 2;
    }


	/* If I am source for non-power 2 children wait for them */
	/* If I am secondary root then my partner would be real root
	 * so no need for exchange of data with the extra partner */
	extra_partner = my_rank + pow_2 ;
	if ((extra_partner < group_size) && (!secondary_root)) {
		volatile mca_bcol_basesmuma_header_t *extra_partner_ctl_pointer = NULL;

		extra_partner_ctl_pointer = data_buffs[extra_partner].ctl_struct;
		/* Block until extra partner has copied data */
	while(!IS_SG_DATA_READY(extra_partner_ctl_pointer, ready_flag, sequence_number)) {
		    opal_progress();
        }

	}

Release:

	/* free the event queue */
	rc = PtlEQFree(allgather_eq_h);
	if (rc != PTL_OK) {
		BASESMUMA_VERBOSE(10,("PtlEQFree() failed: %d )\n",rc));
	}

    my_ctl_pointer->starting_flag_value++;
    input_args->status = FINISHED;

    return BCOL_FN_COMPLETE;

}
#endif

/*
 * Blocking Portals Scatter Allgather
 *
 *
 *
 *
 *
 */

int bcol_basesmuma_lmsg_scatter_allgather_portals_bcast(bcol_function_args_t *input_args,
    mca_bcol_base_function_t *c_input_args)
{

    /* local variables */
    int i;
    uint64_t length;
    int my_rank, parent_rank, src =-1, matched = 0;
    int *src_list = NULL;
    int group_size = -1, dummy_group_size;
    int first_instance=0;
    int rc = OMPI_SUCCESS;
    int leading_dim, buff_idx, idx;
    int count=input_args->count;
    size_t pack_len = 0, dt_size =0 ;
	volatile int8_t ready_flag;
    int flag_offset;
    int pow_2, pow_2_levels;
    int src_list_index = -1;
    uint64_t fragment_size;  /* user buffer size */
	int sg_matchbits;

	/* Input argument variables */
	void *my_userbuf = (void*)((unsigned char*)input_args->userbuf);
	int64_t sequence_number=input_args->sequence_num;
    struct ompi_datatype_t* dtype=input_args->dtype;

	/* Extra source variables */
	bool secondary_root = false;
	int partner = -1, extra_partner = -1;

	/* Scatter Allgather offsets */
	uint64_t local_sg_offset = 0, global_sg_offset = 0, partner_offset = 0;

	/* Portals messaging relevant variables */
	mca_bcol_basesmuma_portal_proc_info_t *portals_info;
	ptl_handle_eq_t allgather_eq_h;
	ptl_event_t  allgather_event;
	bool blocked_post = false;
	bool msg_posted = false;
	int total_msg_posts = -1, scatter_posts = -1, allgather_posts = -1, extra_src_posts = -1;

	/* OMPI module and component variables */
    mca_bcol_basesmuma_component_t *cs = &mca_bcol_basesmuma_component;
    mca_bcol_basesmuma_module_t *bcol_module =
        (mca_bcol_basesmuma_module_t *) c_input_args->bcol_module;

	/* Control structure and payload variables */
	volatile mca_bcol_basesmuma_ctl_struct_t **ctl_structs;
    volatile mca_bcol_basesmuma_ctl_struct_t  *my_ctl_pointer = NULL;
	volatile mca_bcol_basesmuma_ctl_struct_t  *parent_ctl_pointer = NULL; /* binomial fanout */
    volatile mca_bcol_basesmuma_ctl_struct_t  *partner_ctl_pointer = NULL; /* recursive double */

	/* Make sure there userbuffer is not null */
	assert(my_userbuf != NULL);

    /* Get portals info*/
	portals_info = (mca_bcol_basesmuma_portal_proc_info_t*)cs->portals_info;

	/* Get addresing information */
    buff_idx = input_args->src_desc->buffer_index;
    group_size = bcol_module->colls_no_user_data.size_of_group;
    leading_dim=bcol_module->colls_no_user_data.size_of_group;
    my_rank = bcol_module->super.sbgp_partner_module->my_index;
    idx=SM_ARRAY_INDEX(leading_dim,buff_idx,0);

    /* calculate the largest power of two that is smaller than
     * or equal to the group size
     */
    pow_2_levels = pow_sm_k(2,group_size, &(dummy_group_size));
    if( group_size < (1<<pow_2_levels)) {
        pow_2_levels--;
    }
    /* power-of-two group size */
    pow_2 = 1<<pow_2_levels;


     /* we will work only on packed data - so compute the length*/
    ompi_datatype_type_size(dtype, &dt_size);
    fragment_size = count*dt_size;

    /* grab the ctl buffs */
    ctl_structs = (volatile mca_bcol_basesmuma_ctl_struct_t **)
        bcol_module->colls_with_user_data.ctl_buffs+idx;


    my_ctl_pointer = ctl_structs[my_rank];
    if(my_ctl_pointer->sequence_number < sequence_number) {
        first_instance = 1;
    }

    if(first_instance) {
        for( i = 0; i < NUM_SIGNAL_FLAGS; i++){
            my_ctl_pointer->flags[i] = -1;
        }
        my_ctl_pointer->index = 1;

        my_ctl_pointer->starting_flag_value = 0;
        flag_offset = 0;

    } else {
        my_ctl_pointer->index++;
    }


    /* increment the starting flag by one and return */
    flag_offset = my_ctl_pointer->starting_flag_value;
    /*ready_flag = flag_offset + sequence_number + 1;*/
    ready_flag = flag_offset + 1;

    my_ctl_pointer->sequence_number = sequence_number;
	sg_matchbits = sequence_number ;

	/* Construct my portal buffer address and copy to payload buffer */
	mca_bcol_basesmuma_construct_portal_address(&my_ctl_pointer->portals_buf_addr,
						portals_info->portal_id.nid,
						portals_info->portal_id.pid,
						sg_matchbits,
						bcol_module->super.sbgp_partner_module->group_comm->c_contextid);

	my_ctl_pointer->portals_buf_addr.userbuf = my_userbuf;
	my_ctl_pointer->portals_buf_addr.userbuf_length = fragment_size;


    if (input_args->root_flag) {
        my_ctl_pointer->offset = 0;
        my_ctl_pointer->n_sends = pow_2_levels;
        my_ctl_pointer->length = fragment_size;

	rc = PtlEQAlloc(((mca_bcol_basesmuma_portal_proc_info_t*)
				cs->portals_info)->ni_h, MAX_PORTAL_EVENTS_IN_Q,
						PTL_EQ_HANDLER_NONE, &allgather_eq_h);

		if (rc != PTL_OK) {
		BASESMUMA_VERBOSE(10,( "PtlEQAlloc() failed: %d \n",rc));
			goto Release;
	}

		/* Compute number of posts required */
		if (my_rank >= pow_2) {
			/* I am root and my rank is greater than pow_2, I will hand
			 * over to rank (that is < pow_2) to act as secondary root
			 */
			total_msg_posts  = 1;
		}
		else {

			extra_src_posts = (my_rank + pow_2 < group_size ) ? 1: 0;
			scatter_posts = my_ctl_pointer->n_sends;
			allgather_posts = pow_2_levels - 1;

			total_msg_posts = scatter_posts + allgather_posts + extra_src_posts ;
		}

		 mca_bcol_basesmuma_portals_post_msg(cs,
						 &my_ctl_pointer->portals_buf_addr,
						   my_userbuf, fragment_size, allgather_eq_h,
						   total_msg_posts,
						   blocked_post,
						  PTL_MD_EVENT_START_DISABLE| PTL_MD_EVENT_END_DISABLE | PTL_MD_OP_GET | PTL_MD_MANAGE_REMOTE | PTL_MD_TRUNCATE | PTL_MD_EVENT_AUTO_UNLINK_ENABLE);
		 msg_posted = true ;

		 /* important that these be set before my children
         * see the ready flag raised
         */
        opal_atomic_wmb ();
        my_ctl_pointer->flags[BCAST_FLAGS] = ready_flag;
        BASESMUMA_VERBOSE(1,("I am the root(ctl_pointer %x) of the data flag value %d",my_ctl_pointer, my_ctl_pointer->flag));
		/* Wait for my scatter partner */
		if (my_rank >= pow_2) {
			int scatter_partner = -1;
			volatile mca_bcol_basesmuma_ctl_struct_t *scatter_partner_ctl_pointer = NULL;

			scatter_partner = my_rank - pow_2;
			scatter_partner_ctl_pointer =
					ctl_structs[scatter_partner];

			while(!IS_SG_DATA_READY(scatter_partner_ctl_pointer, ready_flag,
									sequence_number)){
SCATTER_WAIT_FOR_EXTRA:
					opal_progress();
			}

			goto Release;
		}
		else {

			wait_for_peers_nb(my_rank, my_ctl_pointer->n_sends, ctl_structs,
							ready_flag, sequence_number);
		}

		goto Allgather;
    }


Extra :
    if( my_rank >= pow_2 ) {
        parent_rank = my_rank & (pow_2-1);
        parent_ctl_pointer = ctl_structs[parent_rank];

		ready_flag = ready_flag + pow_2_levels;

		while(!IS_SG_DATA_READY(parent_ctl_pointer, ready_flag, sequence_number)) {

				opal_progress();

        }


		mca_bcol_basesmuma_portals_get_msg_fragment_no_eq_h(cs, &my_ctl_pointer->portals_buf_addr,
								&parent_ctl_pointer->portals_buf_addr, 0,
								0, fragment_size);

		my_ctl_pointer->flags[BCAST_FLAG] = ready_flag;

		goto Release;
    }

Scatter:

    /* compute the list of possible sources */
    src_list = (int *) malloc(sizeof(int) * (pow_2_levels + 1));
    for( i = 0; i < pow_2_levels; i++) {
        src_list[i] = my_rank ^ (1<<i);
    }

	/* My source might be process > pow_2 */
	if ((my_rank + pow_2) < group_size) {
            src_list[i] = my_rank + pow_2;
    } else {
            src_list[i] = -1;
    }

Probe:

    /* If I am not the root, then poll on possible "senders'" control structs */
	/* For portals we block for now */
	while (!matched) {
      /* Shared memory iprobe */
      SG_LARGE_MSG_NB_PROBE(src_list, pow_2_levels + 1,
                src_list_index, matched, src, ctl_structs,
				parent_ctl_pointer, ready_flag, sequence_number);
	}

	BASESMUMA_VERBOSE(1,("Scatter : Im non-root match received"));
	/* If I am a secondary root */
	if ((matched) && (src == pow_2 + my_rank)) {
		volatile mca_bcol_basesmuma_ctl_struct_t *extra_src_ctl_pointer = NULL;

		secondary_root = true;
        BASESMUMA_VERBOSE(10,("I am the secondary root for the data"));
        my_ctl_pointer->offset = 0;
        my_ctl_pointer->n_sends = pow_2_levels;
        my_ctl_pointer->length = fragment_size;

		extra_src_ctl_pointer = ctl_structs[src];

		/* create an event queue for the incoming buffer */
	rc = PtlEQAlloc(((mca_bcol_basesmuma_portal_proc_info_t*)
				cs->portals_info)->ni_h, MAX_PORTAL_EVENTS_IN_Q,
						PTL_EQ_HANDLER_NONE, &allgather_eq_h);

		if (rc != PTL_OK) {
		BASESMUMA_VERBOSE(10,( "PtlEQAlloc() failed: %d \n",rc));
			goto Release;
	}

		mca_bcol_basesmuma_portals_get_msg_fragment_no_eq_h(cs,
						&my_ctl_pointer->portals_buf_addr,
						&extra_src_ctl_pointer->portals_buf_addr, 0,
						0, fragment_size);


		extra_src_posts = 0;
		scatter_posts = my_ctl_pointer->n_sends;
		allgather_posts = pow_2_levels - 1;

		total_msg_posts = scatter_posts + allgather_posts + extra_src_posts ;

		mca_bcol_basesmuma_portals_post_msg(cs,
						  &my_ctl_pointer->portals_buf_addr,
						   my_userbuf, fragment_size, allgather_eq_h,
						   total_msg_posts,
						   blocked_post,
						   PTL_MD_EVENT_START_DISABLE| PTL_MD_EVENT_END_DISABLE | PTL_MD_OP_GET
						   | PTL_MD_MANAGE_REMOTE | PTL_MD_TRUNCATE | PTL_MD_EVENT_AUTO_UNLINK_ENABLE);
		msg_posted = true ;

		/* important that these be set before my children
         * see the ready flag raised
         */
        opal_atomic_wmb ();
        my_ctl_pointer->flags[BCAST_FLAG] = ready_flag;

		wait_for_peers_nb(my_rank, my_ctl_pointer->n_sends, ctl_structs,
							ready_flag, sequence_number);
		goto Allgather;
    }

    /* we need to see whether this is really
     * who we are looking for
     */
    for( i = 0; i < parent_ctl_pointer->n_sends; i++) {
		uint64_t local_offset = 0;
		uint64_t remote_offset = 0;

		BASESMUMA_VERBOSE(1,("%d found it from %d \n",my_rank,src));

       if( my_rank == (src^(1<<i))) {
            parent_ctl_pointer = ctl_structs[src];

            /* we found our root within the group ... */
            BASESMUMA_VERBOSE(10,("Shared memory probe was matched, the root is %d", src));

			my_ctl_pointer->n_sends = i;

		    /* Am I source for other process during scatter phase */
            if ( i > 0) {

                /* compute the size of the chunk to copy */
                length = (parent_ctl_pointer->length)/
                    (1<<(parent_ctl_pointer->n_sends - my_ctl_pointer->n_sends));
                my_ctl_pointer->length = length;
                my_ctl_pointer->offset =
				parent_ctl_pointer->offset + length;


				local_offset = my_ctl_pointer->offset;
				remote_offset = parent_ctl_pointer->offset + length;

				mca_bcol_basesmuma_portals_get_msg_fragment_no_eq_h(cs, &my_ctl_pointer->portals_buf_addr,
								&parent_ctl_pointer->portals_buf_addr,local_offset,
								remote_offset, length);
				rc = PtlEQAlloc(((mca_bcol_basesmuma_portal_proc_info_t*)
									cs->portals_info)->ni_h, MAX_PORTAL_EVENTS_IN_Q,
													PTL_EQ_HANDLER_NONE,
													&allgather_eq_h);

				if (rc != PTL_OK) {
				BASESMUMA_VERBOSE(10,( "PtlEQAlloc() failed: %d \n",rc));
					goto Release;
			}

				/* Now post the message for other children to read */
				extra_src_posts = (my_rank + pow_2 < group_size ) ? 1: 0;
				scatter_posts = my_ctl_pointer->n_sends;
				allgather_posts = pow_2_levels - 1;

				total_msg_posts = scatter_posts + allgather_posts + extra_src_posts ;


				mca_bcol_basesmuma_portals_post_msg(cs, &my_ctl_pointer->portals_buf_addr,
						   my_userbuf, my_ctl_pointer->portals_buf_addr.userbuf_length,
						   allgather_eq_h,
						   total_msg_posts,
						   blocked_post,
						   PTL_MD_EVENT_START_DISABLE| PTL_MD_EVENT_END_DISABLE
						   | PTL_MD_OP_GET | PTL_MD_MANAGE_REMOTE | PTL_MD_TRUNCATE | PTL_MD_EVENT_AUTO_UNLINK_ENABLE
					  );
				msg_posted = true;
				/* set the memory barrier to ensure completion */
			opal_atomic_wmb ();
		/* signal that I am done */
			my_ctl_pointer->flags[BCAST_FLAG] = ready_flag;

				wait_for_peers_nb(my_rank, my_ctl_pointer->n_sends, ctl_structs,
							ready_flag, sequence_number);

            } else {
                /* takes care of first level recurssive double */
		length = parent_ctl_pointer->length/
                    (1<<(parent_ctl_pointer->n_sends - 1));
                my_ctl_pointer->length = length;
                my_ctl_pointer->offset = parent_ctl_pointer->offset;

				local_offset = my_ctl_pointer->offset;
				remote_offset = my_ctl_pointer->offset;


				while(!IS_SG_DATA_READY(parent_ctl_pointer, ready_flag, sequence_number)) {
			opal_progress();
			}

				mca_bcol_basesmuma_portals_get_msg_fragment_no_eq_h(cs,
								&my_ctl_pointer->portals_buf_addr,
								&parent_ctl_pointer->portals_buf_addr, local_offset,
								remote_offset, length);

				/* signal that I am done reading data from parent */
		    opal_atomic_wmb ();
	        my_ctl_pointer->flags[BCAST_FLAG] = ready_flag;
			}

            /* time for allgather phase */
             input_args->status = ALLGATHER;

            BASESMUMA_VERBOSE(5,("Completed %d found it from %d \n",my_rank,src));

			while(ready_flag > parent_ctl_pointer->flags[BCAST_FLAG]);

			goto Allgather;
        }
	}

	{
	/* this is not who we are looking for,
	 * mark as false positive so we don't
	 * poll here again
	 */
		src_list[src_list_index] = -1;
	matched = 0;
	goto Probe;
     }

Allgather:

	BASESMUMA_VERBOSE(5,(" %d Completed Scatter %d times \n", my_rank, completed_scatter));

    /* zip it back up - we have already taken care of first level */
    global_sg_offset = my_ctl_pointer->offset;

	/* first level of zip up */
    length = 2 * fragment_size/pow_2;


	if (!msg_posted) {
		rc = PtlEQAlloc(((mca_bcol_basesmuma_portal_proc_info_t*)
				cs->portals_info)->ni_h, MAX_PORTAL_EVENTS_IN_Q,
						PTL_EQ_HANDLER_NONE, &allgather_eq_h);

		/* Posting for all phases of recursive doubling */
		extra_src_posts = (my_rank + pow_2 < group_size ) ? 1: 0;
		allgather_posts = pow_2_levels - 1;
		total_msg_posts = allgather_posts + extra_src_posts ;


		mca_bcol_basesmuma_portals_post_msg(cs, &my_ctl_pointer->portals_buf_addr,
						   my_userbuf, my_ctl_pointer->portals_buf_addr.userbuf_length,
						   allgather_eq_h, total_msg_posts , blocked_post,
						   PTL_MD_EVENT_START_DISABLE| PTL_MD_EVENT_END_DISABLE
						   | PTL_MD_OP_GET | PTL_MD_MANAGE_REMOTE | PTL_MD_TRUNCATE | PTL_MD_EVENT_AUTO_UNLINK_ENABLE
							);
		msg_posted = true;
	}

	ready_flag++;
    opal_atomic_wmb ();
    my_ctl_pointer->flags[BCAST_FLAG] = ready_flag;

    for( i = 1; i < pow_2_levels; i++) {
        /* get my partner for this level */
        partner = my_rank^(1<<i);
        partner_ctl_pointer =ctl_structs[partner];


		/* Block until partner is at this level of recursive-doubling stage */
        while(!IS_SG_DATA_READY(partner_ctl_pointer, ready_flag, sequence_number)) {
            opal_progress();
        }
        assert(partner_ctl_pointer->flags[BCAST_FLAG] >= ready_flag);

		if (partner_ctl_pointer->offset < my_ctl_pointer->offset) {
			global_sg_offset -= length;
			local_sg_offset = global_sg_offset;
		} else {
			local_sg_offset = global_sg_offset + length;
		}


		BASESMUMA_VERBOSE(10,("Allgather Phase: Get message from process %d, length %d", partner, length));
		mca_bcol_basesmuma_portals_get_msg_fragment_no_eq_h(cs,
								&my_ctl_pointer->portals_buf_addr,
								&partner_ctl_pointer->portals_buf_addr,local_sg_offset,
								local_sg_offset, length);

		ready_flag++;
		opal_atomic_wmb ();
	my_ctl_pointer->flags[BCAST_FLAG] = ready_flag;

		/* Block until partner is at this level of recursive-doubling stage */
	while(!IS_SG_DATA_READY(partner_ctl_pointer, ready_flag, sequence_number)) {
		    opal_progress();
        }

        /* double the length */
        length *= 2;
    }


	/* If I am source for non-power 2 children wait for them */
	/* If I am secondary root then my partner would be real root
	 * so no need for exchange of data with the extra partner */
	extra_partner = my_rank + pow_2 ;
	if ((extra_partner < group_size) && (!secondary_root)) {
		volatile mca_bcol_basesmuma_ctl_struct_t *extra_partner_ctl_pointer = NULL;

		extra_partner_ctl_pointer = ctl_structs[extra_partner];
		/* Block until extra partner has copied data */
	while(!IS_SG_DATA_READY(extra_partner_ctl_pointer, ready_flag, sequence_number)) {
		    opal_progress();
        }

	}

Release:

	/* free the event queue */
	rc = PtlEQFree(allgather_eq_h);
	if (rc != PTL_OK) {
		BASESMUMA_VERBOSE(10,("PtlEQFree() failed: %d )\n",rc));
	}

    my_ctl_pointer->starting_flag_value++;
    input_args->status = FINISHED;

    return BCOL_FN_COMPLETE;

}


/*
 * static sg_state_t *sg_state = NULL;
 */

int bcol_basesmuma_lmsg_scatter_allgather_portals_nb_bcast(bcol_function_args_t *input_args,
    mca_bcol_base_function_t *c_input_args)
{
	int i;
	mca_bcol_basesmuma_portal_proc_info_t *portals_info;
	int dummy_group_size;
    int rc = OMPI_SUCCESS;
	int buff_idx;
	int count=input_args->count;
    size_t pack_len = 0, dt_size =0 ;
    struct ompi_datatype_t* dtype=input_args->dtype;
	int completed_posts = 0;
	sg_state_t *sg_state = NULL;
	mca_bcol_basesmuma_module_t *bcol_module = NULL;
	int extra_src_posts = -1,allgather_posts = -1, total_msg_posts = -1;

    bcol_module = (mca_bcol_basesmuma_module_t *) c_input_args->bcol_module;
	/*
	sg_state = (sg_state_t*)bcol_module->sg_state;
     */
	sg_state = (sg_state_t*)&(bcol_module->sg_state);
	/* Re-entering the algorithm */
   switch (sg_state->phase) {
	case PROBE:
		 if (input_args->root_flag) {
			/* I became a root for this group */
			sg_state->phase = START;
			goto Start;
		 }
		 goto Probe;
		 break;

	case SCATTER_ROOT_WAIT:
		 goto Scatter_root_wait;

	case SCATTER_EXTRA_ROOT_WAIT:
		 goto Scatter_extra_root_wait;

	case SCATTER_PARENT_WAIT:
		 goto Scatter_parent_wait;

	default:
		break;
   }

	sg_state->phase = INIT;

	BASESMUMA_VERBOSE(1,("Im entering portals_nb_bcast Unknown root "));
	/* Allocate space for algorithm state */
	/*
	sg_state = (sg_state_t *) malloc(sizeof(sg_state_t));
	bcol_module->sg_state = (void *)sg_state;

	assert(NULL != sg_state);
	*/

	sg_state->secondary_root = false;
	sg_state->msg_posted = false;
	sg_state->matched = 0;
	sg_state->phase = SCATTER;
	/* Copy input args to local variables */
	sg_state->my_userbuf = (void*)((unsigned char*)input_args->userbuf);
	assert(sg_state->my_userbuf != NULL);
	sg_state->sequence_number=input_args->sequence_num;
	sg_state->cs = &mca_bcol_basesmuma_component;
    sg_state->bcol_module = (mca_bcol_basesmuma_module_t *) c_input_args->bcol_module;
	/* Should this be buffer index (ML) or control buffer index ? */
    buff_idx = input_args->src_desc->buffer_index;

	/* Initialize SM group info used for control signaling */
	init_sm_group_info(sg_state, buff_idx);

    /* calculate the largest power of two that is smaller than
     * or equal to the group size
     */
    sg_state->pow_2_levels = pow_sm_k(2, sg_state->group_size, &(dummy_group_size));
    if( sg_state->group_size < (1 << sg_state->pow_2_levels)) {
        sg_state->pow_2_levels--;
    }
    /* power-of-two group size */
    sg_state->pow_2 = 1 << sg_state->pow_2_levels;


    /* we will work only on packed data - so compute the length*/
    ompi_datatype_type_size(dtype, &dt_size);
    sg_state->fragment_size = count*dt_size;


	/* Init portals scatter allgather info */
	rc = init_sm_portals_sg_info(sg_state);

	if (rc != OMPI_SUCCESS) {
		goto Release;
	}

Start :
Extra :
    /*
	 *  My rank >  pow2 groupsize
	 */
    if( sg_state->my_rank >= sg_state->pow_2 ) {

		if (input_args->root_flag){

			rc = sm_portals_extra_root_scatter(sg_state);
			if (rc != OMPI_SUCCESS) {
				goto Release;
			}

		} else {
			/*
			 * Wait for my partner to receive bcast data, and copy from it
			 */
			int extra_parent_rank;
			volatile mca_bcol_basesmuma_ctl_struct_t  *extra_parent_ctl_pointer = NULL; /* binomial fanout */
		extra_parent_rank = sg_state->my_rank & (sg_state->pow_2-1);
		extra_parent_ctl_pointer = sg_state->ctl_structs[extra_parent_rank];

			sg_state->ready_flag = sg_state->ready_flag + sg_state->pow_2_levels;

			while(!IS_SG_DATA_READY(extra_parent_ctl_pointer, sg_state->ready_flag,
								sg_state->sequence_number)) {
				opal_progress();

		}

			mca_bcol_basesmuma_portals_get_msg_fragment(sg_state->cs,
								sg_state->read_eq,
								&sg_state->my_ctl_pointer->portals_buf_addr,
								&extra_parent_ctl_pointer->portals_buf_addr, 0,
								0, sg_state->fragment_size);

			sg_state->my_ctl_pointer->flag = sg_state->ready_flag;
		}

		goto Release;
    }

    if (input_args->root_flag) {

		BASESMUMA_VERBOSE(1,("Scatter : Im root (bcol_module %x,ctl_pointer %x) my ready flag %d \n",
									sg_state->bcol_module, sg_state->my_ctl_pointer, sg_state->ready_flag));
		rc = sm_portals_root_scatter(sg_state);

		/* gvm Fix: Redudant
		opal_atomic_wmb ();
		*/

		sg_state->my_ctl_pointer->flag = sg_state->ready_flag;

		if (rc != OMPI_SUCCESS) {
			goto Release;
		}

Scatter_root_wait:

		BASESMUMA_VERBOSE(5,("Scatter: Im root waiting for children to complete my flag %d",
									sg_state->my_ctl_pointer->flag));

		for( i = 0; i < sg_state->cs->num_to_probe && completed_posts < sg_state->my_ctl_pointer->n_sends;
						i++) {

			completed_posts = wait_for_post_complete_nb(sg_state->my_rank,
								sg_state->my_ctl_pointer->n_sends, sg_state->ctl_structs,
								sg_state->ready_flag, sg_state->sequence_number);

		}

		if (completed_posts < sg_state->my_ctl_pointer->n_sends) {
			sg_state->phase = SCATTER_ROOT_WAIT;
			return  BCOL_FN_STARTED;
		}

		goto Allgather;
    }


Scatter:

	BASESMUMA_VERBOSE(1,("Scatter : Im non-root probing for data "));
    /* compute the list of possible sources */
	/*
    sg_state->src_list = (int *) malloc(sizeof(int) * (sg_state->pow_2_levels + 1));
	*/
	assert(MAX_SM_GROUP_SIZE > sg_state->pow_2_levels+1);

    for( i = 0; i < sg_state->pow_2_levels; i++) {
        sg_state->src_list[i] = sg_state->my_rank ^ (1<<i);
    }

	/* My source might be process > pow_2 */

	if ((sg_state->my_rank + sg_state->pow_2) < sg_state->group_size) {
            sg_state->src_list[i] = sg_state->my_rank + sg_state->pow_2;
    } else {
            sg_state->src_list[i] = -1;
    }


	BASESMUMA_VERBOSE(1,("Scatter : Ready flag %d Im  non-root probing for %d procs %d:%d \n",
					sg_state->ready_flag,sg_state->pow_2_levels,sg_state->src_list[0],sg_state->src_list[1]));
Probe:
    /* If I am not the root, then poll on possible "senders'" control structs */
	/* For portals we block for now */
    /* Shared memory iprobe */


	/*
		SG_LARGE_MSG_NB_PROBE(sg_state->src_list, sg_state->pow_2_levels + 1,
                sg_state->src_list_index, sg_state->matched, sg_state->src,
				sg_state->ctl_structs,
				sg_state->parent_ctl_pointer, sg_state->ready_flag, sg_state->sequence_number);
	 */

	for( i = 0; i < sg_state->cs->num_to_probe && 0 == sg_state->matched;
						i++) {
		sg_large_msg_probe(sg_state);
	}

	if (!sg_state->matched) {
		sg_state->phase = PROBE;
		return BCOL_FN_STARTED;
	}

	BASESMUMA_VERBOSE(1,("Scatter : Im non-root match received"));
	/* If I am a secondary root */
	if ((sg_state->matched) && (sg_state->src == sg_state->pow_2 + sg_state->my_rank)) {

		BASESMUMA_VERBOSE(5,("Scatter : Im secondary root \n"));

		rc = sm_portals_secondary_root_scatter(sg_state);
		if (rc != OMPI_SUCCESS) {
				goto Release;
		}

Scatter_extra_root_wait:

		for( i = 0; i < sg_state->cs->num_to_probe && completed_posts < sg_state->my_ctl_pointer->n_sends;
						i++) {

			completed_posts = wait_for_post_complete_nb(sg_state->my_rank, sg_state->my_ctl_pointer->n_sends,
							sg_state->ctl_structs, sg_state->ready_flag, sg_state->sequence_number);

		}

		if (completed_posts < sg_state->my_ctl_pointer->n_sends) {
			sg_state->phase = SCATTER_EXTRA_ROOT_WAIT;
			return  BCOL_FN_STARTED;
		}

		goto Allgather;
    }

    /* we need to see whether this is really
     * who we are looking for
     */
    for( i = 0; i < sg_state->parent_ctl_pointer->n_sends; i++) {
		uint64_t local_offset = 0;
		uint64_t remote_offset = 0;

		BASESMUMA_VERBOSE(5,("%d found it from %d \n",sg_state->my_rank,sg_state->src));

       if( sg_state->my_rank == (sg_state->src^(1<<i))) {
            sg_state->parent_ctl_pointer = sg_state->ctl_structs[sg_state->src];

            /* we found our root within the group ... */
            BASESMUMA_VERBOSE(5,("Shared memory probe was matched, the root is	%d ",sg_state->src));

			sg_state->my_ctl_pointer->n_sends = i;

		    /* Am I source for other process during scatter phase */
            if ( i > 0) {
				BASESMUMA_VERBOSE(1,("Scatter : Im Internal node \n"));

				rc = sm_portals_internode_scatter(sg_state);

				if (rc != OMPI_SUCCESS) {
					goto Release;
				}

Scatter_parent_wait:

				for( i = 0; i < sg_state->cs->num_to_probe && completed_posts < sg_state->my_ctl_pointer->n_sends;
						i++) {

					completed_posts = wait_for_post_complete_nb(sg_state->my_rank,
								sg_state->my_ctl_pointer->n_sends,
								sg_state->ctl_structs,
								sg_state->ready_flag, sg_state->sequence_number);
				}

				if (completed_posts < sg_state->my_ctl_pointer->n_sends) {
					sg_state->phase = SCATTER_PARENT_WAIT;
					return  BCOL_FN_STARTED;
				}

            } else {

				BASESMUMA_VERBOSE(1,("Scatter : Im leaf node \n"));

				/* takes care of first level recurssive double */
		sg_state->length = sg_state->parent_ctl_pointer->length/
                    (1<<(sg_state->parent_ctl_pointer->n_sends - 1));
                sg_state->my_ctl_pointer->length = sg_state->length;
                sg_state->my_ctl_pointer->offset = sg_state->parent_ctl_pointer->offset;


				while(!IS_SG_DATA_READY(sg_state->parent_ctl_pointer,
										sg_state->ready_flag, sg_state->sequence_number)) {
			opal_progress();
			}

				mca_bcol_basesmuma_portals_get_msg_fragment(sg_state->cs,
								sg_state->read_eq,
								&sg_state->my_ctl_pointer->portals_buf_addr,
								&sg_state->parent_ctl_pointer->portals_buf_addr,
								sg_state->my_ctl_pointer->offset,
								sg_state->my_ctl_pointer->offset, sg_state->length);

				/* signal that I am done reading data from parent */
				/*
		    opal_atomic_wmb ();
				*/
	        sg_state->my_ctl_pointer->flag = sg_state->ready_flag;
			}

            BASESMUMA_VERBOSE(1,("Completed %d found it from %d \n",
									sg_state->my_rank, sg_state->src));

			while(sg_state->ready_flag > sg_state->parent_ctl_pointer->flag);

			goto Allgather;
        }
	}

	{
	/* this is not who we are looking for,
	 * mark as false positive so we don't
	 * poll here again
	 */
		sg_state->src_list[sg_state->src_list_index] = -1;
	sg_state->matched = 0;
	goto Probe;
     }

Allgather:

	BASESMUMA_VERBOSE(5,("Completed Scatter phase"));

    /* zip it back up - we have already taken care of first level */
    sg_state->global_sg_offset = sg_state->my_ctl_pointer->offset;

	/* first level of zip up */
    sg_state->length = 2 * sg_state->fragment_size/sg_state->pow_2;


	/* Posting for all phases of recursive doubling */
	extra_src_posts = (sg_state->my_rank + sg_state->pow_2 < sg_state->group_size ) ? 1: 0;
	allgather_posts = sg_state->pow_2_levels - 1;
	total_msg_posts = allgather_posts + extra_src_posts ;

	if ((!sg_state->msg_posted) && (total_msg_posts > 0)){

			mca_bcol_basesmuma_portals_post_msg(sg_state->cs, &sg_state->my_ctl_pointer->portals_buf_addr,
						   sg_state->my_userbuf, sg_state->my_ctl_pointer->portals_buf_addr.userbuf_length,
						   PTL_EQ_NONE, total_msg_posts, blocked_post,
						   PTL_MD_EVENT_START_DISABLE| PTL_MD_EVENT_END_DISABLE
						   | PTL_MD_OP_GET | PTL_MD_MANAGE_REMOTE | PTL_MD_TRUNCATE | PTL_MD_EVENT_AUTO_UNLINK_ENABLE
							);
			sg_state->msg_posted = true;
	}

	BASESMUMA_VERBOSE(5,("Done with allgather phase"));
	/* I reached an allgather phase */
	sg_state->ready_flag++;
    opal_atomic_wmb ();
    sg_state->my_ctl_pointer->flag = sg_state->ready_flag;

	rc = sm_portals_bcasts_allgather_phase(sg_state);

	if (rc != OMPI_SUCCESS) {
		BASESMUMA_VERBOSE(10,("Error in Bcast's allgather phase "));
		goto Release;
	}

	/* If I am source for non-power 2 children wait for them */
	/* If I am secondary root then my partner would be real root
	 * so no need for exchange of data with the extra partner */
	sg_state->extra_partner = sg_state->my_rank + sg_state->pow_2 ;
	if ((sg_state->extra_partner < sg_state->group_size) && (!sg_state->secondary_root)) {

		sg_state->extra_partner_ctl_pointer = sg_state->ctl_structs[sg_state->extra_partner];
		/* Block until extra partner has copied data */
	while(!IS_SG_DATA_READY(sg_state->extra_partner_ctl_pointer,
								sg_state->ready_flag, sg_state->sequence_number)) {
		    opal_progress();
        }

	}

Release:

    BASESMUMA_VERBOSE(1,("Im done "));

    sg_state->my_ctl_pointer->starting_flag_value++;
    sg_state->phase = FINISHED;


	return BCOL_FN_COMPLETE;

}


int bcol_basesmuma_lmsg_scatter_allgather_portals_nb_knownroot_bcast(bcol_function_args_t *input_args,
    mca_bcol_base_function_t *c_input_args)
{

	int i;
	mca_bcol_basesmuma_portal_proc_info_t *portals_info;
	int dummy_group_size;
    int rc = OMPI_SUCCESS;
	int buff_idx;
	int count=input_args->count;
    size_t pack_len = 0, dt_size =0 ;
    struct ompi_datatype_t* dtype=input_args->dtype;
	int completed_posts = 0;
	sg_state_t *sg_state = NULL;
    mca_bcol_basesmuma_module_t *bcol_module=NULL;
	int extra_src_posts = -1,allgather_posts = -1, total_msg_posts = -1;
    bcol_module = (mca_bcol_basesmuma_module_t *) c_input_args->bcol_module;

	sg_state = (sg_state_t*)(&bcol_module->sg_state);

	BASESMUMA_VERBOSE(1,("Im entering nb_knownroot_bcast bcol = %x ",
							c_input_args->bcol_module));
	/* Re-entering the algorithm */
	switch (sg_state->phase) {
		case PROBE:
			 if (input_args->root_flag) {
				/* I became a root for this group */
				sg_state->phase = START;
				goto Start;
			 }
			 goto Probe;
			 break;

		case SCATTER_ROOT_WAIT:
			 goto Scatter_root_wait;

		case SCATTER_EXTRA_ROOT_WAIT:
			 goto Scatter_extra_root_wait;

		case SCATTER_PARENT_WAIT:
			 goto Scatter_parent_wait;

		default:
			break;
	}

	/* Allocate space for algorithm state */
	/*
	sg_state = (sg_state_t *) malloc(sizeof(sg_state_t));
	bcol_module->sg_state = (void*) sg_state;
	*/

	/* Make sure there userbuffer is not null */

	sg_state->phase = INIT;
	sg_state->secondary_root = false;
	sg_state->msg_posted = false;
	sg_state->matched = 0;
	/* Copy input args to local variables */
	sg_state->my_userbuf = (void*)((unsigned char*)input_args->userbuf);
	assert(sg_state->my_userbuf != NULL);
	sg_state->sequence_number=input_args->sequence_num;
	sg_state->cs = &mca_bcol_basesmuma_component;
	sg_state->bcol_module = bcol_module;
    buff_idx = input_args->src_desc->buffer_index;

	/* Initialize SM group info used for control signaling */
	init_sm_group_info(sg_state, buff_idx);

    /* calculate the largest power of two that is smaller than
     * or equal to the group size
     */
    sg_state->pow_2_levels = pow_sm_k(2, sg_state->group_size, &(dummy_group_size));
    if( sg_state->group_size < (1 << sg_state->pow_2_levels)) {
        sg_state->pow_2_levels--;
    }
    /* power-of-two group size */
    sg_state->pow_2 = 1 << sg_state->pow_2_levels;


     /* we will work only on packed data - so compute the length*/
    ompi_datatype_type_size(dtype, &dt_size);
    sg_state->fragment_size = count*dt_size;


	/* Init portals scatter allgather info */
	rc = init_sm_portals_sg_info(sg_state);

	if (rc != OMPI_SUCCESS) {
		goto Release;
	}
Start:
Extra :
    /*
	 *  My rank >  pow2 groupsize
	 */
    if( sg_state->my_rank >= sg_state->pow_2 ) {

		if (input_args->root_flag){

			rc = sm_portals_extra_root_scatter(sg_state);
			if (rc != OMPI_SUCCESS) {
				goto Release;
			}

		} else {
			/*
			 * Wait for my partner to receive bcast data, and copy from it
			 */
			int extra_parent_rank;
			volatile mca_bcol_basesmuma_ctl_struct_t  *extra_parent_ctl_pointer = NULL; /* binomial fanout */
		extra_parent_rank = sg_state->my_rank & (sg_state->pow_2-1);
		extra_parent_ctl_pointer = sg_state->ctl_structs[extra_parent_rank];

			sg_state->ready_flag = sg_state->ready_flag + sg_state->pow_2_levels;

			while(!IS_SG_DATA_READY(extra_parent_ctl_pointer, sg_state->ready_flag,
								sg_state->sequence_number)) {
				opal_progress();

		}

			mca_bcol_basesmuma_portals_get_msg_fragment(sg_state->cs,
								sg_state->read_eq,
								&sg_state->my_ctl_pointer->portals_buf_addr,
								&extra_parent_ctl_pointer->portals_buf_addr, 0,
								0, sg_state->fragment_size);

			sg_state->my_ctl_pointer->flag = sg_state->ready_flag;
		}

		goto Release;
    }

    if (input_args->root_flag) {

		BASESMUMA_VERBOSE(1,("Scatter : Im root (bcol_module %x,ctl_pointer %x) my ready flag %d \n",
									bcol_module, sg_state->my_ctl_pointer, sg_state->ready_flag));
		rc = sm_portals_root_scatter(sg_state);

		sg_state->my_ctl_pointer->flag = sg_state->ready_flag;

		if (rc != OMPI_SUCCESS) {
			goto Release;
		}

Scatter_root_wait:

		BASESMUMA_VERBOSE(5,("Scatter: Im root waiting for children to complete my flag %d",
									sg_state->my_ctl_pointer->flag));
		for( i = 0; i < sg_state->cs->num_to_probe && completed_posts < sg_state->my_ctl_pointer->n_sends;
						i++) {
			completed_posts = wait_for_post_complete_nb(sg_state->my_rank,
								sg_state->my_ctl_pointer->n_sends, sg_state->ctl_structs,
								sg_state->ready_flag, sg_state->sequence_number);
		}

		if (completed_posts < sg_state->my_ctl_pointer->n_sends) {
			sg_state->phase = SCATTER_ROOT_WAIT;
			return  BCOL_FN_STARTED;
		}

		goto Allgather;
    }


Probe:

	sg_state->src = compute_src_from_root(input_args->root_route->rank, sg_state->my_rank,
					sg_state->pow_2, sg_state->group_size);

	sg_state->parent_ctl_pointer = sg_state->ctl_structs[sg_state->src];

	while(!IS_SG_DATA_READY(sg_state->parent_ctl_pointer, sg_state->ready_flag,
								sg_state->sequence_number)) {
				opal_progress();

    }
	sg_state->matched = true;

	/* If I am a secondary root */
	if ((sg_state->matched) && (sg_state->src == sg_state->pow_2 + sg_state->my_rank)) {

		rc = sm_portals_secondary_root_scatter(sg_state);
		if (rc != OMPI_SUCCESS) {
				goto Release;
		}
Scatter_extra_root_wait:

		for( i = 0; i < sg_state->cs->num_to_probe && completed_posts < sg_state->my_ctl_pointer->n_sends;
						i++) {

			completed_posts = wait_for_post_complete_nb(sg_state->my_rank, sg_state->my_ctl_pointer->n_sends,
							sg_state->ctl_structs, sg_state->ready_flag, sg_state->sequence_number);

		}

		if (completed_posts < sg_state->my_ctl_pointer->n_sends) {
			sg_state->phase = SCATTER_EXTRA_ROOT_WAIT;
			return  BCOL_FN_STARTED;
		}

		goto Allgather;
    }

    /* we need to see whether this is really
     * who we are looking for
     */
    for( i = 0; i < sg_state->parent_ctl_pointer->n_sends; i++) {
		uint64_t local_offset = 0;
		uint64_t remote_offset = 0;

		BASESMUMA_VERBOSE(5,("%d found it from %d \n",sg_state->my_rank,sg_state->src));

       if( sg_state->my_rank == (sg_state->src^(1<<i))) {
            sg_state->parent_ctl_pointer = sg_state->ctl_structs[sg_state->src];

            /* we found our root within the group ... */
            BASESMUMA_VERBOSE(10,("Shared memory probe was matched, the root is	%d ",sg_state->src));

			sg_state->my_ctl_pointer->n_sends = i;

		    /* Am I source for other process during scatter phase */
            if ( i > 0) {

				rc = sm_portals_internode_scatter(sg_state);

				if (rc != OMPI_SUCCESS) {
					goto Release;
				}
Scatter_parent_wait:

				for( i = 0; i < sg_state->cs->num_to_probe && completed_posts < sg_state->my_ctl_pointer->n_sends;
						i++) {

					completed_posts = wait_for_post_complete_nb(sg_state->my_rank,
								sg_state->my_ctl_pointer->n_sends,
								sg_state->ctl_structs,
								sg_state->ready_flag, sg_state->sequence_number);
				}

				if (completed_posts < sg_state->my_ctl_pointer->n_sends) {
					sg_state->phase = SCATTER_PARENT_WAIT;
					return  BCOL_FN_STARTED;
				}

            } else {

                /* takes care of first level recursive double */
		sg_state->length = sg_state->parent_ctl_pointer->length/
                    (1<<(sg_state->parent_ctl_pointer->n_sends - 1));
                sg_state->my_ctl_pointer->length = sg_state->length;
                sg_state->my_ctl_pointer->offset = sg_state->parent_ctl_pointer->offset;


				while(!IS_SG_DATA_READY(sg_state->parent_ctl_pointer,
										sg_state->ready_flag, sg_state->sequence_number)) {
			opal_progress();
			}

				mca_bcol_basesmuma_portals_get_msg_fragment(sg_state->cs,
								sg_state->read_eq,
								&sg_state->my_ctl_pointer->portals_buf_addr,
								&sg_state->parent_ctl_pointer->portals_buf_addr,
								sg_state->my_ctl_pointer->offset,
								sg_state->my_ctl_pointer->offset, sg_state->length);

				/* signal that I am done reading data from parent */
	        sg_state->my_ctl_pointer->flag = sg_state->ready_flag;
			}

            BASESMUMA_VERBOSE(5,("Completed %d found it from %d \n",
									sg_state->my_rank, sg_state->src));

			while(sg_state->ready_flag > sg_state->parent_ctl_pointer->flag);

			goto Allgather;
        }
	}

	{
	/* this is not who we are looking for,
	 * mark as false positive so we don't
	 * poll here again
	 */
		sg_state->src_list[sg_state->src_list_index] = -1;
	sg_state->matched = 0;
	goto Probe;
     }

Allgather:

    /* zip it back up - we have already taken care of first level */
    sg_state->global_sg_offset = sg_state->my_ctl_pointer->offset;

	/* first level of zip up */
    sg_state->length = 2 * sg_state->fragment_size/sg_state->pow_2;

	/* Posting for all phases of recursive doubling */
	extra_src_posts = (sg_state->my_rank + sg_state->pow_2 < sg_state->group_size ) ? 1: 0;
	allgather_posts = sg_state->pow_2_levels - 1;
	total_msg_posts = allgather_posts + extra_src_posts ;

	if ((!sg_state->msg_posted) && (total_msg_posts > 0)){

			mca_bcol_basesmuma_portals_post_msg(sg_state->cs, &sg_state->my_ctl_pointer->portals_buf_addr,
						   sg_state->my_userbuf, sg_state->my_ctl_pointer->portals_buf_addr.userbuf_length,
						   PTL_EQ_NONE, total_msg_posts, blocked_post,
						   PTL_MD_EVENT_START_DISABLE| PTL_MD_EVENT_END_DISABLE
						   | PTL_MD_OP_GET | PTL_MD_MANAGE_REMOTE | PTL_MD_TRUNCATE | PTL_MD_EVENT_AUTO_UNLINK_ENABLE
							);
			sg_state->msg_posted = true;
	}

	sg_state->ready_flag++;
    opal_atomic_wmb ();
    sg_state->my_ctl_pointer->flag = sg_state->ready_flag;

	rc = sm_portals_bcasts_allgather_phase(sg_state);

	if (rc != OMPI_SUCCESS) {
		BASESMUMA_VERBOSE(10,("Error in Bcast's allgather phase "));
		goto Release;
	}

	/* If I am source for non-power 2 children wait for them */
	/* If I am secondary root then my partner would be real root
	 * so no need for exchange of data with the extra partner */
	sg_state->extra_partner = sg_state->my_rank + sg_state->pow_2 ;
	if ((sg_state->extra_partner < sg_state->group_size) && (!sg_state->secondary_root)) {

		sg_state->extra_partner_ctl_pointer = sg_state->ctl_structs[sg_state->extra_partner];
		/* Block until extra partner has copied data */
	while(!IS_SG_DATA_READY(sg_state->extra_partner_ctl_pointer,
								sg_state->ready_flag, sg_state->sequence_number)) {
		    opal_progress();
        }

	}

Release:

    BASESMUMA_VERBOSE(1,("Im done "));

    sg_state->my_ctl_pointer->starting_flag_value++;
    sg_state->phase = FINISHED;

	return BCOL_FN_COMPLETE;

}
#endif /* __PORTALS_AVAIL__ */
