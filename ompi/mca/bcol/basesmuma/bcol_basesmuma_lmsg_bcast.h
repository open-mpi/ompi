#ifdef __PORTALS_AVAIL__
#define __PORTALS_ENABLE__

#include <unistd.h>

#include "ompi_config.h"
#include "ompi/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"

#include "bcol_basesmuma_utils.h"
#include "bcol_basesmuma_portals.h"
#include "bcol_basesmuma.h"

#if 0
struct scatter_allgather_nb_bcast_state_t
{
    /* local variables */
    uint64_t length;
    int my_rank, src, matched;
    int *src_list;
    int group_size;
	int64_t ready_flag;
    int pow_2, pow_2_levels;
    int src_list_index;
    uint64_t fragment_size;  /* user buffer size */

	/* Input argument variables */
	void *my_userbuf;
	int64_t sequence_number;

	/* Extra source variables */
	bool secondary_root;
	int partner , extra_partner;

	/* Scatter Allgather offsets */
	uint64_t local_sg_offset , global_sg_offset , partner_offset ;

	/* Portals messaging relevant variables */
	ptl_handle_eq_t allgather_eq_h;
	ptl_handle_eq_t read_eq;
	ptl_event_t  allgather_event;
	bool msg_posted;

	/* OMPI module and component variables */
    mca_bcol_basesmuma_component_t *cs;
    mca_bcol_basesmuma_module_t *bcol_module;

	/* Control structure and payload variables */
	volatile mca_bcol_basesmuma_ctl_struct_t **ctl_structs;
    volatile mca_bcol_basesmuma_ctl_struct_t  *my_ctl_pointer;
	volatile mca_bcol_basesmuma_ctl_struct_t  *parent_ctl_pointer; /* scatter source */
	volatile mca_bcol_basesmuma_ctl_struct_t  *extra_partner_ctl_pointer; /* scatter source */

	int phase;
};

typedef struct scatter_allgather_nb_bcast_state_t sg_state_t;
#endif

bool blocked_post = false;

#define IS_SG_DATA_READY(peer, my_flag, my_sequence_number) 								\
    (((peer)->sequence_number == (my_sequence_number) && 									\
      (peer)->flags[BCAST_FLAG] >= (my_flag) 															\
     )? true : false )



#define  SG_LARGE_MSG_PROBE(src_list, n_src, src_list_index, matched,								\
						    src, data_buffs, data_src_ctl_pointer,							\
							data_src_lmsg_ctl_pointer, ready_flag,							\
							sequence_number)  					  							\
do {                                                                              			\
    int j;                                                                        			\
    for( j = 0; j < n_src; j++) {                                                 			\
        if(src_list[j] != -1) {                                                   			\
            data_src_ctl_pointer = data_buffs[src_list[j]].ctl_struct;                      \
            data_src_lmsg_ctl_pointer = (mca_bcol_basesmuma_portal_buf_addr_t*)				\
										data_buffs[src_list[j]].payload;                    \
            if( IS_SG_DATA_READY(data_src_ctl_pointer,ready_flag,sequence_number)) {   	    \
                src = src_list[j];                                                			\
                matched = 1;                                                      			\
                src_list_index = j;   																\
                break;                                                        				\
            }                                                                     			\
        }                                                                         			\
    }                                                                             			\
} while(0)

#define  SG_LARGE_MSG_NB_PROBE(src_list, n_src, src_list_index, matched,					\
						    src, ctl_structs, data_src_ctl_pointer,							\
							ready_flag, sequence_number)  									\
do {                                                                              			\
    int j;                                                                        			\
    for( j = 0; j < n_src; j++) {                                                 			\
        if(src_list[j] != -1) {                                                   			\
            data_src_ctl_pointer = ctl_structs[src_list[j]];		                        \
            if( IS_SG_DATA_READY(data_src_ctl_pointer,ready_flag,sequence_number)) {   	    \
                src = src_list[j];                                                			\
                matched = 1;                                                      			\
                src_list_index = j;   														\
                break;                                                        				\
            }                                                                     			\
        }                                                                         			\
    }                                                                             			\
} while(0)





static inline  __opal_attribute_always_inline__
int wait_for_peers(int my_rank, int npeers, volatile mca_bcol_basesmuma_payload_t *data_buffs,
				int flag_value, int sn)
{
	int *peers_list = NULL;
	int counter = 0, diter = 0;
	volatile mca_bcol_basesmuma_header_t *peer_ctl_pointer = NULL;

	peers_list = (int *)malloc(sizeof(int) * npeers);

	for (diter = 0; diter < npeers; diter++ ){
		peers_list[diter] = my_rank ^ (1<<diter);
		assert(peers_list[diter] != -1);
	}

	counter = 0;
	while (counter < npeers) {
		for (diter = 0; diter < npeers; diter++){
			if (-1 != peers_list[diter]) {
				peer_ctl_pointer = data_buffs[peers_list[diter]].ctl_struct;

				if (IS_SG_DATA_READY(peer_ctl_pointer, flag_value, sn)) {
					counter++;
					peers_list[diter] = -1;
				}
			}
		}
		opal_progress();
	}

	return 0;
}

static inline  __opal_attribute_always_inline__
int wait_for_peers_nb(int my_rank, int npeers,
				volatile mca_bcol_basesmuma_ctl_struct_t **ctl_structs,
				volatile int flag_value, int sn)
{
	int *peers_list = NULL;
	int counter = 0, diter = 0;
	volatile mca_bcol_basesmuma_ctl_struct_t *peer_ctl_pointer = NULL;

	peers_list = (int *)malloc(sizeof(int) * npeers);

	for (diter = 0; diter < npeers; diter++ ){
		peers_list[diter] = my_rank ^ (1<<diter);
		assert(peers_list[diter] != -1);
	}

	counter = 0;
	while (counter < npeers) {
		for (diter = 0; diter < npeers; diter++){
			if (-1 != peers_list[diter]) {
				peer_ctl_pointer = ctl_structs[peers_list[diter]];

				if (IS_SG_DATA_READY(peer_ctl_pointer, flag_value, sn)) {
					counter++;
					peers_list[diter] = -1;
				}
			}
		}
		opal_progress();
	}

	return 0;
}

static inline  __opal_attribute_always_inline__
int wait_for_post_complete_nb(int my_rank, int npeers,
				volatile mca_bcol_basesmuma_ctl_struct_t **ctl_structs,
				int flag_value, int sn)
{
	/* int *peers_list = NULL; */
	int peers_list[MAX_SM_GROUP_SIZE];
	int counter = 0, diter = 0;
	volatile mca_bcol_basesmuma_ctl_struct_t *peer_ctl_pointer = NULL;

/*	peers_list = (int *)malloc(sizeof(int) * npeers); */

	assert(npeers < MAX_SM_GROUP_SIZE);

	for (diter = 0; diter < npeers; diter++ ){
		peers_list[diter] = my_rank ^ (1<<diter);
		assert(peers_list[diter] != -1);
	}

	counter = 0;
	for (diter = 0; diter < npeers; diter++){
		peer_ctl_pointer = ctl_structs[peers_list[diter]];

		if (IS_SG_DATA_READY(peer_ctl_pointer, flag_value, sn)) {
					counter++;
		}
	}

/*	free(peers_list); */
	return counter;
}

static inline  __opal_attribute_always_inline__
int  sg_large_msg_probe(sg_state_t *sg_state)
{
	int j,n_src = sg_state->pow_2_levels+1;


	for( j = 0; j < n_src; j++) {
        if(sg_state->src_list[j] != -1) {
			sg_state->parent_ctl_pointer = sg_state->ctl_structs[sg_state->src_list[j]];

			BASESMUMA_VERBOSE(5,("Parent %d ctl pointer (parent=%x, my ctl=%x) flag %d",
								sg_state->src_list[j],sg_state->parent_ctl_pointer,
								sg_state->my_ctl_pointer,
								sg_state->parent_ctl_pointer->flag));

			if (IS_SG_DATA_READY(sg_state->parent_ctl_pointer,
						sg_state->ready_flag, sg_state->sequence_number)) {
                sg_state->src = sg_state->src_list[j];
                sg_state->matched = 1;
                sg_state->src_list_index = j;
				break;
            }
        }
    }

	return 0;
}
/*
 * I will post message for all the my children
 */
static inline  __opal_attribute_always_inline__
int sm_portals_root_scatter(sg_state_t *sg_state)
{
	int extra_src_posts = -1, scatter_posts = -1, allgather_posts = -1,
						total_msg_posts = -1;

	BASESMUMA_VERBOSE(10,("I am the root of the data"));
    sg_state->my_ctl_pointer->offset = 0;
    sg_state->my_ctl_pointer->n_sends = sg_state->pow_2_levels;
    sg_state->my_ctl_pointer->length = sg_state->fragment_size;



	extra_src_posts = (sg_state->my_rank + sg_state->pow_2 < sg_state->group_size ) ? 1: 0;
	scatter_posts = sg_state->my_ctl_pointer->n_sends;
	allgather_posts = sg_state->pow_2_levels - 1;

	total_msg_posts = scatter_posts + allgather_posts + extra_src_posts ;

	if ( total_msg_posts <= 0) {
		BASESMUMA_VERBOSE(10,("No need to post the data "));
		return OMPI_SUCCESS;
	}

	mca_bcol_basesmuma_portals_post_msg(sg_state->cs,
						 &sg_state->my_ctl_pointer->portals_buf_addr,
						   sg_state->my_userbuf, sg_state->fragment_size,
						   PTL_EQ_NONE,
						   total_msg_posts,
						   blocked_post,
						  PTL_MD_EVENT_START_DISABLE| PTL_MD_EVENT_END_DISABLE |
						  PTL_MD_OP_GET | PTL_MD_MANAGE_REMOTE | PTL_MD_TRUNCATE | PTL_MD_EVENT_AUTO_UNLINK_ENABLE);

	/*
	 mca_bcol_basesmuma_portals_post_msg(sg_state->cs,
						 &sg_state->my_ctl_pointer->portals_buf_addr,
						   sg_state->my_userbuf, sg_state->fragment_size,
						   sg_state->allgather_eq_h,
						   total_msg_posts,
						   blocked_post,
						  PTL_MD_EVENT_START_DISABLE| PTL_MD_EVENT_END_DISABLE |
						  PTL_MD_OP_GET | PTL_MD_MANAGE_REMOTE | PTL_MD_TRUNCATE | PTL_MD_EVENT_AUTO_UNLINK_ENABLE);
	 */

	 sg_state->msg_posted = true ;

	/*
	opal_atomic_wmb();
	*/
	sg_state->my_ctl_pointer->flag = sg_state->ready_flag;

	return OMPI_SUCCESS;
}

/*
 * Im root but my rank > pow2_groupsize, so will copy to partner who
 * will act as root (secondary)
 */
static inline  __opal_attribute_always_inline__
int sm_portals_extra_root_scatter(sg_state_t *sg_state)
{
	int scatter_partner = -1;
	volatile mca_bcol_basesmuma_ctl_struct_t *scatter_partner_ctl_pointer = NULL;

	int	total_msg_posts  = 1;

	if ( total_msg_posts <= 0) {
		BASESMUMA_VERBOSE(10,("No need to post the data "));
	}
	else {
		mca_bcol_basesmuma_portals_post_msg(sg_state->cs,
						 &sg_state->my_ctl_pointer->portals_buf_addr,
						   sg_state->my_userbuf, sg_state->fragment_size,
						   PTL_EQ_NONE,
						   total_msg_posts,
						   blocked_post,
						  PTL_MD_EVENT_START_DISABLE| PTL_MD_EVENT_END_DISABLE | PTL_MD_OP_GET
						  | PTL_MD_MANAGE_REMOTE | PTL_MD_TRUNCATE | PTL_MD_EVENT_AUTO_UNLINK_ENABLE);
	sg_state->msg_posted = true ;

	}

	opal_atomic_wmb();
	sg_state->my_ctl_pointer->flag = sg_state->ready_flag;



	scatter_partner = sg_state->my_rank - sg_state->pow_2;
	scatter_partner_ctl_pointer =
					sg_state->ctl_structs[scatter_partner];

	while(!IS_SG_DATA_READY(scatter_partner_ctl_pointer, sg_state->ready_flag,
									sg_state->sequence_number)){
					opal_progress();
	}

	return OMPI_SUCCESS;
}

/*
 * Gets msg from the partner (> pow2_groupsize) and posts the
 * message acting as root
 */
static inline  __opal_attribute_always_inline__
int sm_portals_secondary_root_scatter(sg_state_t *sg_state)
{

	volatile mca_bcol_basesmuma_ctl_struct_t *extra_src_ctl_pointer = NULL;
	int scatter_posts, allgather_posts, extra_src_posts, total_msg_posts;

	sg_state->secondary_root = true;
    BASESMUMA_VERBOSE(10,("I am the secondary root for the data"));
    sg_state->my_ctl_pointer->offset = 0;
    sg_state->my_ctl_pointer->n_sends = sg_state->pow_2_levels;
    sg_state->my_ctl_pointer->length = sg_state->fragment_size;

	extra_src_ctl_pointer = sg_state->ctl_structs[sg_state->src];

	mca_bcol_basesmuma_portals_get_msg_fragment(sg_state->cs,
						sg_state->read_eq,
						&sg_state->my_ctl_pointer->portals_buf_addr,
						&extra_src_ctl_pointer->portals_buf_addr, 0,
						0, sg_state->fragment_size);


	extra_src_posts = 0;
	scatter_posts = sg_state->my_ctl_pointer->n_sends;
	allgather_posts = sg_state->pow_2_levels - 1;

	total_msg_posts = scatter_posts + allgather_posts + extra_src_posts ;

	if (total_msg_posts > 0) {
		mca_bcol_basesmuma_portals_post_msg(sg_state->cs,
						  &sg_state->my_ctl_pointer->portals_buf_addr,
						   sg_state->my_userbuf, sg_state->fragment_size,
						   PTL_EQ_NONE,
						   total_msg_posts,
						   blocked_post,
						   PTL_MD_EVENT_START_DISABLE| PTL_MD_EVENT_END_DISABLE | PTL_MD_OP_GET
						   | PTL_MD_MANAGE_REMOTE | PTL_MD_TRUNCATE | PTL_MD_EVENT_AUTO_UNLINK_ENABLE);
		sg_state->msg_posted = true ;
	}
    opal_atomic_wmb();
    sg_state->my_ctl_pointer->flag = sg_state->ready_flag;

	return OMPI_SUCCESS;
}

/*
 * Internode Scatter: Get data from my parent and post for my children
 */

static inline  __opal_attribute_always_inline__
int sm_portals_internode_scatter(sg_state_t *sg_state)
{

	int scatter_posts, allgather_posts, extra_src_posts,
					total_msg_posts;
	uint64_t local_offset, remote_offset;

	/* compute the size of the chunk to copy */
	sg_state->length = (sg_state->parent_ctl_pointer->length)/
       (1<<(sg_state->parent_ctl_pointer->n_sends - sg_state->my_ctl_pointer->n_sends));
	sg_state->my_ctl_pointer->length = sg_state->length;
	sg_state->my_ctl_pointer->offset =
				sg_state->parent_ctl_pointer->offset + sg_state->length;


	local_offset = sg_state->my_ctl_pointer->offset;
	remote_offset = sg_state->parent_ctl_pointer->offset +
						sg_state->length;

	mca_bcol_basesmuma_portals_get_msg_fragment(sg_state->cs,
								sg_state->read_eq,
								&sg_state->my_ctl_pointer->portals_buf_addr,
								&sg_state->parent_ctl_pointer->portals_buf_addr,local_offset,
								remote_offset,sg_state->length);

	/* Now post the message for other children to read */
	extra_src_posts = (sg_state->my_rank + sg_state->pow_2 <
								sg_state->group_size ) ? 1: 0;
	scatter_posts = sg_state->my_ctl_pointer->n_sends;
	allgather_posts = sg_state->pow_2_levels - 1;

	total_msg_posts = scatter_posts + allgather_posts + extra_src_posts ;

	if (total_msg_posts > 0) {
		mca_bcol_basesmuma_portals_post_msg(sg_state->cs, &sg_state->my_ctl_pointer->portals_buf_addr,
						   sg_state->my_userbuf, sg_state->my_ctl_pointer->portals_buf_addr.userbuf_length,
						   PTL_EQ_NONE,
						   total_msg_posts,
						   blocked_post,
						   PTL_MD_EVENT_START_DISABLE| PTL_MD_EVENT_END_DISABLE
						   | PTL_MD_OP_GET | PTL_MD_MANAGE_REMOTE | PTL_MD_TRUNCATE | PTL_MD_EVENT_AUTO_UNLINK_ENABLE);

		sg_state->msg_posted = true;
	}
	/*
    opal_atomic_wmb();
	 */
    sg_state->my_ctl_pointer->flag = sg_state->ready_flag;

	return OMPI_SUCCESS;
}

/*
 * Bcast's Allgather Phase:
 * Combines data from all processes using recursive doubling algorithm
 */
static inline  __opal_attribute_always_inline__
int sm_portals_bcasts_allgather_phase(sg_state_t *sg_state)
{
	int ag_loop,  partner;
	volatile mca_bcol_basesmuma_ctl_struct_t  *partner_ctl_pointer = NULL; /* recursive double */


	for( ag_loop = 1; ag_loop < sg_state->pow_2_levels; ag_loop++) {
	        /* get my partner for this level */
        partner = sg_state->my_rank^(1<<ag_loop);
        partner_ctl_pointer = sg_state->ctl_structs[partner];


		/* Block until partner is at this level of recursive-doubling stage */
        while(!IS_SG_DATA_READY(partner_ctl_pointer, sg_state->ready_flag,
								sg_state->sequence_number)) {
            opal_progress();
        }
        assert(partner_ctl_pointer->flag >= sg_state->ready_flag);

		if (partner_ctl_pointer->offset < sg_state->my_ctl_pointer->offset) {
			sg_state->global_sg_offset -= sg_state->length;
			sg_state->local_sg_offset = sg_state->global_sg_offset;
		} else {
			sg_state->local_sg_offset = sg_state->global_sg_offset + sg_state->length;
		}


		BASESMUMA_VERBOSE(10,("Allgather Phase: Get message from process %d, length %d",
								partner, sg_state->length));
		mca_bcol_basesmuma_portals_get_msg_fragment(sg_state->cs,
								sg_state->read_eq,
								&sg_state->my_ctl_pointer->portals_buf_addr,
								&partner_ctl_pointer->portals_buf_addr,sg_state->local_sg_offset,
								sg_state->local_sg_offset, sg_state->length);

		sg_state->ready_flag++;
		opal_atomic_wmb();
	sg_state->my_ctl_pointer->flag = sg_state->ready_flag;

		/* Block until partner is at this level of recursive-doubling stage */
	while(!IS_SG_DATA_READY(partner_ctl_pointer, sg_state->ready_flag,
								sg_state->sequence_number)) {
		    opal_progress();
        }

        /* double the length */
        sg_state->length *= 2;
    }

	return OMPI_SUCCESS;

}


static inline  __opal_attribute_always_inline__
int init_sm_group_info(sg_state_t *sg_state, int buff_idx)
{
	int idx, leading_dim;
	int first_instance=0;
    int flag_offset;

	/* Get addresing information */
    sg_state->group_size = sg_state->bcol_module->colls_no_user_data.size_of_group;
    leading_dim = sg_state->bcol_module->colls_no_user_data.size_of_group;
    idx=SM_ARRAY_INDEX(leading_dim,buff_idx,0);

	BASESMUMA_VERBOSE(1,("My buffer idx %d group size %d, leading dim %d, idx %d",
							buff_idx,sg_state->group_size,leading_dim,idx));
    /* grab the ctl buffs */
    sg_state->ctl_structs = (volatile mca_bcol_basesmuma_ctl_struct_t **)
        sg_state->bcol_module->colls_with_user_data.ctl_buffs+idx;

	sg_state->my_rank = sg_state->bcol_module->super.sbgp_partner_module->my_index;
    sg_state->my_ctl_pointer = sg_state->ctl_structs[sg_state->my_rank];

	if (sg_state->my_ctl_pointer->sequence_number < sg_state->sequence_number) {
        first_instance = 1;
    }

    if(first_instance) {
        sg_state->my_ctl_pointer->flag = -1;
        sg_state->my_ctl_pointer->index = 1;

        sg_state->my_ctl_pointer->starting_flag_value = 0;
        flag_offset = 0;

    } else {
        sg_state->my_ctl_pointer->index++;
    }

	/* For bcast we shud have only entry to this bcol
	assert(sg_state->my_ctl_pointer->flag == -1);
	*/

	/* increment the starting flag by one and return */
    flag_offset = sg_state->my_ctl_pointer->starting_flag_value;
    sg_state->ready_flag = flag_offset + sg_state->sequence_number + 1;

    sg_state->my_ctl_pointer->sequence_number = sg_state->sequence_number;

	return OMPI_SUCCESS;

}

static inline  __opal_attribute_always_inline__
int init_sm_portals_sg_info(sg_state_t *sg_state)
{
/* Get portals info*/
	mca_bcol_basesmuma_portal_proc_info_t *portals_info;
	int rc = OMPI_SUCCESS;
	int sg_matchbits;

	portals_info = (mca_bcol_basesmuma_portal_proc_info_t*)sg_state->cs->portals_info;

	sg_matchbits = sg_state->sequence_number ;

	/* Construct my portal buffer address and copy to payload buffer */
	mca_bcol_basesmuma_construct_portal_address(&sg_state->my_ctl_pointer->portals_buf_addr,
						portals_info->portal_id.nid,
						portals_info->portal_id.pid,
						sg_matchbits,
						sg_state->bcol_module->super.sbgp_partner_module->group_comm->c_contextid);

	sg_state->my_ctl_pointer->portals_buf_addr.userbuf = sg_state->my_userbuf;
	sg_state->my_ctl_pointer->portals_buf_addr.userbuf_length = sg_state->fragment_size;

	return OMPI_SUCCESS;
}

static inline  __opal_attribute_always_inline__
int compute_src_from_root(int group_root, int my_group_rank, int pow2, int
				group_size)
{

	int root, relative_rank, src, i;

	if (group_root < pow2) {
        root = group_root;
    } else {
        /* the source of the data is extra node,
           the real root it represented by some rank from
           pow2 group */
        root = group_root - pow2;
        /* shortcut for the case when my rank is root for the group */
        if (my_group_rank == root) {
            return group_root;
        }
    }

    relative_rank = (my_group_rank - root) < 0 ? my_group_rank - root + pow2 :
                                           my_group_rank - root;

    for (i = 1; i < pow2; i<<=1) {
        if (relative_rank & i) {
            src = my_group_rank ^ i;
            if (src >= pow2)
                src -= pow2;

            return src;
        }
    }

	return -1;
}

int bcol_basesmuma_lmsg_scatter_allgather_portals_bcast(bcol_function_args_t *input_args,
    mca_bcol_base_function_t *c_input_args);

int bcol_basesmuma_lmsg_scatter_allgather_portals_nb_bcast(bcol_function_args_t *input_args,
    mca_bcol_base_function_t *c_input_args);

int bcol_basesmuma_lmsg_scatter_allgather_portals_nb_knownroot_bcast(bcol_function_args_t *input_args,
    mca_bcol_base_function_t *c_input_args);

#endif
