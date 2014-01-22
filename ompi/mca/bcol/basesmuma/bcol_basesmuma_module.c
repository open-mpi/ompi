/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC. All rights
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
#include "ompi/communicator/communicator.h"
#include "ompi/mca/bcol/bcol.h"
#include "ompi/mca/bcol/base/base.h"
#include "ompi/patterns/net/netpatterns.h"

#include "opal/util/show_help.h"
#include "opal/align.h"

#include "ompi/mca/bcol/basesmuma/bcol_basesmuma_reduce.h"
#include "bcol_basesmuma.h"
#include "bcol_basesmuma_utils.h"

#ifdef __PORTALS_AVAIL__
#include "bcol_basesmuma_portals.h"
#endif



#if 0

/* debug */
static mca_bcol_basesmuma_module_t *test_sm_module;

int test_addressing(int seq_num,
                    mca_bcol_base_module_t *b_module)
{

    /* local variables */
    int ret=OMPI_SUCCESS, idx, leading_dim, loop_cnt, exchange, flag_to_set, i;
    int pair_rank, flag_offset;
    mca_bcol_basesmuma_ctl_struct_t **ctl_structs;
    netpatterns_pair_exchange_node_t *my_exchange_node;
    int extra_rank, my_rank, pow_2;
    mca_bcol_basesmuma_ctl_struct_t volatile *partner_ctl;
    mca_bcol_basesmuma_ctl_struct_t volatile *my_ctl;
    int64_t sequence_number,seq_actual,seq_exp,flag_actual,flag_exp ;
    bool found;
    int buff_index, first_instance=0;
    mca_bcol_basesmuma_module_t *bcol_module=
        (mca_bcol_basesmuma_module_t *)b_module;

    /* get the pointer to the segment of control structures */
    my_exchange_node=&(bcol_module->recursive_doubling_tree);
    my_rank=bcol_module->super.sbgp_partner_module->my_index;
    pow_2=bcol_module->super.sbgp_partner_module->pow_2;

    /* figure out what instance of the basesmuma bcol I am */
    leading_dim=bcol_module->colls_no_user_data.size_of_group;
    sequence_number=seq_num;
    buff_index=sequence_number & (bcol_module->colls_no_user_data.mask);
    idx=SM_ARRAY_INDEX(leading_dim,buff_index,0);
    ctl_structs=(mca_bcol_basesmuma_ctl_struct_t **)
        bcol_module->colls_no_user_data.ctl_buffs+idx;
    my_ctl=ctl_structs[my_rank];
    for(i=0 ; i < leading_dim ; i++ ) {
        partner_ctl=ctl_structs[i];
        seq_actual=partner_ctl->sequence_number;
        flag_actual=partner_ctl->flag;
        seq_exp=i;
        flag_exp=sequence_number;
        if( seq_actual != seq_exp ) {
            fprintf(stderr," Error sn expected %ld  got %ld \n",
                    seq_exp,seq_actual);
            fflush(stderr);
        }
        if( flag_actual != flag_exp ) {
            fprintf(stderr," Error flag expected %ld  got %ld \n",
                    flag_exp,flag_actual);
            fflush(stderr);
        }
    }
}
static void test_resrouce_recycle()
{
    int i,j, grp_size, my_rank;
    int idx,sum;
    mca_bcol_basesmuma_component_t *cs = &mca_bcol_basesmuma_component;
    int num_bufs= cs->basesmuma_num_mem_banks*
        cs->basesmuma_num_regions_per_bank;
    int64_t sn=100;
    bcol_function_args_t fun_args;
    mca_bcol_basesmuma_ctl_struct_t **ctl_structs;

    fprintf(stderr," num buffs %d \n",num_bufs);
    fflush(stderr);

    sm_nbbar_desc_t *sm_desc=
        &((test_sm_module->colls_no_user_data.ctl_buffs_mgmt[0].nb_barrier_desc));

    grp_size=sm_desc->sm_module->super.sbgp_partner_module->group_size;
    my_rank=sm_desc->sm_module->super.sbgp_partner_module->my_index;

    /* need to hack the sequence number offset, as this is set after this
     * routine is called */
    test_sm_module->super.squence_number_offset=100;

    for( i=0 ; i < num_bufs +2 ; i++ ) {
        /* get my ctl structure index in this buffer set */
        idx=SM_ARRAY_INDEX(grp_size,i,0);
        ctl_structs=(mca_bcol_basesmuma_ctl_struct_t **)
            test_sm_module->colls_no_user_data.ctl_buffs+idx;
        ctl_structs[my_rank]->sequence_number=(int64_t)my_rank;
        ctl_structs[my_rank]->flag=(int64_t)i;
    }

    /* mark that I am here */
    idx=SM_ARRAY_INDEX(grp_size,(num_bufs+1),0);
    ctl_structs=(mca_bcol_basesmuma_ctl_struct_t **)
        test_sm_module->colls_no_user_data.ctl_buffs+idx;
    ctl_structs[my_rank]->index=(int64_t)1;

    /* make sure all have arrived */

    sum=0;
    while(sum < grp_size ) {
        sum=0;
        idx=SM_ARRAY_INDEX(grp_size,(num_bufs+1),0);
        ctl_structs=(mca_bcol_basesmuma_ctl_struct_t **)
            test_sm_module->colls_no_user_data.ctl_buffs+idx;
        for( i=0 ; i < grp_size ; i++ ) {
            sum+=ctl_structs[i]->index;
        }

    }

    /* check to see what values we see */
    for( i=0 ; i < num_bufs +2 ; i++ ) {
        idx=SM_ARRAY_INDEX(grp_size,i,0);
        ctl_structs=(mca_bcol_basesmuma_ctl_struct_t **)
            test_sm_module->colls_no_user_data.ctl_buffs+idx;
        for( j=0 ; j < grp_size ; j++ ) {
            if( ctl_structs[j]->sequence_number != j ) {
                fprintf(stderr," Error sn %ld expected %ld i %d j %d my %d \n",
                        ctl_structs[i]->sequence_number,j, i,j,my_rank);
                fflush(stderr);
            }
            if( ctl_structs[j]->flag != i ) {
                fprintf(stderr," Error sn %ld expected %ld \n",
                        ctl_structs[i]->flag,i);
                fflush(stderr);
            }

        }
    }

    for( i=0 ; i < num_bufs ; i++ ) {
        test_addressing((int64_t)i,test_sm_module);
    }
    /*
      fun_args.n_of_this_type_in_collective=2;
      for( i=0 ; i < 2*grp_size ; i++ ) {

      if( my_rank == (i%grp_size) ) {
      sleep(3);
      }

      fun_args.sequence_num=sn;
      sn++;
      bcol_basesmuma_fanin(&fun_args,
      (mca_bcol_base_module_t *)test_sm_module);
      bcol_basesmuma_fanout(&fun_args,
      (mca_bcol_base_module_t *)test_sm_module);

      if( my_rank == (i%grp_size) ) {
      fprintf(stderr," hi from %d \n",my_rank);
      fflush(stderr);
      }

      }
      for( i=0 ; i < 2*grp_size ; i++ ) {

      if( my_rank == (i%grp_size) ) {
      sleep(3);
      }
      bcol_basesmuma_rd_nb_barrier_init_admin(sm_desc);
      while( sm_desc->collective_phase != NB_BARRIER_DONE ) {
      bcol_basesmuma_rd_nb_barrier_progress_admin(sm_desc);
      }
      sm_desc->sm_module->colls_no_user_data.ctl_buffs_mgmt[0].bank_gen_counter++;
      if( my_rank == (i%grp_size) ) {
      fprintf(stderr," hi from %d \n",my_rank);
      fflush(stderr);
      }

      }
    */


    /*
      for( i = 0 ; i < num_bufs+1 ; i++ ) {
      indx=bcol_basesmuma_get_buff_index(&(test_sm_module->colls_no_user_data),
      (uint64_t)i);
      fprintf(stderr," indx %d \n ",indx);
      fflush(stderr);
      opal_progress();
      if( -1 != indx ) {
      ret=bcol_basesmuma_free_buff(&(test_sm_module->colls_no_user_data),
      (uint64_t)i);
      if( OMPI_SUCCESS != ret ) {
      fprintf(stderr," Error when returning %d \n",i);
      fflush(stderr);
      }
      }
      }
    */

}
/* end debug */
#endif

#if 0  /* this routine (with appropriate bug fixes) is correct when
        * the subgroup is a single socket.  However, it is not correct
        * if used over several sockets - the data is correct, but dermining
        * the tree radix based on that is not.
        */
/*
 * Local function
 * Get the maximal number of processors sharing the
 * same socket within a sub-group.
 * @param my_rank - the rank of the executing process.
 * @param group_comm - pointer to the sbgp's communicator.
 * @return - maximal number of procs sharing a socket.
 */
static int get_num_of_ranks_sharing_socket(int my_rank, struct ompi_communicator_t *group_comm)
{
    int i;
    int rc;
    int ret;
    int proc;
    int local;
    bool bound;
    int cnt = 0;
    int socket_tmp;
    int group_size;

    int32_t pcnt = 1;
    int num_sockets;
    opal_list_t peers;
    int n_local_peers;
    int num_processors;
    int core_index = -1;
    int my_socket_index = -1;
    int* num_proc_per_socket;

    orte_namelist_t *peer;
    struct ompi_proc_t** procs = NULL;
    opal_paffinity_base_cpu_set_t my_cpu_set;
    opal_buffer_t* sbuffer = OBJ_NEW(opal_buffer_t);
    opal_buffer_t* rbuffer = OBJ_NEW(opal_buffer_t);


    /* Set proc info */
    procs = ompi_comm_local_group_procs(group_comm);
    group_size = ompi_comm_size(group_comm);

    /* Get the number of processors on this node */
    ret = opal_paffinity_base_get_processor_info(&num_processors);
    if((OMPI_SUCCESS != ret) || (OMPI_SUCCESS != rc)){
        return 0;
    }


    /* Get process affinity mask */
    OPAL_PAFFINITY_CPU_ZERO(my_cpu_set);
    ret = opal_paffinity_base_get(&my_cpu_set);
    OPAL_PAFFINITY_PROCESS_IS_BOUND(my_cpu_set, &bound);
    if(!bound) {
        return 0;
    }else{
        /* figure out how many sockets are in this system */
        rc = opal_paffinity_base_get_socket_info(&num_sockets);
        if((OMPI_SUCCESS != ret) || (OMPI_SUCCESS != rc)){
            return 0;
        }

        /* No sockets*/
        if(num_sockets == 0){
            return 0;
        }

        /* All subgroup members share the same socket on the node */
        if(num_sockets == 1){
            return group_size;
        }

        /* Allocate a buffer that counts the number of sockets on a node */
        num_proc_per_socket = (int*)calloc(num_sockets, sizeof(int));
        if(NULL == num_proc_per_socket){
            return 0;
        }
        /* Loop over processors on this node */
        /*  for (proc = 0 ; proc < num_processors ; proc++) {
            if (OPAL_PAFFINITY_CPU_ISSET(proc, my_cpu_set)) {
            ret = opal_paffinity_base_get_map_to_socket_core(proc,
            &socket_tmp, &core_index);

            if(my_socket_index != socket_tmp) {
            my_socket_index = socket_tmp;
            break;
            }
            }
            }*/
    }


    /*
     * The subgroup devided according to node hierarchy,
     * therefore several sockets might be shared by the groups procs.
     */

    /* Construct peers list */
    n_local_peers = 0;
    OBJ_CONSTRUCT(&peers, opal_list_t);
    for(proc = 0; proc < group_size; proc++){
        local = OPAL_PROC_ON_LOCAL_NODE(procs[proc]->proc_flags);
        if (local) {
            peer = OBJ_NEW(orte_namelist_t);
            peer->name.jobid = group_comm->c_local_group->grp_proc_pointers[proc]->proc_name.jobid;
            peer->name.vpid = group_comm->c_local_group->grp_proc_pointers[proc]->proc_name.vpid;
            opal_list_append(&peers, &peer->item);
            n_local_peers++;
        }
    }


    /*Pack socket index */
    ret = opal_dss.pack(sbuffer, &my_socket_index, 1, OPAL_INT32);
    if (ORTE_SUCCESS != ret){
        fprintf(stderr, " pack returned error %d for my_socket_index \n",ret);
        fflush(stderr);
        return -1; /*ret;*/
    }

    /*Allgather data over the communicator */
    if (ORTE_SUCCESS != (ret = orte_grpcomm.allgather_list(&peers, sbuffer, rbuffer))) {
        fprintf(stderr," orte_grpcomm.allgather_list returned error %d \n", ret);
        fflush(stderr);
        return -1;
    }

    /* Is every socket being shared by the same number of procs?
     * If not we should go over each proc index and find maximal socket */

    /* TODO: Need to find maximal number of procs
     * sharing the same socket on the node in case
     * there is more than one socket on the node
     * and that the sbgp type is according to node
     * and not socket */
    /*Unpack the data and find fellow socketeers*/
    cnt = 0;
    for (proc = 0; proc < n_local_peers; proc++) {

        int rem_socket_index;

        /*Unpack socket_index*/
        ret = opal_dss.unpack(rbuffer, &rem_socket_index, &pcnt, OPAL_INT32);
        if (ORTE_SUCCESS != ret) {
            fprintf(stderr," Unpack returned error %d for rem_socket_index\n", ret);
            fflush(stderr);
            return -1;
        }

        /*Populate the list*/
        /*    if (rem_socket_index == my_socket_index) {
              cnt++;
              }*/
        num_proc_per_socket[rem_socket_index]++;
    }

    /* Find the maximal number of procs sharing the same socket */
    for(i = 0; i < num_sockets; i++){
        if(cnt < num_proc_per_socket[i]){
            cnt = num_proc_per_socket[i];
        }
    }


    /*Free resources*/
    OBJ_DESTRUCT(&peers);
    OBJ_RELEASE(sbuffer);
    OBJ_RELEASE(rbuffer);
    free(num_proc_per_socket);

    return cnt;
}
#endif

#if 0
/*
 * Local functions
 */
static int basesmuma_module_enable(mca_bcol_base_module_t *module,
                                   struct ompi_communicator_t *comm);
#endif

/*
 * Local functions
 */
static int alloc_lmsg_reduce_offsets_array(mca_bcol_basesmuma_module_t *sm_module)
{
    int rc = OMPI_SUCCESS, i = 0;
    netpatterns_k_exchange_node_t *k_node = &sm_module->knomial_exchange_tree;
    int n_exchanges = k_node->n_exchanges;

    /* Precalculate the allreduce offsets */
    if (0 < k_node->n_exchanges) {
        sm_module->reduce_offsets = (int **)malloc(n_exchanges * sizeof(int*));

        if (!sm_module->reduce_offsets) {
            rc = OMPI_ERROR;
            return rc;
        }

        for (i=0; i < n_exchanges ; i++) {
            sm_module->reduce_offsets[i] = (int *)malloc (sizeof(int) * NOFFSETS);

            if (!sm_module->reduce_offsets[i]){
                rc = OMPI_ERROR;
                return rc;
            }
        }
    }
    return rc;
}

static int free_lmsg_reduce_offsets_array(mca_bcol_basesmuma_module_t *sm_module)
{
    int rc = OMPI_SUCCESS, i = 0;
    netpatterns_k_exchange_node_t *k_node = &sm_module->knomial_exchange_tree;
    int n_exchanges = k_node->n_exchanges;

    if (sm_module->reduce_offsets) {
        for (i=0; i < n_exchanges; i++) {
            free (sm_module->reduce_offsets[i]);
        }

        free(sm_module->reduce_offsets);
    }
    return rc;
}

static void
mca_bcol_basesmuma_module_construct(mca_bcol_basesmuma_module_t *module)
{
    module->super.bcol_component = (mca_bcol_base_component_t *) &mca_bcol_basesmuma_component;
    module->super.list_n_connected = NULL;
    module->super.hier_scather_offset = 0;

}

static void
mca_bcol_basesmuma_module_destruct(mca_bcol_basesmuma_module_t *sm_module)
{
    /* local variables */
    int i;
    mca_bcol_basesmuma_component_t *cs = &mca_bcol_basesmuma_component;

    /*
     * release allocated resrouces
     */

    /* ...but not until you're sure you have no outstanding collectives */
    while(0 != opal_list_get_size(&(cs->nb_admin_barriers))) {
        opal_progress();
    }

#ifdef __PORTALS_AVAIL__
    /* Remove portals bcast specific resources */
    if ( PTL_OK != PtlEQFree(sm_module->sg_state.read_eq)) {
        BASESMUMA_VERBOSE(10,("PtlEQFree() failed:  )"));
    }
#endif

    /* Remove Lmsg Reduce Offsets Array */
    free_lmsg_reduce_offsets_array(sm_module);


    /* collective topology data */
    if( sm_module->fanout_read_tree) {
        for(i=0 ; i < sm_module->super.size_of_subgroup ; i++ ) {
            if(0 < sm_module->fanout_read_tree[i].n_children ) {
                free(sm_module->fanout_read_tree[i].children_ranks);
                sm_module->fanout_read_tree[i].children_ranks=NULL;
            }
        }
        free(sm_module->fanout_read_tree);
        sm_module->fanout_read_tree=NULL;
    }

    /* gvm Leak FIX Reduction_tree[].children_ranks has
     * to be removed. I don't how to get the size (which is
     * size of subgroup) of array reduction_tree
     */
    if( sm_module->reduction_tree) {
        for(i=0 ; i < sm_module->super.size_of_subgroup ; i++ ) {
            if(0 < sm_module->reduction_tree[i].n_children ) {
                free(sm_module->reduction_tree[i].children_ranks);
                sm_module->reduction_tree[i].children_ranks=NULL;
            }
        }
        free(sm_module->reduction_tree);
        sm_module->reduction_tree=NULL;
    }

    /* gvm Leak FIX */
    if (sm_module->fanout_node.children_ranks){
        free(sm_module->fanout_node.children_ranks);
        sm_module->fanout_node.children_ranks = NULL;
    }

    if (sm_module->fanin_node.children_ranks){
        free(sm_module->fanin_node.children_ranks);
        sm_module->fanin_node.children_ranks = NULL;
    }

    /* colls_no_user_data resrouces */
    if(sm_module->colls_no_user_data.ctl_buffs_mgmt){
        free(sm_module->colls_no_user_data.ctl_buffs_mgmt);
        sm_module->colls_no_user_data.ctl_buffs_mgmt=NULL;
    }
    if(sm_module->colls_no_user_data.ctl_buffs){
        free(sm_module->colls_no_user_data.ctl_buffs);
        sm_module->colls_no_user_data.ctl_buffs=NULL;
    }

    /* colls_with_user_data resrouces */
    /*
     *debug print */
    /*
      fprintf(stderr,"AAA colls_with_user_data.ctl_buffs %p \n",
      sm_module->colls_with_user_data.ctl_buffs_mgmt);
      fflush(stderr);
      end debug */

    if(sm_module->colls_with_user_data.ctl_buffs_mgmt){
        free(sm_module->colls_with_user_data.ctl_buffs_mgmt);
        sm_module->colls_with_user_data.ctl_buffs_mgmt=NULL;
    }
    if(sm_module->colls_with_user_data.ctl_buffs){
        free(sm_module->colls_with_user_data.ctl_buffs);
        sm_module->colls_with_user_data.ctl_buffs=NULL;
    }

    if(sm_module->shared_memory_scratch_space) {
        free(sm_module->shared_memory_scratch_space);
        sm_module->shared_memory_scratch_space=NULL;
    }

#if 1
    if(sm_module->scatter_kary_tree) {
        for(i=0 ; i < sm_module->super.size_of_subgroup ; i++ ) {
            if(0 < sm_module->scatter_kary_tree[i].n_children) {
                free(sm_module->scatter_kary_tree[i].children_ranks);
                sm_module->scatter_kary_tree[i].children_ranks=NULL;
            }
        }
        free(sm_module->scatter_kary_tree);
    }
#endif

    if(NULL != sm_module->super.list_n_connected ){
        free(sm_module->super.list_n_connected);
        sm_module->super.list_n_connected = NULL;
    }

    /* free the k-nomial allgather tree here */



    /* done */
}

static void bcol_basesmuma_set_small_msg_thresholds(struct mca_bcol_base_module_t *super)
{
    mca_bcol_basesmuma_module_t *basesmuma_module =
        (mca_bcol_basesmuma_module_t *) super;

    size_t basesmuma_offset = bcol_basesmuma_data_offset_calc(basesmuma_module);

    /* Set the Allreduce threshold, for Basesmuma it equals to ML buffer size - data offset */
    super->small_message_thresholds[BCOL_ALLREDUCE] =
        basesmuma_module->ml_mem.ml_mem_desc->size_buffer - basesmuma_offset;

    /* Set the Bcast threshold, for Basesmuma it equals to ML buffer size - data offset */
    super->small_message_thresholds[BCOL_BCAST] =
        basesmuma_module->ml_mem.ml_mem_desc->size_buffer - basesmuma_offset;

    /* Set the Gather threshold, for Basesmuma it equals to ML buffer size - data offset */
    super->small_message_thresholds[BCOL_GATHER] =
        (basesmuma_module->ml_mem.ml_mem_desc->size_buffer - basesmuma_offset) /
        ompi_comm_size(basesmuma_module->super.sbgp_partner_module->group_comm);

    /* Set the ALLgather threshold, for Basesmuma it equals to ML buffer size - data offset */
    super->small_message_thresholds[BCOL_ALLGATHER] =
        (basesmuma_module->ml_mem.ml_mem_desc->size_buffer - basesmuma_offset) /
        ompi_comm_size(basesmuma_module->super.sbgp_partner_module->group_comm);

    /* Set the Reduce threshold, for Basesmuma it equals to ML buffer size - data offset */
    super->small_message_thresholds[BCOL_REDUCE] =
        basesmuma_module->ml_mem.ml_mem_desc->size_buffer - basesmuma_offset;

    /* Set the Scatter threshold, for Basesmuma it equals to ML buffer size - data offset */
    super->small_message_thresholds[BCOL_SCATTER] =
        basesmuma_module->ml_mem.ml_mem_desc->size_buffer - basesmuma_offset;
}

/* setup memory management and collective routines */

static void load_func(mca_bcol_base_module_t *super)
{
    int fnc;

    /* Loading memory management and collective functions */

    for (fnc = 0; fnc < BCOL_NUM_OF_FUNCTIONS; fnc++) {
        super->bcol_function_table[fnc] = NULL;
    }

    /*super->bcol_function_table[BCOL_BARRIER] = bcol_basesmuma_recursive_double_barrier;*/

#ifdef __PORTALS_AVAIL__
    super->bcol_function_table[BCOL_BCAST] = bcol_basesmuma_lmsg_scatter_allgather_portals_bcast;
    /* super->bcol_function_table[BCOL_BCAST]   =
       bcol_basesmuma_lmsg_bcast_k_nomial_anyroot; */
#endif

    /*super->bcol_function_table[BCOL_BCAST]   = bcol_basesmuma_bcast;*/
    /*super->bcol_function_table[BCOL_BCAST]   = bcol_basesmuma_binary_scatter_allgather_segment;*/
    /*super->bcol_function_table[BCOL_BCAST]    =  bcol_basesmuma_bcast_k_nomial_anyroot;*/
    super->bcol_function_table[BCOL_BCAST]    =  bcol_basesmuma_bcast;
#ifdef __PORTALS_AVAIL__
    super->bcol_function_table[BCOL_BCAST] =
        bcol_basesmuma_lmsg_scatter_allgather_portals_bcast;
#endif
    /* super->bcol_function_table[BCOL_ALLREDUCE]  = bcol_basesmuma_allreduce_intra_fanin_fanout; */
    super->bcol_function_table[BCOL_ALLREDUCE]  = bcol_basesmuma_allreduce_intra_recursive_doubling;
    super->bcol_function_table[BCOL_REDUCE]  = bcol_basesmuma_reduce_intra_fanin_old;
    /* memory management */
    super->bcol_memory_init                  = bcol_basesmuma_bank_init_opti;

    super->k_nomial_tree                     = bcol_basesmuma_setup_knomial_tree;

    /* Set thresholds */
    super->set_small_msg_thresholds = bcol_basesmuma_set_small_msg_thresholds;
}

static void load_func_with_choices(mca_bcol_base_module_t *super)
{
    int fnc;

    /* Loading memory management and collective functions */

    for (fnc=0; fnc < BCOL_NUM_OF_FUNCTIONS; fnc++) {
        super->bcol_function_init_table[fnc] = NULL;
    }

    super->bcol_function_init_table[BCOL_FANIN]  = bcol_basesmuma_fanin_init;
    super->bcol_function_init_table[BCOL_FANOUT] = bcol_basesmuma_fanout_init;
    super->bcol_function_init_table[BCOL_BARRIER] = bcol_basesmuma_barrier_init;

    super->bcol_function_init_table[BCOL_BCAST]  = bcol_basesmuma_bcast_init;
    super->bcol_function_init_table[BCOL_ALLREDUCE]  = bcol_basesmuma_allreduce_init;
    super->bcol_function_init_table[BCOL_REDUCE]  = bcol_basesmuma_reduce_init;
    super->bcol_function_init_table[BCOL_GATHER]  = bcol_basesmuma_gather_init;
    super->bcol_function_init_table[BCOL_ALLGATHER]  = bcol_basesmuma_allgather_init;
    super->bcol_function_init_table[BCOL_SYNC]  = bcol_basesmuma_memsync_init;
    /* memory management */
    super->bcol_memory_init                  = bcol_basesmuma_bank_init_opti;

    super->k_nomial_tree                     = bcol_basesmuma_setup_knomial_tree;

}

static int load_recursive_knomial_info(mca_bcol_basesmuma_module_t
                                       *sm_module)
{
    int rc = OMPI_SUCCESS;
    rc = netpatterns_setup_recursive_knomial_tree_node(sm_module->super.sbgp_partner_module->group_size,
                                                       sm_module->super.sbgp_partner_module->my_index,
                                                       mca_bcol_basesmuma_component.k_nomial_radix,
                                                       &sm_module->knomial_exchange_tree);
    return rc;
}


int bcol_basesmuma_setup_knomial_tree(mca_bcol_base_module_t *super)
{
    mca_bcol_basesmuma_module_t *sm_module = (mca_bcol_basesmuma_module_t *) super;

    return netpatterns_setup_recursive_knomial_allgather_tree_node(sm_module->super.sbgp_partner_module->group_size,
                                                                   sm_module->super.sbgp_partner_module->my_index,
                                                                   mca_bcol_basesmuma_component.k_nomial_radix,
                                                                   super->list_n_connected,
                                                                   &sm_module->knomial_allgather_tree);
}




/* query to see if the module is available for use on the given
 * communicator, and if so, what it's priority is.  This is where
 * the backing shared-memory file is created.
 */
mca_bcol_base_module_t **
mca_bcol_basesmuma_comm_query(mca_sbgp_base_module_t *module, int *num_modules)
{
    /* local variables */
    mca_bcol_base_module_t **sm_modules = NULL;
    mca_bcol_basesmuma_module_t *sm_module;
    bcol_basesmuma_registration_data_t *sm_reg_data;
    int ret, my_rank, name_length;
    char *name;
    int i;

    int bcast_radix;

    mca_bcol_basesmuma_component_t *cs = &mca_bcol_basesmuma_component;
    /*mca_base_component_list_item_t *hdl_cli = NULL;*/
    /*int hdl_num;*/

    /* at this point I think there is only a sinle shared
       memory bcol that we need to be concerned with */

    /* No group, no modules */
    if (OPAL_UNLIKELY(NULL == module)) {
        return NULL;
    }

    /* allocate and initialize an sm_bcol module */
    sm_module = OBJ_NEW(mca_bcol_basesmuma_module_t);

    /* set the subgroup */
    sm_module->super.sbgp_partner_module=module;

    (*num_modules)=1;
    cs->super.n_net_contexts = *num_modules;
    sm_modules = (mca_bcol_base_module_t **) malloc((cs->super.n_net_contexts)*
                                                    sizeof(mca_bcol_base_module_t *));

    if( !sm_modules ) {
        fprintf(stderr,"In base_bcol_masesmuma_setup_library_buffers failed to allocate memory for sm_modules\n");
        fflush(stderr);
        return NULL;
    }

    sm_module->reduction_tree = NULL;
    sm_module->fanout_read_tree = NULL;

    ret=netpatterns_setup_recursive_doubling_tree_node(
                                                       module->group_size,module->my_index,
                                                       &(sm_module->recursive_doubling_tree));
    if(OMPI_SUCCESS != ret) {
        fprintf(stderr,"Error setting up recursive_doubling_tree \n");
        fflush(stderr);
        return NULL;
    }

    /* setup the fanin tree - this is used only as part of a hierarchical
     *   barrier, so will set this up with rank 0 as the root */
    my_rank=module->my_index;
    ret=netpatterns_setup_narray_tree(cs->radix_fanin,
                                      my_rank,module->group_size,&(sm_module->fanin_node));
    if(OMPI_SUCCESS != ret) {
        fprintf(stderr,"Error setting up fanin tree \n");
        fflush(stderr);
        return NULL;
    }

    /* setup the fanout tree - this is used only as part of a hierarchical
     *   barrier, so will set this up with rank 0 as the root */
    ret=netpatterns_setup_narray_tree(cs->radix_fanout,
                                      my_rank,module->group_size,&(sm_module->fanout_node));
    if(OMPI_SUCCESS != ret) {
        fprintf(stderr,"Error setting up fanout tree \n");
        fflush(stderr);
        return NULL;
    }

    /*
     * Setup the broadcast tree - this is used only as part of a hierarchical
     * bcast, so will set this up with rank 0 as the root.
     */

    /* set the radix of the bcast tree */
    bcast_radix = cs->radix_read_tree;

    /* initialize fan-out read tree */
    sm_module->fanout_read_tree=(netpatterns_tree_node_t*) malloc(
                                                                  sizeof(netpatterns_tree_node_t)*module->group_size);
    if( NULL == sm_module->fanout_read_tree ) {
        goto Error;
    }

    for(i = 0; i < module->group_size; i++){
        ret = netpatterns_setup_narray_tree(bcast_radix,
                                            i, module->group_size, &(sm_module->fanout_read_tree[i]));
        if(OMPI_SUCCESS != ret) {
            goto Error;
        }
    }

    ret = load_recursive_knomial_info(sm_module);
    if (OMPI_SUCCESS != ret) {
        BASESMUMA_VERBOSE(10, ("Failed to load recursive knomial tree"));
        goto Error;
    }

    /* Allocate offsets array for lmsg reduce */
    ret = alloc_lmsg_reduce_offsets_array(sm_module);
    if (OMPI_SUCCESS != ret) {
        BASESMUMA_VERBOSE(10, ("Failed to allocate reduce offsets array"));
        goto Error;
    }

    /* initialize reduction tree */
    sm_module->reduction_tree=(netpatterns_tree_node_t *) malloc(
                                                                 sizeof(netpatterns_tree_node_t )*module->group_size);
    if( NULL == sm_module->reduction_tree ) {
        goto Error;
    }

    ret=netpatterns_setup_multinomial_tree(
                                           cs->order_reduction_tree,module->group_size,
                                           sm_module->reduction_tree);
    if( MPI_SUCCESS != ret ) {
        goto Error;
    }

    /* get largest power of k for given group size */
    sm_module->pow_k_levels = pow_sm_k(cs->k_nomial_radix,
                                       sm_module->super.sbgp_partner_module->group_size,
                                       &(sm_module->pow_k));

    /* get largest power of 2 for a given group size
     * used in scatter allgather
     */
    sm_module->pow_2_levels = pow_sm_k(2,
                                       sm_module->super.sbgp_partner_module->group_size,
                                       &(sm_module->pow_2));

    /*
     * setup scatter data
     */
    sm_module->scatter_kary_radix=cs->scatter_kary_radix;
    sm_module->scatter_kary_tree=NULL;
    ret=netpatterns_setup_narray_tree_contigous_ranks(
                                                      sm_module->scatter_kary_radix,
                                                      sm_module->super.sbgp_partner_module->group_size,
                                                      &(sm_module->scatter_kary_tree));
    if(OMPI_SUCCESS != ret) {
        fprintf(stderr,"In base_bcol_masesmuma_setup_library_buffers and scatter k-ary tree setup failed \n");
        fflush(stderr);
        return NULL;
    }

    /* setup the module shared memory management */
    ret=base_bcol_basesmuma_setup_library_buffers(sm_module, cs);

    if(OMPI_SUCCESS != ret) {
        fprintf(stderr,"In base_bcol_masesmuma_setup_library_buffers and mpool was not successfully setup!\n");
        fflush(stderr);
        return NULL;
    }

    /* setup the collectives and memory management */

    /* check to see whether or not the mpool has been inited */
    /* allocate some space for the network contexts */
    if(!cs->mpool_inited) {
        /* if it's empty, then fill it for first time */
        cs->super.network_contexts = (bcol_base_network_context_t **)
            malloc((cs->super.n_net_contexts)*
                   sizeof(bcol_base_network_context_t *));
        /* you need to do some basic setup - define the file name,
         * set data seg alignment and size of cntl structure in sm
         * file.
         */
        /* give the payload sm file a name */
        name_length=asprintf(&name,
                             "%s"OPAL_PATH_SEP"0%s%0d",
                             ompi_process_info.job_session_dir,
                             cs->payload_base_fname,
                             (int)getpid());
        if( 0 > name_length ) {
            fprintf(stderr,"Failed to assign the shared memory payload file a name\n");
            fflush(stderr);
            return NULL;
        }
        /* make sure name is not too long */
        if ( OPAL_PATH_MAX < (name_length-1) ) {
            fprintf(stderr,"Shared memory file name is too long!\n");
            fflush(stderr);
            return NULL;
        }
        /* set the name and alignment characteristics */
        sm_reg_data = (bcol_basesmuma_registration_data_t *) malloc(
                                                                    sizeof(bcol_basesmuma_registration_data_t));
        sm_reg_data->file_name = name;

        sm_reg_data->data_seg_alignment = getpagesize();
        sm_reg_data->size_ctl_structure = 0;
        cs->super.network_contexts[0] = (bcol_base_network_context_t *)
            malloc(sizeof(bcol_base_network_context_t));
        cs->super.network_contexts[0]->context_data =
            (void *) sm_reg_data;
        cs->super.network_contexts[0]->
            register_memory_fn = mca_bcol_basesmuma_register_sm;
        cs->super.network_contexts[0]->
            deregister_memory_fn = mca_bcol_basesmuma_deregister_sm;
        sm_module->super.network_context = cs->super.network_contexts[0];
    } else {

        sm_module->super.network_context = cs->super.network_contexts[0];
    }

    /* Set the header size */
    sm_module->super.header_size = sizeof(mca_bcol_basesmuma_header_t);

    /*initialize the hdl module if it's to be enabled*/
#if 0
    if (module->use_hdl) {
        sm_module->super.use_hdl = module->use_hdl;
        hdl_cli = (mca_base_component_list_item_t *)
            opal_list_get_first(&mca_hdl_base_components_in_use);
        sm_module->hdl_module = ((mca_hdl_base_component_t*)
                                 hdl_cli->cli_component)->hdl_comm_query(sm_module, &hdl_num);
        if (1 != hdl_num || sm_module->hdl_module == NULL) {
            ML_ERROR(("hdl modules are not successfully initialized!\n"));
            goto Error;
        }
    } else {
        sm_module->hdl_module = NULL;
    }
#else
    sm_module->hdl_module = NULL;
#endif


    /* collective setup */
    load_func(&(sm_module->super));
    load_func_with_choices(&(sm_module->super));

    /*
     * This initializes all collective algorithms
     */

    ret = mca_bcol_base_bcol_fns_table_init(&(sm_module->super));

    if (OMPI_SUCCESS != ret) {

        goto Error;
    }

    sm_module->super.supported_mode = 0;

    /* NTH: this is not set anywhere on the trunk as of 08/13/13 */
#if 0
    if (module->use_hdl) {
        sm_module->super.supported_mode = MCA_BCOL_BASE_ZERO_COPY;
    }
#endif

    /* Initializes portals library required for basesmuma large message */
#ifdef __PORTALS_AVAIL__
    /* Enable zero copy mode */
    sm_module->super.supported_mode = MCA_BCOL_BASE_ZERO_COPY;

    ret = mca_bcol_basesmuma_portals_init(cs);
    if (OMPI_SUCCESS != ret) {
        return NULL;
    }

    sm_module->sg_state.phase = INIT;

    ret = PtlEQAlloc(((mca_bcol_basesmuma_portal_proc_info_t*)
                      cs->portals_info)->ni_h, MAX_PORTAL_EVENTS_IN_Q,
                     PTL_EQ_HANDLER_NONE, &sm_module->sg_state.read_eq);

    if (ret != PTL_OK) {
        BASESMUMA_VERBOSE(10,( "PtlEQAlloc() failed: %d",ret));
        return NULL;
    }

#endif
    /* blocking recursive double barrier test */
    /*
      {
      fprintf(stderr,"BBB About to hit the barrier test\n");
      fflush(stderr);
      int rc;
      bcol_function_args_t bogus;
      rc = bcol_basesmuma_rd_barrier_init(&(sm_module->super));
      rc = bcol_basesmuma_recursive_double_barrier(
      &bogus, &(sm_module->super));
      }
    */

    /* in this case we only expect a single network context.
       in the future we should loop around this */
    sm_modules[0] = &(sm_module->super);

#if 0
    /* debug  */
    /* test resource recycling */
    test_sm_module=sm_module;
    /* debug */
    fprintf(stderr," ZZZZ sn %lld \n",sm_module->squence_number_offset);
    fflush(stderr);
    /* end debug */
    test_resrouce_recycle();

    /* end debug */
#endif

    return sm_modules;

 Error:

    /* cleanup */
    if( sm_module->reduction_tree ) {
        free(sm_module->reduction_tree);
        sm_module->reduction_tree=NULL;
    }

    return NULL;
}



#if 0
/*
 * Init module on the communicator
 */
static int
basesmuma_module_enable(mca_bcol_base_module_t *module,
                        struct ompi_communicator_t *comm)
{
    /* local variables */
    char output_buffer[2*MPI_MAX_OBJECT_NAME];

    memset(&output_buffer[0],0,sizeof(output_buffer));
    snprintf(output_buffer,sizeof(output_buffer),"%s (cid %d)", comm->c_name,
             comm->c_contextid);
    opal_output_verbose(10, ompi_bcol_base_framework.framework_output,
                        "bcol:basesmuma:enable: new communicator: %s", output_buffer);

    /* All done */
    return OMPI_SUCCESS;
}
#endif

OBJ_CLASS_INSTANCE(mca_bcol_basesmuma_module_t,
                   mca_bcol_base_module_t,
                   mca_bcol_basesmuma_module_construct,
                   mca_bcol_basesmuma_module_destruct);
