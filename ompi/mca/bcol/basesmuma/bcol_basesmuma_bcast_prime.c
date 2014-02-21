/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2013 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"

#include "bcol_basesmuma_utils.h"
#include "bcol_basesmuma.h"

/* debug
 *   #include "opal/sys/timer.h"
 *
 *   extern uint64_t timers[7];
 *   end debug */

/* debug */
#include <unistd.h>
/* end debug */

/* includes shared memory optimization */

#define  BCOL_BASESMUMA_SM_PROBE(src_list, n_src, my_index, matched, src) \
  do {                                                                    \
    int j;                                                                \
    for( j = 0; j < n_src; j++) {                                         \
      parent_ctl_pointer = data_buffs[src_list[j]].ctl_struct;            \
      parent_data_pointer = data_buffs[src_list[j]].payload;              \
      if( IS_DATA_READY(parent_ctl_pointer,ready_flag,sequence_number)) { \
        src = src_list[j];                                                \
        matched = 1;                                                      \
        break;                                                            \
      }                                                                   \
    }                                                                     \
  } while(0)

/*
  #define IS_LARGE_DATA_READY(peer, my_flag, my_sequence_number) \
  (((peer)->sequence_number == (my_sequence_number) && \
  (peer)->flags[BCAST_FLAG] >= (my_flag) \
  )? true : false )
*/

/*
  #define IS_KNOWN_ROOT_DATA_READY(peer, my_flag, my_sequence_number) \
  (((peer)->sequence_number == (my_sequence_number) && \
  (peer)->flags[BCAST_FLAG][bcol_id] >= (my_flag) \
  )? true : false )
*/

#define  BCOL_BASESMUMA_SM_LARGE_MSG_PROBE(src_list, n_src, my_index, matched, src, flag_index, bcol_id) \
  do {                                                                        \
    int j;                                                                \
    for( j = 0; j < n_src; j++) {                                        \
      /* fprintf(stderr,"my_rank %d and %d\n",my_rank,1);         */        \
      if(src_list[j] != -1) {                                                \
        parent_ctl_pointer = ctl_structs[src_list[j]];                        \
        parent_data_pointer = (void *) data_buffs[src_list[j]].ctl_struct; \
        /*fprintf(stderr,"my_rank %d ready flag %d partner flag %d and %d\n",my_rank,ready_flag,parent_ctl_pointer->flag,2);    */ \
        if( IS_PEER_READY(parent_ctl_pointer,ready_flag,sequence_number, flag_index, bcol_id)) { \
          src = src_list[j];                                                \
          matched = 1;                                                        \
          index = j;                                                        \
          /*  fprintf(stderr,"found it from %d!\n",src);*/                \
          break;                                                        \
        }                                                                \
      }                                                                        \
    }                                                                        \
  } while(0)

#define K_NOMIAL_DATA_SRC(radix, my_group_index, group_size, group_root, data_src, radix_mask) \
  do {                                                                        \
    int relative_rank = (my_group_index >= group_root) ? my_group_index - group_root : \
      my_group_index - group_root + group_size;                                \
    radix_mask = 1;                                                        \
    while (radix_mask < group_size) {                                        \
      if (relative_rank % (radix * radix_mask)) {                        \
        data_src = relative_rank/(radix * radix_mask) * (radix * radix_mask) + group_root; \
        if (data_src >= group_size) data_src -= group_size;                \
        break;                                                                \
      }                                                                        \
      radix_mask *= radix;                                                \
    }                                                                        \
  } while (0)

int bcol_basesmuma_bcast_k_nomial_knownroot(bcol_function_args_t *input_args,
                                            mca_bcol_base_function_t *c_input_args)
{
  /* local variables */
  mca_bcol_basesmuma_module_t* bcol_module=
    (mca_bcol_basesmuma_module_t *)c_input_args->bcol_module;
  mca_bcol_basesmuma_component_t *cs = &mca_bcol_basesmuma_component;
  int i, matched = 0;
  int group_size;
  int my_rank;
  int leading_dim,
    buff_idx,
    idx;
  int count = input_args->count;
  struct ompi_datatype_t* dtype = input_args->dtype;
  int64_t sequence_number = input_args->sequence_num;
  int radix =
    mca_bcol_basesmuma_component.k_nomial_radix;
  int radix_mask;
  int16_t data_src = -1;

  volatile int8_t ready_flag;
  int bcol_id = (int) bcol_module->super.bcol_id;
  volatile mca_bcol_basesmuma_payload_t *data_buffs;
  volatile char* parent_data_pointer;
  volatile mca_bcol_basesmuma_header_t *parent_ctl_pointer;
  volatile mca_bcol_basesmuma_header_t *my_ctl_pointer;

  size_t pack_len = 0;
  void *data_addr = (void *)((unsigned char *)input_args->src_desc->data_addr +
                             input_args->sbuf_offset);

#if 0
  fprintf(stderr,"Entering nb-sm broadcast input_args->sbuf_offset %d \n",input_args->sbuf_offset);
  fflush(stderr);
#endif


  /* we will work only on packed data - so compute the length*/
  BASESMUMA_VERBOSE(3, ("Calling bcol_basesmuma_bcast_k_nomial_knownroot"));

  pack_len = mca_bcol_base_get_buff_length(dtype, count);
  /* Some hierarchical algorithms have data that is accumulated at each step
   * this factor accounts for this
   */
  pack_len = pack_len*input_args->hier_factor;
  buff_idx = input_args->buffer_index;

  /* Get addressing information */
  my_rank     = bcol_module->super.sbgp_partner_module->my_index;
  group_size  = bcol_module->colls_no_user_data.size_of_group;
  leading_dim = bcol_module->colls_no_user_data.size_of_group;
  idx         = SM_ARRAY_INDEX(leading_dim,buff_idx,0);
  data_buffs = (volatile mca_bcol_basesmuma_payload_t *)
    bcol_module->colls_with_user_data.data_buffs + idx;

  /* Set pointer to current proc ctrl region */
  my_ctl_pointer = data_buffs[my_rank].ctl_struct;

  /* setup resource recycling */
  BASESMUMA_HEADER_INIT(my_ctl_pointer, ready_flag, sequence_number, bcol_id);
  /* removing dependence on sequence number */
  /* I believe this is resolved now with the signaling flags */
  /*
    ready_temp = 1 + (int8_t) flag_offset + (int8_t) bcol_id;
    if( ready_temp >= my_ctl_pointer->flags[BCAST_FLAG][bcol_id]) {
    ready_flag = ready_temp;
    } else {
    ready_flag =  my_ctl_pointer->flags[BCAST_FLAG][bcol_id];
    }
    opal_atomic_wmb ();
    my_ctl_pointer->sequence_number = sequence_number;
  */


  /* non-blocking broadcast algorithm */

  /* If I am the root, then signal ready flag */
  if(input_args->root_flag) {
    BASESMUMA_VERBOSE(10,("I am the root of the data"));
    /*
     * signal ready flag
     */
    opal_atomic_wmb ();
    my_ctl_pointer->flags[BCAST_FLAG][bcol_id] = ready_flag;

    /* root is finished */
    goto Release;
  }


  /* Calculate source of the data */
  K_NOMIAL_DATA_SRC(radix, my_rank, group_size,
                    input_args->root_route->rank, data_src, radix_mask);


  parent_ctl_pointer = data_buffs[data_src].ctl_struct;
  parent_data_pointer = data_buffs[data_src].payload;

  for( i = 0; i < cs->num_to_probe && 0 == matched; i++) {

    if(IS_PEER_READY(parent_ctl_pointer,ready_flag,sequence_number, BCAST_FLAG, bcol_id)) {
      matched = 1;
      break;
    }
  }

  /* If not matched, then hop out and put me on progress list */
  if(0 == matched ) {
    BASESMUMA_VERBOSE(10,("Shared memory probe didn't find a match"));
    return BCOL_FN_NOT_STARTED;
  }

  /* else, we found our root within the group ... */
  BASESMUMA_VERBOSE(10,("Shared memory probe was matched, the root is %d", data_src));

  /* copy the data */
  memcpy(data_addr, (void *) parent_data_pointer, pack_len);
  /* set the memory barrier to ensure completion */
  opal_atomic_wmb ();
  /* signal that I am done */
  my_ctl_pointer->flags[BCAST_FLAG][bcol_id] = ready_flag;


 Release:
  my_ctl_pointer->starting_flag_value[bcol_id]++;
  return BCOL_FN_COMPLETE;
}


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
int bcol_basesmuma_bcast_k_nomial_anyroot(bcol_function_args_t *input_args,
                                          mca_bcol_base_function_t *c_input_args)
{
  /* local variables */
  mca_bcol_basesmuma_module_t* bcol_module=
    (mca_bcol_basesmuma_module_t *)c_input_args->bcol_module;
  mca_bcol_basesmuma_component_t *cs = &mca_bcol_basesmuma_component;
  int i;
  int group_size;
  int my_rank;
  int leading_dim, buff_idx, idx;
  int count=input_args->count;
  struct ompi_datatype_t* dtype=input_args->dtype;
  int64_t sequence_number=input_args->sequence_num;
  int radix = cs->k_nomial_radix;
  int radix_mask;
  int relative_rank;
  int pow_k_group_size;

  volatile int8_t ready_flag;
  int bcol_id = (int) bcol_module->super.bcol_id;
  volatile mca_bcol_basesmuma_payload_t *data_buffs;
  volatile void* parent_data_pointer;

  volatile mca_bcol_basesmuma_header_t *child_ctl_pointer;
  volatile mca_bcol_basesmuma_header_t *my_ctl_pointer;

  size_t pack_len = 0;
  void *data_addr = (void *)((unsigned char *)input_args->src_desc->data_addr +
                             input_args->sbuf_offset);

#if 0
  fprintf(stderr,"Entering nb-sm broadcast input_args->sbuf_offset %d \n",input_args->sbuf_offset);
  fflush(stderr);
#endif



  /* we will work only on packed data - so compute the length*/
  pack_len = mca_bcol_base_get_buff_length(dtype, count);

  buff_idx = input_args->buffer_index;

  /* Get addressing information */
  my_rank = bcol_module->super.sbgp_partner_module->my_index;
  group_size = bcol_module->colls_no_user_data.size_of_group;
  leading_dim=bcol_module->colls_no_user_data.size_of_group;
  idx=SM_ARRAY_INDEX(leading_dim,buff_idx,0);

  /* get pow_k_levels and pow_k_group_size */
  pow_k_group_size = bcol_module->pow_k;


  data_buffs=(volatile mca_bcol_basesmuma_payload_t *)
    bcol_module->colls_with_user_data.data_buffs+idx;

  /* Set pointer to current proc ctrl region */
  my_ctl_pointer = data_buffs[my_rank].ctl_struct;

  BASESMUMA_HEADER_INIT(my_ctl_pointer, ready_flag, sequence_number, bcol_id);

  /* non-blocking broadcast algorithm */

  /* If I am the root, then signal ready flag */
  if(input_args->root_flag) {

    BASESMUMA_VERBOSE(10,("I am the root of the data"));
    /*
     * set the radix_mask */
    radix_mask = pow_k_group_size;
    /* send to children */
    opal_atomic_wmb ();
    BASESMUMA_K_NOMIAL_SEND_CHILDREN(radix_mask,
                                     radix,0,
                                     my_rank,group_size, ready_flag);
    /* root is finished */
    goto Release;
  }

  /* If I am not the root, then poll on possible "senders'" control structs */
  for( i = 0; i < cs->num_to_probe; i++) {

    if( ready_flag == my_ctl_pointer->flags[BCAST_FLAG][bcol_id]) {

      /* else, we found our root within the group ... */
      parent_data_pointer = data_buffs[my_ctl_pointer->src].payload;
      BASESMUMA_VERBOSE(5,("%d found it from %d \n",my_rank,my_ctl_pointer->src));
      /* memcopy the data */
      memcpy(data_addr, (void *) parent_data_pointer, pack_len);
      /* compute my relative rank */
      relative_rank = (my_rank - my_ctl_pointer->src) < 0 ? my_rank -
        my_ctl_pointer->src + group_size : my_rank - my_ctl_pointer->src;

      /* compute my radix mask */
      radix_mask = 1;
      while(radix_mask < group_size ){
        if( 0 != relative_rank % (radix*radix_mask)) {
          /* found it */
          break;
        }
        radix_mask *= radix;
      }
      /* go one step back */
      radix_mask /= radix;

      /* send to children */
      opal_atomic_wmb ();
      BASESMUMA_K_NOMIAL_SEND_CHILDREN(radix_mask,
                                       radix, relative_rank,
                                       my_rank, group_size, ready_flag);
      /* bail */

      goto Release;
    }

  }



  /* If not matched, then hop out and put me on progress list */
  BASESMUMA_VERBOSE(10,("Shared memory probe didn't find a match"));
  /*fprintf(stderr,"bcol_id %d Not started\n",bcol_id);*/
  return BCOL_FN_NOT_STARTED;



 Release:


  my_ctl_pointer->starting_flag_value[bcol_id]++;

  return BCOL_FN_COMPLETE;
}


/* non-blocking binary scatter allgather anyroot algorithm for large data
 * broadcast
 */


#if 0
/* prototype code for shared memory scatter/allgather algorithm. Signaling scheme
 * works, should be used as a reference for other types of shared memory scatter/allgather
 * algorithms.
 */
int bcol_basesmuma_binary_scatter_allgather_segment(bcol_function_args_t *input_args,
                                                    mca_bcol_base_function_t *c_input_args)
{

  /* local variables */
  int i, j;
  int length;
  int start;
  int my_rank, parent_rank;
  int partner;
  int src = -1;
  int matched = 0;
  int group_size;
  int first_instance=0;
  int leading_dim, buff_idx, idx;
  int64_t sequence_number=input_args->sequence_num;

  int64_t ready_flag;
  int64_t local_offset;

  int flag_offset;
  int pow_2, pow_2_levels;
  int index = -1;

  mca_bcol_basesmuma_component_t *cs = &mca_bcol_basesmuma_component;
  mca_bcol_basesmuma_module_t *bcol_module =
    (mca_bcol_basesmuma_module_t *) c_input_args->bcol_module;
  /* use the old control structs for large messages,
   * otherwise we will destroy the shared memory
   * optimization
   */
  mca_bcol_basesmuma_ctl_struct_t **ctl_structs;
  mca_bcol_basesmuma_ctl_struct_t  *my_ctl_pointer;
  mca_bcol_basesmuma_ctl_struct_t  *parent_ctl_pointer; /* binomial fanout */
  mca_bcol_basesmuma_ctl_struct_t  *partner_ctl_pointer; /* recursive double */

  /* for now, we use the payload buffer for single fragment */
  volatile mca_bcol_basesmuma_payload_t *data_buffs;
  volatile void *parent_data_pointer; /* binomial scatter */
  volatile void *partner_data_pointer;  /* recursive double */

  uint32_t fragment_size;  /* ml buffer size for now */

  /* we will transfer the entire buffer,
   * so start at the base address of the ml buffer
   */
  void *data_addr = (void *) ((unsigned char *) input_args->src_desc->base_data_addr);
#if 0
  fprintf(stderr,"AAA Entering nb-sm large msg broadcast input_args->frag_size %d \n",input_args->frag_size);
  fflush(stderr);
#endif

  buff_idx = input_args->src_desc->buffer_index;

  group_size = bcol_module->colls_no_user_data.size_of_group;
  leading_dim=bcol_module->colls_no_user_data.size_of_group;

  /* get the largest power of two that is smaller than
   * or equal to the group size
   */
  pow_2_levels = bcol_module->pow_2_levels;
  pow_2 = bcol_module->pow_2;

  /* get the fragment size
   */

  /* still just the size of the entire buffer */
  fragment_size = input_args->buffer_size;
  idx=SM_ARRAY_INDEX(leading_dim,buff_idx,0);
  my_rank = bcol_module->super.sbgp_partner_module->my_index;


  /* grab the control structs */
  ctl_structs = (mca_bcol_basesmuma_ctl_struct_t **)
    bcol_module->colls_with_user_data.ctl_buffs+idx;

  /* grab the data buffs */
  data_buffs = (mca_bcol_basesmuma_payload_t *)
    bcol_module->colls_with_user_data.data_buffs+idx;

  my_ctl_pointer = ctl_structs[my_rank];

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

  /* increment the starting flag by one and return */
  flag_offset = my_ctl_pointer->starting_flag_value;
  ready_flag = flag_offset + sequence_number + 1;

  my_ctl_pointer->sequence_number = sequence_number;

  /* am I the root */
  if(input_args->root_flag) {
    /* if I've already been here, then
     * hop down to the allgather
     */
    if(ALLGATHER == my_ctl_pointer->status) {
      goto Allgather;
    }
    BASESMUMA_VERBOSE(10,("I am the root of the data"));
    /* debug print */
    /*fprintf(stderr,"I am the root %d\n",my_rank);*/
    /*
     * signal ready flag
     */
    /* set the offset into the buffer */
    my_ctl_pointer->offset = 0;
    /* how many children do I have */
    my_ctl_pointer->n_sends = pow_2_levels;
    /* my data length */
    my_ctl_pointer->length = fragment_size;

    /* important that these be set before my children
     * see the ready flag raised
     */
    opal_atomic_wmb ();
    my_ctl_pointer->flag = ready_flag;

    /* root is finished */
    if( my_rank < pow_2 ) {
      /* if I'm in the power of two group,
       * then goto the allgather
       */
      my_ctl_pointer->status = ALLGATHER;
      goto Allgather;

    } else {

      /* if I'm not, then I'm done and release */
      goto Release;
    }

  }

  /* what phase am I participating in
   */
  switch(my_ctl_pointer->status) {

  case SCATTER:
    goto Scatter;
    break;

  case ALLGATHER:
    goto Allgather;
    break;

  case EXTRA_RANK:
    goto Extra;
    break;

  default:
    break;
  }


 Extra:
  /* am I part of the non-power-of-2 group */
  if( my_rank >= pow_2 ) {
    /* find parent to copy from */
    parent_rank = my_rank&(pow_2-1);
    parent_ctl_pointer = ctl_structs[parent_rank];
    /* start at the base */
    parent_data_pointer = (void *) data_buffs[parent_rank].ctl_struct;

    /* now, I need to do some arithmetic to
     * arrive at the value everyone else does
     * when they have completed the algorithm
     */

    /* compute ready flag value to poll on */
    ready_flag = ready_flag + pow_2_levels;

    /* start to poll */
    for( i = 0; i< cs->num_to_probe; i++) {
      if(IS_LARGE_DATA_READY(parent_ctl_pointer,ready_flag, sequence_number)) {
        /* copy the data and bail */
        memcpy(data_addr,(void *)parent_data_pointer,fragment_size);
        goto Release;
      }
      /*
         else {
         opal_progress();
         }
      */
    }
    my_ctl_pointer->status = EXTRA_RANK;

    /* hop out and put me onto a progress queue */
    return BCOL_FN_NOT_STARTED;
  }

 Scatter:

  /* on first entry, compute the list of possible sources */
  if( NULL == my_ctl_pointer->src_ptr ) {
    my_ctl_pointer->src_ptr = (int *) malloc(sizeof(int)*(pow_2_levels+1));

    for( i = 0; i < pow_2_levels; i++) {
      my_ctl_pointer->src_ptr[i] = my_rank ^ (1<<i);
    }
    /* am I participating in the non-power of two */
    if((my_rank+pow_2) < group_size) {
      /* extra rank that I'm paired with */
      my_ctl_pointer->src_ptr[i] = my_rank + pow_2;
    } else {
      /* no extra rank to worry about */
      my_ctl_pointer->src_ptr[i] = -1;
    }
  }

  /* If I am not the root, then poll on possible "senders'" control structs */
  for( i = 0; i < cs->num_to_probe && 0 == matched; i++) {

    /* Shared memory iprobe */
    BCOL_BASESMUMA_SM_LARGE_MSG_PROBE(my_ctl_pointer->src_ptr, pow_2_levels+1,
                                      my_rank, matched, src);
  }

  /* If not matched, then hop out and put me on progress list */
  if(0 == matched ) {

    BASESMUMA_VERBOSE(10,("Shared memory probe didn't find a match"));

    my_ctl_pointer->status = SCATTER;
    return BCOL_FN_NOT_STARTED;

  } else if ( src >= pow_2 ){

    /* If matched from an extra rank, then get the whole message from partner */
    memcpy((void *) data_addr, (void *) parent_data_pointer,
           parent_ctl_pointer->length);

    /* now I am the psuedo-root in the power-of-two group */
    my_ctl_pointer->offset = 0;
    my_ctl_pointer->length = parent_ctl_pointer->length;
    my_ctl_pointer->n_sends = parent_ctl_pointer->n_sends;

    /* set the memory barrier */
    opal_atomic_wmb ();

    /* fire the ready flag */
    my_ctl_pointer->flag = ready_flag;
    my_ctl_pointer->status = ALLGATHER;
    /* go to the allgather */
    goto Allgather;
  }


  /* we need to see whether this is really
   * who we are looking for
   */
  for( i = 0; i < parent_ctl_pointer->n_sends; i++) {
    /* debug print */
    /*
        fprintf(stderr,"I am %d checking on a hit from %d with n_sends %d\n",my_rank,src,parent_ctl_pointer->n_sends);
        fflush(stderr);
    */
    /* end debug */
    if( my_rank == (src^(1<<i))) {

      /* we found our root within the group ... */
      BASESMUMA_VERBOSE(10,("Shared memory probe was matched, the root is %d", src));
      /* this is who I've been looking for */
      my_ctl_pointer->n_sends = i;

      if ( i > 0) {
        /* compute the size of the chunk to copy */
        length = (parent_ctl_pointer->length)/
          (1<<(parent_ctl_pointer->n_sends - my_ctl_pointer->n_sends));
        my_ctl_pointer->length = length;
        my_ctl_pointer->offset =
          parent_ctl_pointer->offset+length;

        /*fprintf(stderr,"%d's offset %d and length %d \n",my_rank,my_ctl_pointer->offset,length);*/

        /* now we can copy the data */
        memcpy((void *) ((uint64_t) data_addr+my_ctl_pointer->offset),
               (void *) ((uint64_t) parent_data_pointer+(uint64_t) parent_ctl_pointer->offset +
                         (uint64_t) length),
               (size_t)length);
      } else {
        /* this "trick" takes care of the first level
         * of recurssive doubling
         */
        length = parent_ctl_pointer->length/
          (1<<(parent_ctl_pointer->n_sends - 1));
        my_ctl_pointer->length = length;
        my_ctl_pointer->offset = parent_ctl_pointer->offset;

        /*fprintf(stderr,"%d's offset %d and length %d\n",my_rank,my_ctl_pointer->offset,length);*/
        /* now we can copy the data */
        memcpy((void *) ((uint64_t) data_addr+my_ctl_pointer->offset),
               (void *) ((uint64_t) parent_data_pointer+(uint64_t) my_ctl_pointer->offset),
               (size_t)length);
      }
      /* set the memory barrier to ensure completion */
      opal_atomic_wmb ();
      /* signal that I am done */
      my_ctl_pointer->flag = ready_flag;
      /* set my status */
      my_ctl_pointer->status = ALLGATHER;
      /* time for allgather phase */
      goto Allgather;
    }

  }

  /* this is not who we are looking for,
   * mark as false positive so we don't
   * poll here again
   */
  my_ctl_pointer->src_ptr[index] = -1;
  /* probably we should jump out and put onto progress list */
  my_ctl_pointer->status = SCATTER;
  return BCOL_FN_NOT_STARTED;

 Allgather:

  /* zip it back up - we have already taken care of first level */
  /* needed for non-blocking conditional */
  matched = 0;

  /* get my local_offset */
  local_offset = my_ctl_pointer->offset;

  /* bump the ready flag */
  ready_flag++;

  /* first level of zip up */
  length = 2*fragment_size/pow_2;

  /* first level of zip-up
   * already includes first level of
   * recursive doubling
   */
  start = 1;

  /* for non-blocking, check to see if I need to reset the state */
  if(my_ctl_pointer->flag >= ready_flag) {
    /* then reset the state */
    ready_flag = my_ctl_pointer->flag;
    start = my_ctl_pointer->start;
    /* get the local offset */
    local_offset = my_ctl_pointer->offset_zip;
    /* compute the correct length */
    length = length*(1<<(start - 1));
    /* careful! skip over the opal_atomic_wmb () to avoid the
     * cost on every re-entry
     */
    goto Loop;
  }


  opal_atomic_wmb ();
  /* I am ready, set the flag */
  my_ctl_pointer->flag = ready_flag;

 Loop:

  for( i = start; i < pow_2_levels; i++) {
    /* get my partner for this level */
    partner = my_rank^(1<<i);
    partner_ctl_pointer = ctl_structs[partner];
    partner_data_pointer = (void *) data_buffs[partner].ctl_struct;

    /* is data ready */
    for( j = 0; j < cs->num_to_probe && matched == 0; j++) {
      if(IS_LARGE_DATA_READY(partner_ctl_pointer, ready_flag, sequence_number)) {

        /* debug prints
           fprintf(stderr,"666 I am %d and sequence num is %d partner is %d ready_flag %d parent ready_flag %d buff_idx %d partner_offset %d\n",
           my_rank,sequence_number,partner, ready_flag,partner_ctl_pointer->flag,buff_idx,partner_ctl_pointer->offset);
        */
        /* debug print */
#if 0
        fprintf(stderr,"I am %d and sequence num is %d partner is %d ready_flag %d parent ready_flag %d buff_idx %d \n",
                my_rank,sequence_number,partner, ready_flag,parent_ctl_pointer->flag,buff_idx);
#endif
        /* end debug prints */

        assert(partner_ctl_pointer->flag >= ready_flag);
        /* found it */
        matched = 1;
        /* only copy it, if you sit at a lower level in the tree */
        if( my_ctl_pointer->n_sends <= partner_ctl_pointer->n_sends ) {

          /* calculate the local offset based on partner's remote offset */
          if( partner_ctl_pointer->offset < my_ctl_pointer->offset ) {
            /* then I'm looking "up" the tree */
            local_offset -= length;
            /* debug print */
            /*fprintf(stderr,"I am %d and partner is %d partner offset %d length %d \n",my_rank,partner, local_offset,length);*/
            /* end debug */
            memcpy((void *) ((uint64_t) data_addr + (uint64_t) local_offset),
                   (void *) ((uint64_t) partner_data_pointer + (uint64_t) local_offset),
                   length);
          } else {
            /* I'm looking "down" the tree */
            local_offset += length;
            /* debug print */
            /*fprintf(stderr,"I am %d and partner is %d partner offset %d length %d \n",my_rank,partner, local_offset,length);*/
            /* end debug */
            memcpy((void *) ((uint64_t) data_addr + (uint64_t) local_offset),
                   (void *) ((uint64_t) partner_data_pointer + (uint64_t) local_offset),
                   length);
            /* reset my local offset */
            local_offset -= length;
          }

        }
        /* bump the ready flag */
        ready_flag++;
        /* ensure completion */
        opal_atomic_wmb ();

        /* fire the flag for the next level */
        my_ctl_pointer->flag = ready_flag;

        /* double the length */
        length *= 2;
      }
    }
    /* check to see what kind of progress I've made */
    if( 0 == matched ) {
      /* save state, hop out and try again later */
      my_ctl_pointer->start = i;
      /* save the local offset */
      my_ctl_pointer->offset_zip = local_offset;
      /* put in progress queue */
      return BCOL_FN_STARTED;
    }
    /* else, start next level of recursive doubling */
    matched = 0;

  }


  /* cleanup */
  if(NULL != my_ctl_pointer->src_ptr) {
    free(my_ctl_pointer->src_ptr);
    my_ctl_pointer->src_ptr = NULL;
  }

 Release:


  /* If I am the last instance, release the resource */
  /*
    if( IS_LAST_BCOL_FUNC(c_input_args)) {
    rc = bcol_basesmuma_free_buff(
    &(bcol_module->colls_with_user_data),
    sequence_number);
    }
  */

  my_ctl_pointer->starting_flag_value++;
  my_ctl_pointer->status = FINISHED;
  return BCOL_FN_COMPLETE;

}
#endif

#if 0
int mca_bcol_basesmuma_bcast_binomial_scatter_allgather(void *desc)
{
  /* local variables */
  int rc, n_frags_sent;
  uint32_t stripe_number;
  int count, count_processed;
  size_t dt_size;
  uint32_t n_data_segments_to_schedule;
  ompi_datatype_t *dtype;
  message_descriptor_t *message_descriptor;
  mca_bcol_basesmuma_module_t *bcol_module;
  int pipe_depth;


  /* get the full message descriptor */


  /* compute the number of fragments to send */


  /* start to fill the pipeline */


  return OMPI_SUCCESS;




}
#endif
