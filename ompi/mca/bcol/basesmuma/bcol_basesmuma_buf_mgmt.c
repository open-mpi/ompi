/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2013 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2013-2014 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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
#include "ompi/mca/bcol/bcol.h"
#include "ompi/mca/bcol/base/base.h"
#include "ompi/patterns/comm/coll_ops.h"

#include "opal/dss/dss.h"

#include "bcol_basesmuma.h"
/*
 * With support for nonblocking collectives, we don't have an upper
 * limit on the number of outstanding collectives per communicator.
 * Also, since we want to avoid communication to figure out which
 * buffers other ranks in the group will use, we will rely on the
 * fact that collective operations are called in the same order
 * in each process, to assign a unique ID to each collective operation.
 * We use this to create a static mapping from the index to the buffer
 * that will be used.  Also, because there is no limit to the number of
 * outstanding collective operations, we use a generation index for each
 * memory bank, so the collective will use the buffer only when the
 * correct generation of the bank is ready for use.
 */
int bcol_basesmuma_get_buff_index( sm_buffer_mgmt *buff_block,
                                   uint64_t buff_id )
{
    /* local variables */
    int memory_bank;
    uint64_t generation;
    int index=-1;


    /* get the bank index that will be used */
    memory_bank=buff_id& buff_block->mask;
    memory_bank = memory_bank SHIFT_DOWN buff_block->log2_num_buffs_per_mem_bank;

    /* get the generation of the bank this maps to */
    generation = buff_id SHIFT_DOWN (buff_block->log2_number_of_buffs);

    /* check to see if the bank is available */
    if( generation == buff_block->ctl_buffs_mgmt[memory_bank].
        bank_gen_counter ) {

        /* get the buffer index that will be returned */
        index=buff_id & buff_block->mask;

        /* no in-use counter increment, as the mapping is static, and
         * all we need to know if the number of collectives that complete */

    } else {
        /* progress communications so that resources can be freed up */
        opal_progress();
    }

    /* return */
    return index;
}

/* release the shared memory buffers
 *  buf_id is the unique ID assigned to the particular buffer
 */
int bcol_basesmuma_free_buff( sm_buffer_mgmt * buff_block,
                              uint64_t buff_id )
{
    /* local variables */
    int ret=OMPI_SUCCESS;
    int memory_bank;
    uint64_t generation;
    mca_bcol_basesmuma_component_t *cs = &mca_bcol_basesmuma_component;

    /* get the bank index that will be used */
    memory_bank=buff_id& buff_block->mask;
    memory_bank = memory_bank SHIFT_DOWN buff_block->log2_num_buffs_per_mem_bank;

    /* get the generation of the bank this maps to */
    generation = buff_id SHIFT_DOWN (buff_block->log2_number_of_buffs);

    /* the generation counter should not change until all resrouces
     *   associated with this bank have been freed.
     */
    assert(generation == buff_block->ctl_buffs_mgmt[memory_bank].bank_gen_counter);

    /*
     * increment counter of completed buffers
     */
    OPAL_THREAD_ADD32(&(buff_block->ctl_buffs_mgmt[memory_bank].n_buffs_freed),
                      1);

    /*
     * If I am the last to checkin - initiate resource recycling
     */
    if( buff_block->ctl_buffs_mgmt[memory_bank].n_buffs_freed ==
        buff_block->ctl_buffs_mgmt[memory_bank].number_of_buffers ) {

        /* Lock to ensure atomic recycling of resources */
        OPAL_THREAD_LOCK(&(buff_block->ctl_buffs_mgmt[memory_bank].mutex));

        /* make sure someone else did not already get to this */
        if( buff_block->ctl_buffs_mgmt[memory_bank].n_buffs_freed !=
            buff_block->ctl_buffs_mgmt[memory_bank].number_of_buffers ) {
            /* release lock and exit */
            OPAL_THREAD_UNLOCK(&(buff_block->ctl_buffs_mgmt[memory_bank].mutex));
        } else {
            sm_nbbar_desc_t *p_sm_nb_desc = NULL;
            /* initiate the freeing of resources.  Need to make sure the other
             * ranks in the group are also done with their resources before this
             * block is made available for use again.
             * No one else will try to allocate from this block or free back to
             * this block until the next genration counter has been incremented,
             * so will just reset the number of freed buffers to 0, so no one else
             * will try to also initialize the recycling of these resrouces
             */
            buff_block->ctl_buffs_mgmt[memory_bank].n_buffs_freed=0;

            /* Start the nonblocking barrier */
            p_sm_nb_desc = &(buff_block->ctl_buffs_mgmt[memory_bank].nb_barrier_desc);
            p_sm_nb_desc->coll_buff = buff_block;
            bcol_basesmuma_rd_nb_barrier_init_admin(p_sm_nb_desc);

            if( NB_BARRIER_DONE !=
                buff_block->ctl_buffs_mgmt[memory_bank].
                nb_barrier_desc.collective_phase) {

                opal_list_t *list=&(cs->nb_admin_barriers);
                opal_list_item_t *append_item;

                /* put this onto the progression list */
                OPAL_THREAD_LOCK(&(cs->nb_admin_barriers_mutex));
                append_item=(opal_list_item_t *)
                    &(buff_block->ctl_buffs_mgmt[memory_bank].nb_barrier_desc);
                opal_list_append(list,append_item);
                OPAL_THREAD_UNLOCK(&(cs->nb_admin_barriers_mutex));
                /* progress communications so that resources can be freed up */
                opal_progress();
            } else {
                /* mark the block as available */
                (buff_block->ctl_buffs_mgmt[memory_bank].bank_gen_counter)++;
            }

            /* get out of here */
            OPAL_THREAD_UNLOCK(&(buff_block->ctl_buffs_mgmt[memory_bank].mutex));
        }

    }

    /* return */
    return ret;
}

/*
 * Allocate buffers for storing non-blocking collective descriptions, required
 * for making code re-entrant
 *
 */
static int init_nb_coll_buff_desc(mca_bcol_basesmuma_nb_coll_buff_desc_t **desc,
                                  void *base_addr, uint32_t num_banks,
                                  uint32_t num_buffers_per_bank,
                                  uint32_t size_buffer,
                                  uint32_t header_size,
                                  int group_size,
                                  int pow_k)
{
    uint32_t i, j, ci;
    mca_bcol_basesmuma_nb_coll_buff_desc_t *tmp_desc = NULL;
    int k_nomial_radix = mca_bcol_basesmuma_component.k_nomial_radix;
    int pow_k_val = (0 == pow_k) ? 1 : pow_k;
    int num_to_alloc = (k_nomial_radix - 1) * pow_k_val * 2 + 1 ;


    *desc = (mca_bcol_basesmuma_nb_coll_buff_desc_t *)calloc(num_banks * num_buffers_per_bank, sizeof(mca_bcol_basesmuma_nb_coll_buff_desc_t));
    if (NULL == *desc) {
        return OMPI_ERROR;
    }

    tmp_desc = *desc;

    for (i = 0; i < num_banks; i++) {
        for (j = 0; j < num_buffers_per_bank; j++) {
            ci = i * num_buffers_per_bank + j;
            tmp_desc[ci].bank_index = i;
            tmp_desc[ci].buffer_index = j;
            /* *2  is for gather session  +1 for extra peer */
            tmp_desc[ci].requests = (ompi_request_t **)
                calloc(num_to_alloc, sizeof(ompi_request_t *));
            tmp_desc[ci].data_addr = (void *)
                ((unsigned char*)base_addr + ci * size_buffer + header_size);
            BASESMUMA_VERBOSE(10, ("ml memory cache setup %d %d - %p", i, j, tmp_desc[ci].data_addr));
        }
    }

    return OMPI_SUCCESS;
}


/*
 * Free buffers for storing non-blocking collective descriptions.
 *
 */
void cleanup_nb_coll_buff_desc(mca_bcol_basesmuma_nb_coll_buff_desc_t **desc,
                                  uint32_t num_banks,
                                  uint32_t num_buffers_per_bank)
{
    uint32_t ci;
    if (NULL != *desc) {
        for (ci=0; ci<num_banks*num_buffers_per_bank; ci++) {
            if (NULL != ((*desc)[ci]).requests) {
                free(((*desc)[ci]).requests);
                ((*desc))[ci].requests = NULL;
            }
        }
        free(*desc);
        *desc = NULL;
    }
}


#if 1
/* New init function used for new control scheme where we put the control
 * struct at the top of the payload buffer
 */
int bcol_basesmuma_bank_init_opti(struct mca_bcol_base_memory_block_desc_t *payload_block,
        uint32_t data_offset,
        mca_bcol_base_module_t *bcol_module,
        void *reg_data)
{
    /* assumption here is that the block has been registered with
     * sm bcol hence has been mapped by each process, need to be
     * sure that memory is mapped amongst sm peers
     */

    /* local variables */
    int ret = OMPI_SUCCESS, i, j;
    sm_buffer_mgmt *pload_mgmt;
    mca_bcol_basesmuma_component_t *cs = &mca_bcol_basesmuma_component;
    bcol_basesmuma_registration_data_t *sm_reg_data =
        (bcol_basesmuma_registration_data_t *) reg_data;
    mca_bcol_basesmuma_module_t *sm_bcol =
        (mca_bcol_basesmuma_module_t *) bcol_module;
    mca_bcol_base_memory_block_desc_t *ml_block = payload_block;
    size_t malloc_size;
    bcol_basesmuma_smcm_file_t input_file;
    int leading_dim,loop_limit,buf_id;
    unsigned char *base_ptr;
    mca_bcol_basesmuma_module_t *sm_bcol_module=
        (mca_bcol_basesmuma_module_t *)bcol_module;
    int my_idx, array_id;
    mca_bcol_basesmuma_header_t *ctl_ptr;
    void **results_array=NULL, *mem_offset;

    mca_bcol_basesmuma_local_mlmem_desc_t *ml_mem = &sm_bcol_module->ml_mem;

    /* first, we get a pointer to the payload buffer management struct */
    pload_mgmt = &(sm_bcol->colls_with_user_data);

    /* go ahead and get the header size that is cached on the payload block
     */
    sm_bcol->total_header_size = data_offset;

    /* allocate memory for pointers to mine and my peers' payload buffers
     * difference here is that now we use our new data struct
     */
    malloc_size = ml_block->num_banks*ml_block->num_buffers_per_bank*
        pload_mgmt->size_of_group *sizeof(mca_bcol_basesmuma_payload_t);
    pload_mgmt->data_buffs = (mca_bcol_basesmuma_payload_t *) malloc(malloc_size);
    if( !pload_mgmt->data_buffs) {
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit_ERROR;
    }

    /* allocate some memory to hold the offsets */
    results_array = (void **) malloc(pload_mgmt->size_of_group * sizeof (void *));
    if (NULL == results_array) {
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit_ERROR;
    }

    /* setup the input file for the shared memory connection manager */
    input_file.file_name = sm_reg_data->file_name;
    input_file.size = sm_reg_data->size;
    input_file.size_ctl_structure = 0;
    input_file.data_seg_alignment = BASESMUMA_CACHE_LINE_SIZE;
    input_file.mpool_size = sm_reg_data->size;

    /* call the connection manager and map my shared memory peers' file
     */
    ret = bcol_basesmuma_smcm_allgather_connection(
        sm_bcol,
        sm_bcol->super.sbgp_partner_module,
        &(cs->sm_connections_list),
        &(sm_bcol->payload_backing_files_info),
        sm_bcol->super.sbgp_partner_module->group_comm,
        input_file, cs->payload_base_fname,
        false);
    if( OMPI_SUCCESS != ret ) {
        goto exit_ERROR;
    }


    /* now we exchange offset info - don't assume symmetric virtual memory
     */

    mem_offset = (void *) ((uintptr_t) ml_block->block->base_addr -
                           (uintptr_t) cs->sm_payload_structs->data_addr);

    /* call into the exchange offsets function */
    ret=comm_allgather_pml(&mem_offset, results_array, sizeof (void *), MPI_BYTE,
                           sm_bcol_module->super.sbgp_partner_module->my_index,
                           sm_bcol_module->super.sbgp_partner_module->group_size,
                           sm_bcol_module->super.sbgp_partner_module->group_list,
                           sm_bcol_module->super.sbgp_partner_module->group_comm);
    if( OMPI_SUCCESS != ret ) {
        goto exit_ERROR;
    }

    /* convert memory offset to virtual address in current rank */
    leading_dim = pload_mgmt->size_of_group;
    loop_limit =  ml_block->num_banks*ml_block->num_buffers_per_bank;
    for (i=0;i< sm_bcol_module->super.sbgp_partner_module->group_size;i++) {

        /* get the base pointer */
        int array_id=SM_ARRAY_INDEX(leading_dim,0,i);
        if( i == sm_bcol_module->super.sbgp_partner_module->my_index) {
            /* me */
            base_ptr=cs->sm_payload_structs->map_addr;
        } else {
            base_ptr=sm_bcol_module->payload_backing_files_info[i]->
                sm_mmap->map_addr;
        }

        /* first, set the pointer to the control struct */
        pload_mgmt->data_buffs[array_id].ctl_struct=(mca_bcol_basesmuma_header_t *)
            (uintptr_t)(((uint64_t)(uintptr_t)results_array[array_id])+(uint64_t)(uintptr_t)base_ptr);
        /* second, calculate where to set the data pointer */
        pload_mgmt->data_buffs[array_id].payload=(void *)
            (uintptr_t)((uint64_t)(uintptr_t) pload_mgmt->data_buffs[array_id].ctl_struct +
                        (uint64_t)(uintptr_t) data_offset);

        for( buf_id = 1 ; buf_id < loop_limit ; buf_id++ ) {
            int array_id_m1=SM_ARRAY_INDEX(leading_dim,(buf_id-1),i);
            array_id=SM_ARRAY_INDEX(leading_dim,buf_id,i);
            /* now, play the same game as above
             *
             * first, set the control struct's position */
            pload_mgmt->data_buffs[array_id].ctl_struct=(mca_bcol_basesmuma_header_t *)
                (uintptr_t)(((uint64_t)(uintptr_t)(pload_mgmt->data_buffs[array_id_m1].ctl_struct) +
                             (uint64_t)(uintptr_t)ml_block->size_buffer));

            /* second, set the payload pointer */
            pload_mgmt->data_buffs[array_id].payload =(void *)
                (uintptr_t)((uint64_t)(uintptr_t) pload_mgmt->data_buffs[array_id].ctl_struct +
                            (uint64_t)(uintptr_t) data_offset);
        }

    }

    /* done with the index array */
    free (results_array);
    results_array = NULL;

    /* initialize my control structures!! */
    my_idx = sm_bcol_module->super.sbgp_partner_module->my_index;
    leading_dim = sm_bcol_module->super.sbgp_partner_module->group_size;
    for( buf_id = 0; buf_id < loop_limit; buf_id++){
        array_id = SM_ARRAY_INDEX(leading_dim,buf_id,my_idx);
        ctl_ptr = pload_mgmt->data_buffs[array_id].ctl_struct;

        /* initialize the data structures */
        for( j = 0; j < SM_BCOLS_MAX; j++){
            for( i = 0; i < NUM_SIGNAL_FLAGS; i++){
                ctl_ptr->flags[i][j] = -1;
            }
        }
        ctl_ptr->sequence_number = -1;
        ctl_ptr->src = -1;
    }




    /* setup the data structures needed for releasing the payload
     * buffers back to the ml level
     */
    for( i=0 ; i < (int) ml_block->num_banks ; i++ ) {
        sm_bcol->colls_with_user_data.
            ctl_buffs_mgmt[i].nb_barrier_desc.ml_memory_block_descriptor=
            ml_block;
    }

    ml_mem->num_banks = ml_block->num_banks;
    ml_mem->bank_release_counter = calloc(ml_block->num_banks, sizeof(uint32_t));
    ml_mem->num_buffers_per_bank = ml_block->num_buffers_per_bank;
    ml_mem->size_buffer = ml_block->size_buffer;
    /* pointer to ml level descriptor */
    ml_mem->ml_mem_desc = ml_block;

    if (OMPI_SUCCESS != init_nb_coll_buff_desc(&ml_mem->nb_coll_desc,
                                               ml_block->block->base_addr,
                                               ml_mem->num_banks,
                                               ml_mem->num_buffers_per_bank,
                                               ml_mem->size_buffer,
                                               data_offset,
                                               sm_bcol_module->super.sbgp_partner_module->group_size,
                                               sm_bcol_module->pow_k)) {

        BASESMUMA_VERBOSE(10, ("Failed to allocate memory descriptors for storing state of non-blocking collectives\n"));
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;

exit_ERROR:
    if (NULL != results_array) {
        free(results_array);
    }
    return ret;
}

#endif



/* Basesmuma interface function used for buffer release */
#if 0
/* gvm
 * A collective operation calls this routine to release the payload buffer.
 * All processes in the shared memory sub-group of a bcol should call the non-blocking
 * barrier on the last payload buffer of a memory bank. On the completion
 * of the non-blocking barrier, the ML callback is called which is responsible
 * for recycling the memory bank.
 */
mca_bcol_basesmuma_module_t *sm_bcol_module
int bcol_basesmuma_free_payload_buff(
    struct mca_bcol_base_memory_block_desc_t *block,
    sm_buffer_mgmt *ctl_mgmt,
    uint64_t buff_id)
{
    /* local variables */
    int ret = OMPI_SUCCESS;

    memory_bank = BANK_FROM_BUFFER_IDX(buff_id);
    ctl_mgmt->ctl_buffs_mgmt[memory_bank].n_buffs_freed++;

    OPAL_THREAD_ADD32(&(ctl_mgmt->ctl_buffs_mgmt[memory_bank].n_buffs_freed),1);

    if (ctl_mgmt->ctl_buffs_mgmt[memory_bank].n_buffs_freed == block->size_buffers_bank){

        /* start non-blocking barrier */
        bcol_basesmuma_rd_nb_barrier_init_admin(
            &(ctl_mgmt->ctl_buffs_mgmt[memory_bank].nb_barrier_desc));

        if (NB_BARRIER_DONE !=
            ctl_mgmt->ctl_buffs_mgmt[memory_bank].
            nb_barrier_desc.collective_phase){

            /* progress the barrier */
            opal_progress();
        }
        else{
            /* free the buffer - i.e. initiate callback to ml level */
            block->ml_release_cb(block,memory_bank);
        }
    }
    return ret;
}
#endif
