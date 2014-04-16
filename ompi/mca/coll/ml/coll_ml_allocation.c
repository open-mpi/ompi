/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif  /* HAVE_STDLIB_H */

#include "coll_ml.h"
#include "coll_ml_inlines.h"
#include "coll_ml_allocation.h"

long memory_buffer_index;

mca_bcol_base_memory_block_desc_t *mca_coll_ml_allocate_block(struct mca_coll_ml_component_t *ml_component,
                                                   mca_bcol_base_memory_block_desc_t *ml_memblock)
{
    mca_bcol_base_memory_block_desc_t *ret = NULL;
    mca_bcol_base_memory_block_desc_t *memory_block = NULL;
    mca_coll_ml_lmngr_t *memory_manager = NULL;

    if (ml_memblock) {
        ML_ERROR(("Memory already allocated - expecting NULL pointer"));
        return ret;
    }
    memory_block = (mca_bcol_base_memory_block_desc_t*) calloc(1, sizeof(mca_bcol_base_memory_block_desc_t));

    if (NULL == memory_block){
        ML_ERROR(("Couldn't allocate memory for ml_memblock"));
        return ret;
    }

    memory_manager = &ml_component->memory_manager;
    memory_block->block = mca_coll_ml_lmngr_alloc(memory_manager);
    memory_block->size_block = memory_manager->list_block_size;

    if (!memory_block->block){
        ML_VERBOSE(1, ("lmngr failed."));
        ret = NULL;
        goto exit_ERROR;
    }

    return memory_block;

exit_ERROR:
    if (memory_block){
        free(memory_block);
        return ret;
    }

    return ret;
}

void mca_coll_ml_free_block (mca_bcol_base_memory_block_desc_t *ml_memblock)
{
    if (!ml_memblock)
        return;

    if (ml_memblock->buffer_descs){
        free(ml_memblock->buffer_descs);
    }

    mca_coll_ml_lmngr_free(ml_memblock->block);
    free(ml_memblock->bank_release_counters);
    free(ml_memblock->ready_for_memsync);
    free(ml_memblock->bank_is_busy);
    free(ml_memblock);
}

int mca_coll_ml_initialize_block(mca_bcol_base_memory_block_desc_t *ml_memblock,
                                 uint32_t num_buffers,
                                 uint32_t num_banks,
                                 uint32_t buffer_size,
                                 int32_t data_offset,
                                 opal_list_t *bcols_in_use)
{
    int ret = OMPI_SUCCESS;
    uint32_t bank_loop, buff_loop;
    uint64_t addr_offset = 0;
    mca_bcol_base_payload_buffer_desc_t *pbuff_descs = NULL,*pbuff_desc = NULL;

    if (0 == num_banks || 0 == num_buffers || 0 == buffer_size) {
        return OMPI_ERR_BAD_PARAM;
    }

    if (NULL == ml_memblock){
        ML_ERROR(("Memory block not initialized"));
        ret = OMPI_ERROR;
        goto exit_ERROR;
    }

    if (ml_memblock->size_block < (num_buffers * num_banks * buffer_size) ){
        ML_ERROR(("Not enough memory for all buffers  and banks in the memory block"));
        ret = OMPI_ERROR;
        goto exit_ERROR;
    }

    pbuff_descs = (mca_bcol_base_payload_buffer_desc_t*) malloc(sizeof(mca_bcol_base_payload_buffer_desc_t)
            * num_banks * num_buffers);
    if (NULL == pbuff_descs) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for(bank_loop = 0; bank_loop < num_banks; bank_loop++)
        for(buff_loop = 0; buff_loop < num_buffers; buff_loop++){
            pbuff_desc = &pbuff_descs[bank_loop*num_buffers + buff_loop];

            pbuff_desc->base_data_addr = (void *)
                ((char *)ml_memblock->block->base_addr + addr_offset);
            pbuff_desc->data_addr = (void *)
                ((char *)pbuff_desc->base_data_addr + (size_t)data_offset);

            addr_offset+=buffer_size;
            pbuff_desc->buffer_index = BUFFER_INDEX(bank_loop,num_buffers,buff_loop);

            pbuff_desc->bank_index=bank_loop;
            pbuff_desc->generation_number=0;
        }

    /* Initialize ml memory block */
    /* gvm FIX:This counter when zero indicates that the bank is ready for
     * recycle. This is  initialized to number of bcol components as each bcol is responsible for
     * releasing the buffers of a bank. This initialization will have
     * faulty behavior, example in case of multiple interfaces,  when more than
     * one bcol module of the component type is in use.
     */
    ml_memblock->bank_release_counters = (uint32_t *) calloc(num_banks, sizeof(uint32_t));
    if (NULL == ml_memblock->bank_release_counters) {
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit_ERROR;
    }

    ml_memblock->ready_for_memsync = (bool *) calloc(num_banks, sizeof(bool));
    if (NULL == ml_memblock->ready_for_memsync) {
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit_ERROR;
    }

    ml_memblock->bank_is_busy = (bool *) calloc(num_banks, sizeof(bool));
    if (NULL == ml_memblock->bank_is_busy) {
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit_ERROR;
    }

    /* Set index for first bank to sync */
    ml_memblock->memsync_counter = 0;

    /* use first bank and first buffer */
    ml_memblock->next_free_buffer = 0;

    ml_memblock->block_addr_offset = addr_offset;
    ml_memblock->num_buffers_per_bank = num_buffers;
    ml_memblock->num_banks = num_banks;
    ml_memblock->size_buffer = buffer_size;
    ml_memblock->buffer_descs = pbuff_descs;

    return ret;

exit_ERROR:
    /* Free all buffer descriptors */
    if (pbuff_descs){
        free(pbuff_descs);
    }

    return ret;
}

mca_bcol_base_payload_buffer_desc_t *mca_coll_ml_alloc_buffer (mca_coll_ml_module_t *module)
{
    uint64_t bindex;
    uint32_t bank, buffer, num_buffers;
    mca_bcol_base_memory_block_desc_t *ml_memblock = module->payload_block;
    mca_bcol_base_payload_buffer_desc_t *pbuff_descs = NULL,
        *ml_membuffer = NULL;

    /* Return a buffer */
    num_buffers = ml_memblock->num_buffers_per_bank;
    pbuff_descs = ml_memblock->buffer_descs;
    bindex = ml_memblock->next_free_buffer;
    buffer = bindex % num_buffers;
    bank = bindex/num_buffers;

    ML_VERBOSE(10, ("ML allocator: allocating buffer index %d, bank index %d", buffer, bank));

    /* First buffer in bank, use next bank */
    if (0 == buffer) {
        if(!ml_memblock->bank_is_busy[bank]) {
            /* the bank is free, mark it busy */
            ml_memblock->bank_is_busy[bank] = true;
            ML_VERBOSE(10, ("ML allocator: reset bank %d to value %d", bank,
                            ml_memblock->bank_release_counters[bank]));
        } else {
            /* the bank is busy, return NULL and upper layer will handle it */
            ML_VERBOSE(10, ("No free payload buffers are available for use."
                            " Next memory bank is still used by one of bcols"));
            return NULL;
        }
    }

    assert(true == ml_memblock->bank_is_busy[bank]);

    ml_membuffer = &pbuff_descs[bindex];
    ML_VERBOSE(10, ("ML allocator: ml buffer index %d", bindex));

    /* Compute next free buffer */
    buffer = (buffer == num_buffers - 1) ? 0 : buffer + 1;
    if (0 == buffer) {
        bank = (bank == ml_memblock->num_banks - 1) ? 0 : bank + 1;
    }

    ml_memblock->next_free_buffer = BUFFER_INDEX(bank,num_buffers,buffer);

    return ml_membuffer;
}
