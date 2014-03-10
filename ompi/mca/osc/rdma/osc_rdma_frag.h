/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012      Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OSC_RDMA_FRAG_H
#define OSC_RDMA_FRAG_H

#include "ompi/communicator/communicator.h"

#include "osc_rdma_header.h"
#include "osc_rdma_request.h"
#include "opal/align.h"

/** Communication buffer for packing messages */
struct ompi_osc_rdma_frag_t {
    opal_list_item_t super;
    /* target rank of buffer */
    int target;
    unsigned char *buffer;

    /* space remaining in buffer */
    size_t remain_len;

    /* start of unused space */
    char *top;

    /* Number of operations which have started writing into the frag, but not yet completed doing so */
    int pending;
    ompi_osc_rdma_frag_header_t *header;
    ompi_osc_rdma_module_t *module;
};
typedef struct ompi_osc_rdma_frag_t ompi_osc_rdma_frag_t;
OBJ_CLASS_DECLARATION(ompi_osc_rdma_frag_t);

extern int ompi_osc_rdma_frag_start(ompi_osc_rdma_module_t *module, ompi_osc_rdma_frag_t *buffer);
extern int ompi_osc_rdma_frag_flush_target(ompi_osc_rdma_module_t *module, int target);
extern int ompi_osc_rdma_frag_flush_all(ompi_osc_rdma_module_t *module);


/*
 * Note: module lock must be held during this operation
 */
static inline int ompi_osc_rdma_frag_alloc(ompi_osc_rdma_module_t *module, int target,
                                           size_t request_len, ompi_osc_rdma_frag_t **buffer,
                                           char **ptr)
{
    ompi_osc_rdma_frag_t *curr = module->peers[target].active_frag;
    int ret;

    /* osc rdma headers can have 64-bit values. these will need to be aligned
     * on an 8-byte boundary on some architectures so we up align the allocation
     * size here. */
    request_len = OPAL_ALIGN(request_len, 8, size_t);

    if (request_len > mca_osc_rdma_component.buffer_size) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    if (NULL == curr || curr->remain_len < request_len) {
        opal_free_list_item_t *item;

        if (NULL != curr) {
            curr->remain_len = 0;
            /* If there's something pending, the pending finish will
               start the buffer.  Otherwise, we need to start it now. */
            if (0 == curr->pending) {
                module->peers[target].active_frag = NULL;
                ret = ompi_osc_rdma_frag_start(module, curr);
            }
        }

        OPAL_FREE_LIST_GET(&mca_osc_rdma_component.frags,
                           item, ret);
        if (OMPI_SUCCESS != ret) return ret;
        curr = module->peers[target].active_frag =
            (ompi_osc_rdma_frag_t*) item;

        curr->target = target;

        curr->header = (ompi_osc_rdma_frag_header_t*) curr->buffer;
        curr->top = (char*) (curr->header + 1);
        curr->remain_len = mca_osc_rdma_component.buffer_size;
        curr->module = module;
        curr->pending = 0;

        curr->header->base.type = OMPI_OSC_RDMA_HDR_TYPE_FRAG;
        curr->header->base.flags = OMPI_OSC_RDMA_HDR_FLAG_VALID;
        if (module->passive_target_access_epoch) {
            curr->header->base.flags |= OMPI_OSC_RDMA_HDR_FLAG_PASSIVE_TARGET;
        }
        curr->header->source = ompi_comm_rank(module->comm);
        curr->header->num_ops = 0;
        curr->header->windx = ompi_comm_get_cid(module->comm);

        if (curr->remain_len < request_len) {
            return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        }
    }

    *ptr = curr->top;
    *buffer = curr;

    curr->top += request_len;
    curr->remain_len -= request_len;
    curr->pending++;
    curr->header->num_ops++;

    return OMPI_SUCCESS;
}


/*
 * Note: module lock must be held for this operation
 */
static inline int ompi_osc_rdma_frag_finish(ompi_osc_rdma_module_t *module,
                                            ompi_osc_rdma_frag_t* buffer)
{
    if (0 == --buffer->pending && 0 == buffer->remain_len) {
        if (OPAL_LIKELY(buffer == module->peers[buffer->target].active_frag)) {
            /* this is the active fragment. need to set the current fragment to null
             * or it will be started multiple times */
            module->peers[buffer->target].active_frag = NULL;
        }
        return ompi_osc_rdma_frag_start(module, buffer);
    }

    return OMPI_SUCCESS;
}

#endif
