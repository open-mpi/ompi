/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012      Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2014-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OSC_PT2PT_FRAG_H
#define OSC_PT2PT_FRAG_H

#include "ompi/communicator/communicator.h"

#include "osc_pt2pt_header.h"
#include "osc_pt2pt_request.h"
#include "opal/align.h"

/** Communication buffer for packing messages */
struct ompi_osc_pt2pt_frag_t {
    opal_free_list_item_t super;
    /* target rank of buffer */
    int target;
    unsigned char *buffer;

    /* space remaining in buffer */
    size_t remain_len;

    /* start of unused space */
    char *top;

    /* Number of operations which have started writing into the frag, but not yet completed doing so */
    int32_t pending;
    ompi_osc_pt2pt_frag_header_t *header;
    ompi_osc_pt2pt_module_t *module;
};
typedef struct ompi_osc_pt2pt_frag_t ompi_osc_pt2pt_frag_t;
OBJ_CLASS_DECLARATION(ompi_osc_pt2pt_frag_t);

extern int ompi_osc_pt2pt_frag_start(ompi_osc_pt2pt_module_t *module, ompi_osc_pt2pt_frag_t *buffer);
extern int ompi_osc_pt2pt_frag_flush_target(ompi_osc_pt2pt_module_t *module, int target);
extern int ompi_osc_pt2pt_frag_flush_all(ompi_osc_pt2pt_module_t *module);

/*
 * Note: module lock must be held during this operation
 */
static inline int ompi_osc_pt2pt_frag_alloc (ompi_osc_pt2pt_module_t *module, int target,
                                             size_t request_len, ompi_osc_pt2pt_frag_t **buffer,
                                             char **ptr)
{
    ompi_osc_pt2pt_frag_t *curr;
    int ret;

    /* osc pt2pt headers can have 64-bit values. these will need to be aligned
     * on an 8-byte boundary on some architectures so we up align the allocation
     * size here. */
    request_len = OPAL_ALIGN(request_len, 8, size_t);

    if (request_len > mca_osc_pt2pt_component.buffer_size) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    OPAL_THREAD_LOCK(&module->lock);
    curr = module->peers[target].active_frag;
    if (NULL == curr || curr->remain_len < request_len) {
        opal_free_list_item_t *item = NULL;

        if (NULL != curr) {
            curr->remain_len = 0;
            module->peers[target].active_frag = NULL;
            opal_atomic_mb ();

            /* If there's something pending, the pending finish will
               start the buffer.  Otherwise, we need to start it now. */
            if (0 == OPAL_THREAD_ADD32(&curr->pending, -1)) {
                ret = ompi_osc_pt2pt_frag_start(module, curr);
                if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                    return ret;
                }
            }
        }

        item = opal_free_list_get (&mca_osc_pt2pt_component.frags);
        if (OPAL_UNLIKELY(NULL == item)) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        curr = module->peers[target].active_frag =
            (ompi_osc_pt2pt_frag_t*) item;

        curr->target = target;

        curr->header = (ompi_osc_pt2pt_frag_header_t*) curr->buffer;
        curr->top = (char*) (curr->header + 1);
        curr->remain_len = mca_osc_pt2pt_component.buffer_size;
        curr->module = module;
        curr->pending = 1;

        curr->header->base.type = OMPI_OSC_PT2PT_HDR_TYPE_FRAG;
        curr->header->base.flags = OMPI_OSC_PT2PT_HDR_FLAG_VALID;
        if (module->passive_target_access_epoch) {
            curr->header->base.flags |= OMPI_OSC_PT2PT_HDR_FLAG_PASSIVE_TARGET;
        }
        curr->header->source = ompi_comm_rank(module->comm);
        curr->header->num_ops = 0;
        curr->header->windx = ompi_comm_get_cid(module->comm);

        if (curr->remain_len < request_len) {
            OPAL_THREAD_UNLOCK(&module->lock);
            return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        }
    }

    *ptr = curr->top;
    *buffer = curr;

    curr->top += request_len;
    curr->remain_len -= request_len;
    OPAL_THREAD_UNLOCK(&module->lock);

    OPAL_THREAD_ADD32(&curr->pending, 1);
    OPAL_THREAD_ADD32(&curr->header->num_ops, 1);

    return OMPI_SUCCESS;
}


/*
 * Note: module lock must be held for this operation
 */
static inline int ompi_osc_pt2pt_frag_finish(ompi_osc_pt2pt_module_t *module,
                                            ompi_osc_pt2pt_frag_t* buffer)
{
    if (0 == OPAL_THREAD_ADD32(&buffer->pending, -1)) {
        return ompi_osc_pt2pt_frag_start(module, buffer);
    }

    return OMPI_SUCCESS;
}

#endif
