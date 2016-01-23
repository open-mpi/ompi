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

#ifndef OSC_RDMA_FRAG_H
#define OSC_RDMA_FRAG_H

#include "osc_rdma.h"
#include "opal/align.h"

/** Communication buffer for packing messages */
struct ompi_osc_rdma_frag_t {
    opal_free_list_item_t super;

    /* start of unused space */
    unsigned char *top;

    /* space remaining in buffer */
    uint32_t remain_len;
    /* Number of operations which have started writing into the frag, but not yet completed doing so */
    int32_t pending;

    ompi_osc_rdma_module_t *module;
    mca_btl_base_registration_handle_t *handle;
};
typedef struct ompi_osc_rdma_frag_t ompi_osc_rdma_frag_t;
OBJ_CLASS_DECLARATION(ompi_osc_rdma_frag_t);


static inline void ompi_osc_rdma_frag_complete (ompi_osc_rdma_frag_t *frag)
{
    if (0 == OPAL_THREAD_ADD32(&frag->pending, -1)) {
        opal_atomic_rmb ();

        ompi_osc_rdma_deregister (frag->module, frag->handle);
        frag->handle = NULL;

        opal_free_list_return (&mca_osc_rdma_component.frags, (opal_free_list_item_t *) frag);
    }
}

/*
 * Note: module lock must be held during this operation
 */
static inline int ompi_osc_rdma_frag_alloc (ompi_osc_rdma_module_t *module, size_t request_len,
                                            ompi_osc_rdma_frag_t **buffer, char **ptr)
{
    ompi_osc_rdma_frag_t *curr;
    int ret;

    /* ensure all buffers are 8-byte aligned */
    request_len = OPAL_ALIGN(request_len, 8, size_t);

    if (request_len > (mca_osc_rdma_component.buffer_size >> 1)) {
        return OMPI_ERR_VALUE_OUT_OF_BOUNDS;
    }

    OPAL_THREAD_LOCK(&module->lock);
    curr = module->rdma_frag;
    if (OPAL_UNLIKELY(NULL == curr || curr->remain_len < request_len)) {
        if (NULL == curr || (NULL != curr && curr->pending > 1)) {
            opal_free_list_item_t *item = NULL;

            /* release the initial reference to the buffer */
            module->rdma_frag = NULL;

            if (curr) {
                ompi_osc_rdma_frag_complete (curr);
            }

            item = opal_free_list_get (&mca_osc_rdma_component.frags);
            if (OPAL_UNLIKELY(NULL == item)) {
                OPAL_THREAD_UNLOCK(&module->lock);
                return OMPI_ERR_OUT_OF_RESOURCE;
            }

            curr = module->rdma_frag = (ompi_osc_rdma_frag_t *) item;

            curr->handle = NULL;
            curr->pending = 1;
            curr->module = module;
        }

        curr->top = curr->super.ptr;
        curr->remain_len = mca_osc_rdma_component.buffer_size;

        if (curr->remain_len < request_len) {
            OPAL_THREAD_UNLOCK(&module->lock);
            return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        }
    }

    if (!curr->handle && module->selected_btl->btl_register_mem) {
        ret = ompi_osc_rdma_register (module, MCA_BTL_ENDPOINT_ANY, curr->super.ptr, mca_osc_rdma_component.buffer_size,
                                      MCA_BTL_REG_FLAG_ACCESS_ANY, &curr->handle);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            OPAL_THREAD_UNLOCK(&module->lock);
            return ret;
        }
    }


    *ptr = (char *) curr->top;
    *buffer = curr;

    curr->top += request_len;
    curr->remain_len -= request_len;
    OPAL_THREAD_ADD32(&curr->pending, 1);

    OPAL_THREAD_UNLOCK(&module->lock);

    return OMPI_SUCCESS;
}

#endif
