/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2018      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_uct_am.h"
#include "btl_uct_rdma.h"
#include "btl_uct_device_context.h"

/**
 * Allocate a segment.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 */
mca_btl_base_descriptor_t *mca_btl_uct_alloc (mca_btl_base_module_t *btl, mca_btl_base_endpoint_t *endpoint,
                                              uint8_t order, size_t size, uint32_t flags)
{
    mca_btl_uct_module_t *uct_btl = (mca_btl_uct_module_t *) btl;
    mca_btl_uct_base_frag_t *frag = NULL;

    if (size <= (size_t) MCA_BTL_UCT_TL_ATTR(uct_btl->am_tl, 0).cap.am.max_short) {
        frag = mca_btl_uct_frag_alloc_short (uct_btl, endpoint);
    } else if (size <= uct_btl->super.btl_eager_limit) {
        frag = mca_btl_uct_frag_alloc_eager (uct_btl, endpoint);
    } else {
        frag = mca_btl_uct_frag_alloc_max (uct_btl, endpoint);
    }

    if (OPAL_LIKELY(frag != NULL)) {
        frag->segments[0].seg_len  = size;

        frag->base.des_segment_count = 1;
        frag->base.des_flags   = flags;
        frag->base.order       = order;
        frag->uct_iov.length = size;
        if (NULL != frag->base.super.registration) {
            /* zero-copy fragments will need callbacks */
            frag->base.des_flags |= MCA_BTL_DES_SEND_ALWAYS_CALLBACK;
        }
    }

    return (mca_btl_base_descriptor_t *) frag;
}

static inline void _mca_btl_uct_send_pack (void *data, void *header, size_t header_size, opal_convertor_t *convertor,
                                           size_t payload_size)
{
    uint32_t iov_count = 1;
    struct iovec iov;
    size_t length;

    if (header_size > 0) {
        assert (NULL != header);
        memcpy (data, header, header_size);
    }

    /* pack the data into the supplied buffer */
    iov.iov_base = (IOVBASE_TYPE *) ((intptr_t) data + header_size);
    iov.iov_len  = length = payload_size;

    (void) opal_convertor_pack (convertor, &iov, &iov_count, &length);

    assert (length == payload_size);
}

struct mca_btl_base_descriptor_t *mca_btl_uct_prepare_src (mca_btl_base_module_t *btl,
                                                           mca_btl_base_endpoint_t *endpoint,
                                                           opal_convertor_t *convertor,
                                                           uint8_t order, size_t reserve,
                                                           size_t *size, uint32_t flags)
{
    mca_btl_uct_module_t *uct_btl = (mca_btl_uct_module_t *) btl;
    const size_t total_size = reserve + *size;
    mca_btl_uct_base_frag_t *frag;
    void *data_ptr;

    /* in place send fragment */
    if (OPAL_UNLIKELY(opal_convertor_need_buffers(convertor) || total_size > uct_btl->super.btl_eager_limit)) {
        frag = (mca_btl_uct_base_frag_t *) mca_btl_uct_alloc (btl, endpoint, order, total_size, flags);
        if (OPAL_UNLIKELY(NULL == frag)) {
            return NULL;
        }

        _mca_btl_uct_send_pack ((void *) ((intptr_t) frag->uct_iov.buffer + reserve), NULL, 0,
                                convertor, *size);
    } else {
        opal_convertor_get_current_pointer (convertor, &data_ptr);
        assert (NULL != data_ptr);

        frag = mca_btl_uct_frag_alloc_short (uct_btl, endpoint);
        if (OPAL_UNLIKELY(NULL == frag)) {
            return NULL;
        }

        frag->uct_iov.length   = total_size;
        frag->base.order       = order;
        frag->base.des_flags   = flags;
        if (total_size > (size_t) MCA_BTL_UCT_TL_ATTR(uct_btl->am_tl, 0).cap.am.max_short) {
            frag->segments[0].seg_len = reserve;
            frag->segments[1].seg_len = *size;
            frag->segments[1].seg_addr.pval = data_ptr;
            frag->base.des_segment_count = 2;
        } else {
            frag->segments[0].seg_len = total_size;
            memcpy ((void *)((intptr_t) frag->segments[1].seg_addr.pval + reserve), data_ptr, *size);
            frag->base.des_segment_count = 1;
        }
    }

    return &frag->base;
}

/**
 * Return a segment allocated by this BTL.
 *
 * @param btl (IN)      BTL module
 * @param segment (IN)  Allocated segment.
 */
int mca_btl_uct_free (mca_btl_base_module_t *btl, mca_btl_base_descriptor_t *des)
{
    mca_btl_uct_frag_return ((mca_btl_uct_base_frag_t *) des);
    return OPAL_SUCCESS;
}

static size_t mca_btl_uct_send_frag_pack (void *data, void *arg)
{
    mca_btl_uct_base_frag_t *frag = (mca_btl_uct_base_frag_t *) arg;
    size_t length = 8;

    memcpy (data, &frag->header, sizeof (frag->header));
    data = (void *)((intptr_t) data + 8);

    /* this function should only ever get called with fragments with two segments */
    for (size_t i = 0 ; i < frag->base.des_segment_count ; ++i) {
        const size_t seg_len = frag->segments[i].seg_len;
        memcpy (data, frag->segments[i].seg_addr.pval, seg_len);
        data = (void *)((intptr_t) data + seg_len);
        length += seg_len;
    }

    return length;
}

static void mca_btl_uct_append_pending_frag (mca_btl_uct_module_t *uct_btl, mca_btl_uct_base_frag_t *frag,
                                             mca_btl_uct_device_context_t *context, bool ready)
{
    frag->ready = ready;
    frag->base.des_flags |= MCA_BTL_DES_SEND_ALWAYS_CALLBACK;
    opal_atomic_wmb ();

    opal_list_append (&uct_btl->pending_frags, (opal_list_item_t *) frag);
}

int mca_btl_uct_send_frag (mca_btl_uct_module_t *uct_btl, mca_btl_uct_base_frag_t *frag, bool append)
{
    mca_btl_uct_device_context_t *context = frag->context;
    const ssize_t msg_size = frag->uct_iov.length + 8;
    ssize_t size;
    ucs_status_t ucs_status;
    uct_ep_h ep_handle = NULL;

    /* if we get here then we must have an endpoint handle for this context/endpoint pair */
    (void) mca_btl_uct_endpoint_test_am (uct_btl, frag->endpoint, frag->context, &ep_handle);
    assert (NULL != ep_handle);

    /* if another thread set this we really don't care too much as this flag is only meant
     * to protect against deep recursion */
    if (!context->in_am_callback) {
        mca_btl_uct_context_lock (context);
        /* attempt to post the fragment */
        if (NULL != frag->base.super.registration) {
            frag->comp.dev_context = context;
            ucs_status = uct_ep_am_zcopy (ep_handle, MCA_BTL_UCT_FRAG, &frag->header, sizeof (frag->header),
                                          &frag->uct_iov, 1, 0, &frag->comp.uct_comp);

            if (OPAL_LIKELY(UCS_INPROGRESS == ucs_status)) {
                uct_worker_progress (context->uct_worker);
                mca_btl_uct_context_unlock (context);
                return OPAL_SUCCESS;
            }
        } else {
            /* short message */
            if (1 == frag->base.des_segment_count && (frag->uct_iov.length + 8) < MCA_BTL_UCT_TL_ATTR(uct_btl->am_tl, 0).cap.am.max_short) {
                ucs_status = uct_ep_am_short (ep_handle, MCA_BTL_UCT_FRAG, frag->header.value, frag->uct_iov.buffer,
                                              frag->uct_iov.length);

                if (OPAL_LIKELY(UCS_OK == ucs_status)) {
                    uct_worker_progress (context->uct_worker);
                    mca_btl_uct_context_unlock (context);
                    /* send is complete */
                    mca_btl_uct_frag_complete (frag, OPAL_SUCCESS);
                    return 1;
                }
            }

            size = uct_ep_am_bcopy (ep_handle, MCA_BTL_UCT_FRAG, mca_btl_uct_send_frag_pack, frag, 0);
            if (OPAL_LIKELY(size == msg_size)) {
                uct_worker_progress (context->uct_worker);
                mca_btl_uct_context_unlock (context);
                /* send is complete */
                mca_btl_uct_frag_complete (frag, OPAL_SUCCESS);
                return 1;
            }
        }

        /* wait for something to happen */
        uct_worker_progress (context->uct_worker);
        mca_btl_uct_context_unlock (context);

        mca_btl_uct_device_handle_completions (context);
    }

    if (!append) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    OPAL_THREAD_LOCK(&uct_btl->lock);
    mca_btl_uct_append_pending_frag (uct_btl, frag, context, true);
    OPAL_THREAD_UNLOCK(&uct_btl->lock);

    return OPAL_SUCCESS;
}

int mca_btl_uct_send (mca_btl_base_module_t *btl, mca_btl_base_endpoint_t *endpoint, mca_btl_base_descriptor_t *descriptor,
                      mca_btl_base_tag_t tag)
{
    mca_btl_uct_module_t *uct_btl = (mca_btl_uct_module_t *) btl;
    mca_btl_uct_device_context_t *context = mca_btl_uct_module_get_am_context (uct_btl);
    mca_btl_uct_base_frag_t *frag = (mca_btl_uct_base_frag_t *) descriptor;
    uct_ep_h ep_handle;
    int rc;

    BTL_VERBOSE(("btl/uct sending descriptor %p from %d -> %d. length = %" PRIu64, (void *)descriptor,
                 OPAL_PROC_MY_NAME.vpid, endpoint->ep_proc->proc_name.vpid, frag->uct_iov.length));


    frag->header.data.tag = tag;
    frag->context = context;

    rc = mca_btl_uct_endpoint_check_am (uct_btl, endpoint, context, &ep_handle);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        OPAL_THREAD_LOCK(&uct_btl->lock);
        /* check one more time in case another thread is completing the connection now */
        if (OPAL_SUCCESS != mca_btl_uct_endpoint_test_am (uct_btl, endpoint, context, &ep_handle)) {
            mca_btl_uct_append_pending_frag (uct_btl, frag, context, false);
            OPAL_THREAD_UNLOCK(&uct_btl->lock);
            return OPAL_SUCCESS;
        }
        OPAL_THREAD_UNLOCK(&uct_btl->lock);
    }

    return mca_btl_uct_send_frag (uct_btl, frag, true);
}

struct mca_btl_uct_sendi_pack_args_t {
    uint64_t am_header;
    void *header;
    size_t header_size;
    opal_convertor_t *convertor;
    size_t payload_size;
};

typedef struct mca_btl_uct_sendi_pack_args_t mca_btl_uct_sendi_pack_args_t;

static size_t mca_btl_uct_sendi_pack (void *data, void *arg)
{
    mca_btl_uct_sendi_pack_args_t *args = (mca_btl_uct_sendi_pack_args_t *) arg;
    mca_btl_uct_am_header_t *am_header = (mca_btl_uct_am_header_t *) data;

    am_header->value = args->am_header;
    _mca_btl_uct_send_pack ((void *)((intptr_t)data + 8), args->header, args->header_size, args->convertor,
                            args->payload_size);
    return args->header_size + args->payload_size + 8;
}

static inline size_t mca_btl_uct_max_sendi (mca_btl_uct_module_t *uct_btl, int context_id)
{
    return MCA_BTL_UCT_TL_ATTR(uct_btl->am_tl, context_id).cap.am.max_bcopy;
}

int mca_btl_uct_sendi (mca_btl_base_module_t *btl, mca_btl_base_endpoint_t *endpoint, opal_convertor_t *convertor,
                       void *header, size_t header_size, size_t payload_size, uint8_t order, uint32_t flags,
                       mca_btl_base_tag_t tag, mca_btl_base_descriptor_t **descriptor)
{
    mca_btl_uct_module_t *uct_btl = (mca_btl_uct_module_t *) btl;
    mca_btl_uct_device_context_t *context = mca_btl_uct_module_get_am_context (uct_btl);
    const size_t total_size = header_size + payload_size;
    /* message with header */
    const size_t msg_size = total_size + 8;
    mca_btl_uct_am_header_t am_header;
    ucs_status_t ucs_status = UCS_ERR_NO_RESOURCE;
    uct_ep_h ep_handle;
    int rc;

    rc = mca_btl_uct_endpoint_check_am (uct_btl, endpoint, context, &ep_handle);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc || msg_size > mca_btl_uct_max_sendi (uct_btl, context->context_id))) {
        if (descriptor) {
            *descriptor = mca_btl_uct_alloc (btl, endpoint, order, total_size, flags);
        }

        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    am_header.data.tag = tag;

    mca_btl_uct_context_lock (context);
    if (0 == payload_size) {
        ucs_status = uct_ep_am_short (ep_handle, MCA_BTL_UCT_FRAG, am_header.value, header, header_size);
    } else if (msg_size < (size_t) MCA_BTL_UCT_TL_ATTR(uct_btl->am_tl, context->context_id).cap.am.max_short) {
        int8_t *data = alloca (total_size);
        _mca_btl_uct_send_pack (data, header, header_size, convertor, payload_size);
        ucs_status = uct_ep_am_short (ep_handle, MCA_BTL_UCT_FRAG, am_header.value, data, total_size);
    } else {
        ssize_t size;

        size = uct_ep_am_bcopy (ep_handle, MCA_BTL_UCT_FRAG, mca_btl_uct_sendi_pack,
                                &(mca_btl_uct_sendi_pack_args_t) {.am_header = am_header.value,
                                        .header = header, .header_size = header_size,
                                        .convertor = convertor, .payload_size = payload_size}, 0);
        if (OPAL_LIKELY(size == (ssize_t) msg_size)) {
            ucs_status = UCS_OK;
        }
    }

    mca_btl_uct_context_unlock (context);

    if (OPAL_UNLIKELY(UCS_OK != ucs_status)) {
        if (descriptor) {
            *descriptor = mca_btl_uct_alloc (btl, endpoint, order, total_size, flags);
        }

        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    return OPAL_SUCCESS;
}
