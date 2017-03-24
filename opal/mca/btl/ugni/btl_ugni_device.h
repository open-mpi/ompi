/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2017 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file This file contains wrappers for uGNI functionality. These wrappers are thread-safe
 * and intended to provide a way to measure various different ways to handle mutual exclusion
 * into the uGNI library (which is not thread safe). These functions are all defined to be
 * inline to limit the cost to non-threaded users.
 */

#if !defined(BTL_UGNI_DEVICE_H)
#define BTL_UGNI_DEVICE_H

#include "btl_ugni_endpoint.h"
#include "btl_ugni_frag.h"

/* helper functions */

typedef struct mca_btl_ugni_smsg_send_wtag_arg_t {
    gni_ep_handle_t ep_handle;
    void *hdr;
    size_t hdr_len;
    void *payload;
    size_t payload_len;
    uint32_t msg_id;
    int tag;
} mca_btl_ugni_smsg_send_wtag_arg_t;

static inline int mca_btl_ugni_smsg_send_wtag_device (mca_btl_ugni_device_t *device, void *arg)
{
    mca_btl_ugni_smsg_send_wtag_arg_t *args = (mca_btl_ugni_smsg_send_wtag_arg_t *) arg;
    gni_return_t grc;

    grc = GNI_SmsgSendWTag (args->ep_handle, args->hdr, args->hdr_len, args->payload,
                            args->payload_len, args->msg_id, args->tag);
    device->dev_smsg_local_cq.active_operations += (GNI_RC_SUCCESS == grc);
    return grc;
}

typedef struct mca_btl_ugni_smsg_get_next_wtag_arg_t {
    gni_ep_handle_t ep_handle;
    uintptr_t *data_ptr;
    uint8_t *tag;
} mca_btl_ugni_smsg_get_next_wtag_arg_t;

static inline intptr_t mca_btl_ugni_smsg_get_next_wtag_device (mca_btl_ugni_device_t *device, void *arg)
{
    mca_btl_ugni_smsg_get_next_wtag_arg_t *args = (mca_btl_ugni_smsg_get_next_wtag_arg_t *) arg;
    return GNI_SmsgGetNextWTag(args->ep_handle, (void **) args->data_ptr, args->tag);
}

static inline intptr_t mca_btl_ugni_smsg_release_device (mca_btl_ugni_device_t *device, void *arg)
{
    mca_btl_ugni_endpoint_handle_t *ep_handle = (mca_btl_ugni_endpoint_handle_t *) arg;

    return GNI_SmsgRelease (ep_handle->gni_handle);
}

static inline intptr_t mca_btl_ugni_cq_clear_device (mca_btl_ugni_device_t *device, void *arg)
{
    gni_cq_handle_t cq = (gni_cq_handle_t) (intptr_t) arg;
    gni_cq_entry_t event_data;
    int rc;

    do {
        rc = GNI_CqGetEvent (cq, &event_data);
    } while (GNI_RC_NOT_DONE != rc);

    return OPAL_SUCCESS;
}

typedef struct mca_btl_ugni_cq_get_event_args_t {
    mca_btl_ugni_cq_t *cq;
    gni_cq_entry_t *event_data;
} mca_btl_ugni_cq_get_event_args_t;

static inline intptr_t mca_btl_ugni_cq_get_event_device (mca_btl_ugni_device_t *device, void *arg)
{
    mca_btl_ugni_cq_get_event_args_t *args = (mca_btl_ugni_cq_get_event_args_t *) arg;
    gni_return_t rc;

    rc = GNI_CqGetEvent (args->cq->gni_handle, args->event_data);
    args->cq->active_operations -= GNI_RC_NOT_DONE != rc;
    return rc;
}

typedef struct mca_btl_ugni_gni_cq_get_event_args_t {
    gni_cq_handle_t cq;
    gni_cq_entry_t *event_data;
} mca_btl_ugni_gni_cq_get_event_args_t;

static inline intptr_t mca_btl_ugni_gni_cq_get_event_device (mca_btl_ugni_device_t *device, void *arg)
{
    mca_btl_ugni_gni_cq_get_event_args_t *args = (mca_btl_ugni_gni_cq_get_event_args_t *) arg;

    return GNI_CqGetEvent (args->cq, args->event_data);
}

static inline intptr_t mca_btl_ugni_post_fma_device (mca_btl_ugni_device_t *device, void *arg)
{
    mca_btl_ugni_post_descriptor_t *desc = (mca_btl_ugni_post_descriptor_t *) arg;
    bool ep_handle_allocated = false;
    int rc;

    if (NULL == desc->ep_handle) {
        desc->ep_handle = mca_btl_ugni_ep_get_rdma (desc->endpoint, device);
        if (OPAL_UNLIKELY(NULL == desc->ep_handle)) {
            return OPAL_ERR_TEMP_OUT_OF_RESOURCE;
        }
        ep_handle_allocated = true;
    }

    BTL_VERBOSE(("Posting FMA descriptor %p with op_type %d, amo %d, ep_handle %p, remote_addr 0x%lx, "
                 "length %lu", (void*)desc, desc->desc.type, desc->desc.amo_cmd, (void*)desc->ep_handle,
                 desc->desc.remote_addr, desc->desc.length));

    rc = GNI_PostFma (desc->ep_handle->gni_handle, &desc->desc);
    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != rc)) {
        if (ep_handle_allocated) {
            /* only return the endpoint handle if we allocated it. if we didn't allocate the
             * handle this call was likely made from repost() */
            mca_btl_ugni_ep_return_rdma (desc->ep_handle);
            desc->ep_handle = NULL;
        }
    } else {
        ++device->dev_rdma_local_cq.active_operations;
    }

    return mca_btl_rc_ugni_to_opal (rc);
}

static inline intptr_t mca_btl_ugni_post_rdma_device (mca_btl_ugni_device_t *device, void *arg)
{
    mca_btl_ugni_post_descriptor_t *desc = (mca_btl_ugni_post_descriptor_t *) arg;
    bool ep_handle_allocated = false;
    int rc;

    if (NULL == desc->ep_handle) {
        desc->ep_handle = mca_btl_ugni_ep_get_rdma (desc->endpoint, device);
        if (OPAL_UNLIKELY(NULL == desc->ep_handle)) {
            return OPAL_ERR_TEMP_OUT_OF_RESOURCE;
        }
        ep_handle_allocated = true;
    }

    /* pick the appropriate CQ */
    desc->cq = mca_btl_ugni_component.progress_thread_enabled ? &device->dev_rdma_local_irq_cq :
        &device->dev_rdma_local_cq;

    desc->desc.src_cq_hndl = desc->cq->gni_handle;

    BTL_VERBOSE(("Posting RDMA descriptor %p with op_type %d, ep_handle %p, remote_addr 0x%lx, "
                 "length %lu", (void*)desc, desc->desc.type, (void*)desc->ep_handle, desc->desc.remote_addr,
                 desc->desc.length));

    rc = GNI_PostRdma (desc->ep_handle->gni_handle, &desc->desc);
    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != rc)) {
        if (ep_handle_allocated) {
            /* only return the endpoint handle if we allocated it. if we didn't allocate the
             * handle this call was likely made from repost() */
            mca_btl_ugni_ep_return_rdma (desc->ep_handle);
            desc->ep_handle = NULL;
        }
    } else {
        ++desc->cq->active_operations;
    }

    return mca_btl_rc_ugni_to_opal (rc);
}

static inline intptr_t mca_btl_ugni_post_cqwrite_device (mca_btl_ugni_device_t *device, void *arg)
{
    mca_btl_ugni_post_descriptor_t *desc = (mca_btl_ugni_post_descriptor_t *) arg;
    int rc;

    desc->ep_handle = mca_btl_ugni_ep_get_rdma (desc->endpoint, device);
    if (OPAL_UNLIKELY(NULL == desc->ep_handle)) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    desc->desc.src_cq_hndl = device->dev_rdma_local_cq.gni_handle;

    rc = GNI_PostCqWrite (desc->ep_handle->gni_handle, &desc->desc);
    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != rc)) {
        mca_btl_ugni_ep_return_rdma (desc->ep_handle);
        desc->ep_handle = NULL;
    }

    return mca_btl_rc_ugni_to_opal (rc);
}

typedef struct mca_btl_ugni_cq_get_completed_desc_arg_t {
    mca_btl_ugni_cq_t *cq;
    gni_cq_entry_t *event_data;
    mca_btl_ugni_post_descriptor_t **post_desc;
    int count;
} mca_btl_ugni_cq_get_completed_desc_arg_t;

static inline intptr_t mca_btl_ugni_cq_get_completed_desc_device (mca_btl_ugni_device_t *device, void *arg0)
{
    mca_btl_ugni_cq_get_completed_desc_arg_t *args = (mca_btl_ugni_cq_get_completed_desc_arg_t *) arg0;
    mca_btl_ugni_cq_t *cq = args->cq;
    gni_post_descriptor_t *desc;
    int rc;

    for (int i = 0 ; i < args->count ; ++i) {
        rc = GNI_CqGetEvent (cq->gni_handle, args->event_data + i);
        if (GNI_RC_NOT_DONE == rc) {
            return i;
        }

        if (OPAL_UNLIKELY((GNI_RC_SUCCESS != rc && !args->event_data[i]) || GNI_CQ_OVERRUN(args->event_data[i]))) {
            /* TODO -- need to handle overrun -- how do we do this without an event?
               will the event eventually come back? Ask Cray */
            BTL_ERROR(("unhandled post error! ugni rc = %d %s", rc, gni_err_str[rc]));

            return mca_btl_rc_ugni_to_opal (rc);
        }

        rc = GNI_GetCompleted (cq->gni_handle, args->event_data[i], &desc);
        if (OPAL_UNLIKELY(GNI_RC_SUCCESS != rc && GNI_RC_TRANSACTION_ERROR != rc)) {
            BTL_ERROR(("Error in GNI_GetComplete %s", gni_err_str[rc]));
            return mca_btl_rc_ugni_to_opal (rc);
        }

        args->post_desc[i] = MCA_BTL_UGNI_DESC_TO_PDESC(desc);
        /* return the endpoint handle while we have the lock. see the explanation in
         * the documentation for mca_btl_ugni_ep_return_rdma() */
        if (OPAL_LIKELY(GNI_CQ_STATUS_OK(args->event_data[i]))) {
            /* the operation completed successfully. return the endpoint handle now. otherwise
             * we may still need the endpoint handle to start the repost(). */
            mca_btl_ugni_ep_return_rdma (args->post_desc[i]->ep_handle);
            args->post_desc[i]->ep_handle = NULL;
        }
        --cq->active_operations;
    }

    return args->count;
}

typedef struct mca_btl_ugni_get_datagram_args_t {
    mca_btl_ugni_module_t *ugni_module;
    gni_ep_handle_t *handle;
    mca_btl_base_endpoint_t **ep;
} mca_btl_ugni_get_datagram_args_t;

static inline intptr_t mca_btl_ugni_get_datagram_device (mca_btl_ugni_device_t *device, void *arg0)
{
    mca_btl_ugni_get_datagram_args_t *args = (mca_btl_ugni_get_datagram_args_t *) arg0;
    uint32_t remote_addr, remote_id;
    uint64_t datagram_id;
    gni_post_state_t post_state;
    gni_return_t grc;
    uint64_t data;

    grc = GNI_PostDataProbeById (device->dev_handle, &datagram_id);
    if (OPAL_LIKELY(GNI_RC_SUCCESS != grc)) {
        return 0;
    }

    data = datagram_id & ~(MCA_BTL_UGNI_DATAGRAM_MASK);

    BTL_VERBOSE(("rc: %d, datgram_id: %" PRIx64 ", mask: %" PRIx64, grc, datagram_id, (uint64_t) (datagram_id & MCA_BTL_UGNI_DATAGRAM_MASK)));

    if ((datagram_id & MCA_BTL_UGNI_DATAGRAM_MASK) == MCA_BTL_UGNI_CONNECT_DIRECTED_ID) {
        *(args->ep) = (mca_btl_base_endpoint_t *) opal_pointer_array_get_item (&args->ugni_module->endpoints, data);
        *(args->handle) = (*args->ep)->smsg_ep_handle->gni_handle;
    } else {
        *(args->handle) = args->ugni_module->wildcard_ep;
    }

    /* wait for the incoming datagram to complete (in case it isn't) */
    grc = GNI_EpPostDataWaitById (*args->handle, datagram_id, -1, &post_state,
                                  &remote_addr, &remote_id);
    if (GNI_RC_SUCCESS != grc) {
        BTL_ERROR(("GNI_EpPostDataWaitById failed with rc = %d", grc));
        return mca_btl_rc_ugni_to_opal (grc);
    }

    BTL_VERBOSE(("handled datagram completion. post_state: %d, remote_addr: %u, remote_id: %u, directed?: %d",
                 post_state, remote_addr, remote_id, (datagram_id & MCA_BTL_UGNI_DATAGRAM_MASK) == MCA_BTL_UGNI_CONNECT_DIRECTED_ID));

    return 1;
}

typedef struct mca_btl_ugni_reg_mem_args_t {
    mca_btl_ugni_module_t *ugni_module;
    void *base;
    size_t size;
    mca_btl_ugni_reg_t *ugni_reg;
    gni_cq_handle_t cq;
    int flags;
} mca_btl_ugni_reg_mem_args_t;

static intptr_t mca_btl_ugni_reg_mem_device (mca_btl_ugni_device_t *device, void *arg)
{
    mca_btl_ugni_reg_mem_args_t *args = (mca_btl_ugni_reg_mem_args_t *) arg;
    gni_return_t rc;

    rc = GNI_MemRegister (device->dev_handle, (uint64_t) args->base, args->size, args->cq,
                          args->flags, -1, &args->ugni_reg->handle.gni_handle);
    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != rc)) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    return OPAL_SUCCESS;
}

typedef struct mca_btl_ugni_dereg_mem_arg_t {
    mca_btl_ugni_module_t *ugni_module;
    mca_btl_ugni_reg_t *ugni_reg;
} mca_btl_ugni_dereg_mem_arg_t;

static intptr_t mca_btl_ugni_dereg_mem_device (mca_btl_ugni_device_t *device, void *arg)
{
    mca_btl_ugni_dereg_mem_arg_t *args = (mca_btl_ugni_dereg_mem_arg_t *) arg;
    gni_return_t rc;

    rc = GNI_MemDeregister (device->dev_handle, &args->ugni_reg->handle.gni_handle);
    return mca_btl_rc_ugni_to_opal (rc);
}

/* multi-thread safe interface to uGNI */

static inline int mca_btl_ugni_endpoint_smsg_send_wtag (mca_btl_base_endpoint_t *endpoint, void *hdr, size_t hdr_len,
                                                        void *payload, size_t payload_len, uint32_t msg_id, int tag)
{
    mca_btl_ugni_smsg_send_wtag_arg_t args = {.ep_handle = endpoint->smsg_ep_handle->gni_handle,
                                              .hdr = hdr, .hdr_len = hdr_len, .payload = payload,
                                              .payload_len = payload_len, .msg_id = msg_id,
                                              .tag = tag};
    mca_btl_ugni_device_t *device = endpoint->smsg_ep_handle->device;
    return (int) mca_btl_ugni_device_serialize (device, (mca_btl_ugni_device_serialize_fn_t) mca_btl_ugni_smsg_send_wtag_device, &args);
}

static inline int mca_btl_ugni_smsg_get_next_wtag (mca_btl_ugni_endpoint_handle_t *ep_handle, uintptr_t *data_ptr, uint8_t *tag)
{
    mca_btl_ugni_device_t *device = ep_handle->device;
    mca_btl_ugni_smsg_get_next_wtag_arg_t args = {.ep_handle = ep_handle->gni_handle, .data_ptr = data_ptr, .tag = tag};

    return (int) mca_btl_ugni_device_serialize (device, (mca_btl_ugni_device_serialize_fn_t) mca_btl_ugni_smsg_get_next_wtag_device, &args);
}

static inline int mca_btl_ugni_smsg_release (mca_btl_ugni_endpoint_handle_t *ep_handle)
{
    mca_btl_ugni_device_t *device = ep_handle->device;

    return (int) mca_btl_ugni_device_serialize (device, (mca_btl_ugni_device_serialize_fn_t) mca_btl_ugni_smsg_release_device, ep_handle);
}

static inline void mca_btl_ugni_cq_clear (mca_btl_ugni_device_t *device, gni_cq_handle_t cq)
{
    (void) mca_btl_ugni_device_serialize (device, (mca_btl_ugni_device_serialize_fn_t) mca_btl_ugni_cq_clear_device, (void *) (intptr_t) cq);
}

static inline int mca_btl_ugni_cq_get_event (mca_btl_ugni_device_t *device, mca_btl_ugni_cq_t *cq, gni_cq_entry_t *event_data)
{
    mca_btl_ugni_cq_get_event_args_t args = {.cq = cq, .event_data = event_data};
    return (int) mca_btl_ugni_device_serialize (device, (mca_btl_ugni_device_serialize_fn_t) mca_btl_ugni_cq_get_event_device, &args);
}

static inline int mca_btl_ugni_gni_cq_get_event (mca_btl_ugni_device_t *device, gni_cq_handle_t cq, gni_cq_entry_t *event_data)
{
    mca_btl_ugni_gni_cq_get_event_args_t args = {.cq = cq, .event_data = event_data};
    return (int) mca_btl_ugni_device_serialize (device, (mca_btl_ugni_device_serialize_fn_t) mca_btl_ugni_gni_cq_get_event_device, &args);
}

static inline int mca_btl_ugni_endpoint_post_fma (mca_btl_ugni_endpoint_t *endpoint, mca_btl_ugni_post_descriptor_t *desc)
{
    mca_btl_ugni_module_t *ugni_module = mca_btl_ugni_ep_btl (endpoint);
    mca_btl_ugni_device_t *device = desc->ep_handle ? desc->ep_handle->device : mca_btl_ugni_ep_get_device (ugni_module);
    return (int) mca_btl_ugni_device_serialize (device, (mca_btl_ugni_device_serialize_fn_t) mca_btl_ugni_post_fma_device, desc);
}

static inline int mca_btl_ugni_endpoint_post_rdma (mca_btl_ugni_endpoint_t *endpoint, mca_btl_ugni_post_descriptor_t *desc)
{
    mca_btl_ugni_module_t *ugni_module = mca_btl_ugni_ep_btl (endpoint);
    mca_btl_ugni_device_t *device = desc->ep_handle ? desc->ep_handle->device : mca_btl_ugni_ep_get_device (ugni_module);
    return (int) mca_btl_ugni_device_serialize (device, (mca_btl_ugni_device_serialize_fn_t) mca_btl_ugni_post_rdma_device, desc);
}

static inline int mca_btl_ugni_endpoint_post_cqwrite (mca_btl_ugni_endpoint_t *endpoint, mca_btl_ugni_post_descriptor_t *desc)
{
    mca_btl_ugni_module_t *ugni_module = mca_btl_ugni_ep_btl (endpoint);
    mca_btl_ugni_device_t *device = ugni_module->devices;
    return (int) mca_btl_ugni_device_serialize (device, (mca_btl_ugni_device_serialize_fn_t) mca_btl_ugni_post_cqwrite_device, desc);
}

static inline int mca_btl_ugni_cq_get_completed_desc (mca_btl_ugni_device_t *device, mca_btl_ugni_cq_t *cq,
                                                      gni_cq_entry_t *event_data, mca_btl_ugni_post_descriptor_t **post_desc,
                                                      int count)
{
    mca_btl_ugni_cq_get_completed_desc_arg_t args = {.cq = cq, .event_data = event_data, .post_desc = post_desc, .count = count};
    return (int) mca_btl_ugni_device_serialize (device, (mca_btl_ugni_device_serialize_fn_t) mca_btl_ugni_cq_get_completed_desc_device, &args);
}

static inline int mca_btl_ugni_get_datagram (mca_btl_ugni_module_t *ugni_module, mca_btl_ugni_device_t *device, gni_ep_handle_t *gni_handle,
                                             mca_btl_base_endpoint_t **ep)
{
    mca_btl_ugni_get_datagram_args_t args = {.ugni_module = ugni_module, .ep = ep, .handle = gni_handle};
    return (int) mca_btl_ugni_device_serialize (device, (mca_btl_ugni_device_serialize_fn_t) mca_btl_ugni_get_datagram_device, &args);
}

static inline int mca_btl_ugni_reg_mem (mca_btl_ugni_module_t *ugni_module, void *base, size_t size, mca_btl_ugni_reg_t *ugni_reg,
                                 gni_cq_handle_t cq, int flags)
{
    mca_btl_ugni_reg_mem_args_t args = {.ugni_module = ugni_module, .base = base, .size = size,
                                        .ugni_reg = ugni_reg, .cq = cq, .flags = flags};
    mca_btl_ugni_device_t *device = ugni_module->devices;
    return (int) mca_btl_ugni_device_serialize (device, (mca_btl_ugni_device_serialize_fn_t) mca_btl_ugni_reg_mem_device, &args);
}

static inline int mca_btl_ugni_dereg_mem (mca_btl_ugni_module_t *ugni_module, mca_btl_ugni_reg_t *ugni_reg)
{
    mca_btl_ugni_dereg_mem_arg_t args = {.ugni_module = ugni_module, .ugni_reg = ugni_reg};
    mca_btl_ugni_device_t *device = ugni_module->devices;
    return (int) mca_btl_ugni_device_serialize (device, (mca_btl_ugni_device_serialize_fn_t) mca_btl_ugni_dereg_mem_device, &args);
}

#endif /* BTL_UGNI_DEVICE_H */
