/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2018 Los Alamos National Security, LLC. All rights
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
/**
 * @brief Output an error message on CQ or completion error.
 *
 * @param[in] grc        GNI error from GNI_CqGetEvent or GNI_GetCompleted
 * @param[in] event_data event data from GNI_CqGetEvent
 *
 * This is a small function to print out an error if an error
 * was detected on a CQ event.
 */
int mca_btl_ugni_event_fatal_error (gni_return_t grc, gni_cq_entry_t event_data);

/**
 * @brief Attempt to re-post an rdma descriptor
 *
 * @param[in] rdma_desc  RDMA descriptor that failed
 * @param[in] event_data CQ event data
 *
 * @returns OPAL_SUCCESS if the descriptor was re-posted
 * @returns OPAL_ERROR otherwise
 *
 * This function checks if the error is recoverable and re-posts the
 * descriptor if possible. The device lock MUST be held when this
 * function is called.
 */
int mca_btl_ugni_device_handle_event_error (struct mca_btl_ugni_rdma_desc_t *rdma_desc, gni_cq_entry_t event_data);

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

typedef struct mca_btl_ugni_cq_get_event_args_t {
    mca_btl_ugni_cq_t *cq;
    gni_cq_entry_t *event_data;
} mca_btl_ugni_cq_get_event_args_t;

static inline intptr_t mca_btl_ugni_cq_get_event_device (mca_btl_ugni_device_t *device, void *arg)
{
    mca_btl_ugni_cq_get_event_args_t *args = (mca_btl_ugni_cq_get_event_args_t *) arg;
    gni_return_t rc;

    rc = GNI_CqGetEvent (args->cq->gni_handle, args->event_data);
    args->cq->active_operations -= (GNI_RC_NOT_DONE != rc);
    return rc;
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

typedef struct mca_btl_ugni_gni_cq_get_event_args_t {
    gni_cq_handle_t cq;
    gni_cq_entry_t *event_data;
} mca_btl_ugni_gni_cq_get_event_args_t;

static inline intptr_t mca_btl_ugni_gni_cq_get_event_device (mca_btl_ugni_device_t *device, void *arg)
{
    mca_btl_ugni_gni_cq_get_event_args_t *args = (mca_btl_ugni_gni_cq_get_event_args_t *) arg;

    return GNI_CqGetEvent (args->cq, args->event_data);
}

typedef struct mca_btl_ugni_cq_get_completed_desc_arg_t {
    mca_btl_ugni_cq_t *cq;
    mca_btl_ugni_post_descriptor_t *post_desc;
    int count;
} mca_btl_ugni_cq_get_completed_desc_arg_t;

__opal_attribute_always_inline__
static inline int _mca_btl_ugni_repost_rdma_desc_device (mca_btl_ugni_device_t *device, mca_btl_ugni_rdma_desc_t *rdma_desc)
{
    mca_btl_ugni_post_descriptor_t *post_desc = &rdma_desc->btl_ugni_desc;
    int rc;

    if (post_desc->use_bte) {
        rc = GNI_PostRdma (rdma_desc->gni_handle, &post_desc->gni_desc);
    } else {
        rc = GNI_PostFma (rdma_desc->gni_handle, &post_desc->gni_desc);
    }

    return mca_btl_rc_ugni_to_opal (rc);
}

static inline intptr_t _mca_btl_ugni_cq_get_completed_desc_device (mca_btl_ugni_device_t *device, mca_btl_ugni_cq_t *cq,
                                                                   mca_btl_ugni_post_descriptor_t *post_desc,
                                                                   const int count, bool block)
{
    mca_btl_ugni_rdma_desc_t *rdma_desc;
    gni_post_descriptor_t *desc;
    gni_cq_entry_t event_data;
    int rc, desc_index = 0;

    for (desc_index = 0 ; desc_index < count && cq->active_operations ; ) {
        int desc_rc = OPAL_SUCCESS;

        rc = GNI_CqGetEvent (cq->gni_handle, &event_data);
        if (GNI_RC_NOT_DONE == rc) {
	    if (block) {
		/* try again */
		continue;
	    }
            break;
        }

	block = false;

        rc = GNI_GetCompleted (cq->gni_handle, event_data, &desc);
        if (OPAL_UNLIKELY(GNI_RC_SUCCESS != rc && GNI_RC_TRANSACTION_ERROR != rc)) {
            return mca_btl_ugni_event_fatal_error (rc, event_data);
        }

        rdma_desc = MCA_BTL_UGNI_GNI_DESC_TO_RDMA_DESC(desc);

        if (OPAL_UNLIKELY(!GNI_CQ_STATUS_OK(event_data))) {
            desc_rc = mca_btl_ugni_device_handle_event_error (rdma_desc, event_data);
            if (OPAL_LIKELY(OPAL_SUCCESS == desc_rc)) {
                /* descriptor was re-posted */
                continue;
            }
        }

        /* copy back the descriptor only if additional processing is needed. in this case more processing
         * is needed if a user callback is specified or the bte was in use. */
        if (rdma_desc->btl_ugni_desc.cbfunc || rdma_desc->btl_ugni_desc.use_bte || OPAL_SUCCESS != desc_rc) {
            post_desc[desc_index] = rdma_desc->btl_ugni_desc;
            post_desc[desc_index++].rc = desc_rc;
        }

        /* return the descriptor while we have the lock. this is done so we can avoid using the
         * free list atomics (as both push and pop are done with the lock) */
        mca_btl_ugni_return_rdma_desc (rdma_desc);
        --cq->active_operations;
    }

    return desc_index;
}

static inline intptr_t mca_btl_ugni_cq_get_completed_desc_device (mca_btl_ugni_device_t *device, void *arg0)
{
    mca_btl_ugni_cq_get_completed_desc_arg_t *args = (mca_btl_ugni_cq_get_completed_desc_arg_t *) arg0;

    return _mca_btl_ugni_cq_get_completed_desc_device (device, args->cq, args->post_desc, args->count, false);
}

/* NTH: When posting FMA or RDMA descriptors it makes sense to try and clear out a completion
 * event after posting the descriptor. This probably gives us a couple of things:
 *   1) Good locality on the associated data structures (especially with FMA which may
 *      complete fairly quickly).
 *   2) Since we are already holding the lock it could mean fewer attempts to
 *      lock the device over the course of the program.
 *
 * As far as I can tell there is not reason to try and clear out more than a couple
 * completiong events. The code has been written to allow us to easily modify the
 * number reaped if we determine that there is a benefit to clearing a different
 * number of events. */

/**
 * @brief Number of events to clear after posting a descriptor
 */
#define MCA_BTL_UGNI_DEVICE_REAP_COUNT 4

struct mca_btl_ugni_post_device_args_t {
    mca_btl_ugni_post_descriptor_t *desc;
    mca_btl_ugni_device_t *device;
    int count;
    mca_btl_ugni_post_descriptor_t completed[MCA_BTL_UGNI_DEVICE_REAP_COUNT];
};

static inline mca_btl_ugni_rdma_desc_t *
mca_btl_ugni_get_rdma_desc_device (mca_btl_ugni_device_t *device, struct mca_btl_ugni_post_device_args_t *args, bool use_bte)
{
    mca_btl_ugni_post_descriptor_t *desc = args->desc;
    mca_btl_ugni_rdma_desc_t *rdma_desc;

    args->device = device;
    args->count = 0;

    do {
        rdma_desc = mca_btl_ugni_alloc_rdma_desc (device, desc, use_bte);
	if (OPAL_LIKELY(NULL != rdma_desc)) {
	    return rdma_desc;
	}

        if (OPAL_LIKELY(NULL == rdma_desc && !args->count)) {
	    args->count = _mca_btl_ugni_cq_get_completed_desc_device (device, &device->dev_rdma_local_cq,
								      args->completed, MCA_BTL_UGNI_DEVICE_REAP_COUNT,
								      true);
	    continue;
        }

	return NULL;
    } while (1);
}


static inline intptr_t mca_btl_ugni_post_fma_device (mca_btl_ugni_device_t *device, void *arg)
{
    struct mca_btl_ugni_post_device_args_t *args = (struct mca_btl_ugni_post_device_args_t *) arg;
    mca_btl_ugni_rdma_desc_t *rdma_desc;
    int rc;

    rdma_desc = mca_btl_ugni_get_rdma_desc_device (device, args, false);
    if (OPAL_UNLIKELY(NULL == rdma_desc)) {
	return OPAL_ERR_TEMP_OUT_OF_RESOURCE;
    }

    BTL_VERBOSE(("Posting FMA descriptor %p with op_type %d, amo %d, remote_addr 0x%lx, "
                 "length %lu", (void*)rdma_desc, rdma_desc->btl_ugni_desc.gni_desc.type, rdma_desc->btl_ugni_desc.gni_desc.amo_cmd,
                 rdma_desc->btl_ugni_desc.gni_desc.remote_addr, rdma_desc->btl_ugni_desc.gni_desc.length));

    rc = GNI_PostFma (rdma_desc->gni_handle, &rdma_desc->btl_ugni_desc.gni_desc);
    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != rc)) {
        mca_btl_ugni_return_rdma_desc (rdma_desc);
        return mca_btl_rc_ugni_to_opal (rc);
    }

    ++device->dev_rdma_local_cq.active_operations;

    /* to improve bandwidth and latency it is ideal for all posting threads to also reap completions from
     * the rdma completion queue. there are two optimizations here. 1) for bandwidth we only want to
     * reap what is available now so more messages can be posted quickly, and 2) for latency (single
     * put/get before flushing) we want to ensure the operation is complete. To some degree this is
     * gaming the benchmark but it may benefit some application communication patterns without really
     * hurting others (in theory). */
    if (opal_using_threads ()) {
	int count = args->count;
	args->count += _mca_btl_ugni_cq_get_completed_desc_device (device, &device->dev_rdma_local_cq,
								   args->completed + count,
								   MCA_BTL_UGNI_DEVICE_REAP_COUNT - count,
								   device->flushed);
	device->flushed = false;
    }

    return OPAL_SUCCESS;
}

static inline intptr_t mca_btl_ugni_post_rdma_device (mca_btl_ugni_device_t *device, void *arg)
{
    struct mca_btl_ugni_post_device_args_t *args = (struct mca_btl_ugni_post_device_args_t *) arg;
    mca_btl_ugni_rdma_desc_t *rdma_desc;
    int rc;

    rdma_desc = mca_btl_ugni_get_rdma_desc_device (device, args, true);
    if (OPAL_UNLIKELY(NULL == rdma_desc)) {
	return OPAL_ERR_TEMP_OUT_OF_RESOURCE;
    }

    /* pick the appropriate CQ */
    rdma_desc->btl_ugni_desc.cq = mca_btl_ugni_component.progress_thread_enabled ? &device->dev_rdma_local_irq_cq :
        &device->dev_rdma_local_cq;

    BTL_VERBOSE(("Posting RDMA descriptor %p with op_type %d, amo %d, remote_addr 0x%lx, "
                 "length %lu", (void*)rdma_desc, rdma_desc->btl_ugni_desc.gni_desc.type, rdma_desc->btl_ugni_desc.gni_desc.amo_cmd,
                 rdma_desc->btl_ugni_desc.gni_desc.remote_addr, rdma_desc->btl_ugni_desc.gni_desc.length));

    rc = GNI_PostRdma (rdma_desc->gni_handle, &rdma_desc->btl_ugni_desc.gni_desc);
    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != rc)) {
        mca_btl_ugni_return_rdma_desc (rdma_desc);
        return mca_btl_rc_ugni_to_opal (rc);
    }

    ++rdma_desc->btl_ugni_desc.cq->active_operations;

    /* to improve bandwidth and latency it is ideal for all posting threads to also reap completions from
     * the rdma completion queue. there are two optimizations here. 1) for bandwidth we only want to
     * reap what is available now so more messages can be posted quickly, and 2) for latency (single
     * put/get before flushing) we want to ensure the operation is complete. To some degree this is
     * gaming the benchmark but it may benefit some application communication patterns without really
     * hurting others (in theory). */
    if (opal_using_threads ()) {
	int count = args->count;
	args->count += _mca_btl_ugni_cq_get_completed_desc_device (device, &device->dev_rdma_local_cq,
								   args->completed + count,
								   MCA_BTL_UGNI_DEVICE_REAP_COUNT - count,
								   device->flushed);
	device->flushed = false;
    }

    return OPAL_SUCCESS;
}

static inline intptr_t mca_btl_ugni_post_cqwrite_device (mca_btl_ugni_device_t *device, void *arg)
{
    mca_btl_ugni_post_descriptor_t *desc = (mca_btl_ugni_post_descriptor_t *) arg;
    mca_btl_ugni_rdma_desc_t *rdma_desc;
    int rc;

    desc->gni_desc.src_cq_hndl = device->dev_rdma_local_cq.gni_handle;

    rdma_desc = mca_btl_ugni_alloc_rdma_desc (device, desc, false);
    if (OPAL_UNLIKELY(NULL == rdma_desc)) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    rc = GNI_PostCqWrite (rdma_desc->gni_handle, &rdma_desc->btl_ugni_desc.gni_desc);
    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != rc)) {
        mca_btl_ugni_return_rdma_desc (rdma_desc);
    }

    return mca_btl_rc_ugni_to_opal (rc);
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
        *(args->handle) = (*args->ep)->smsg_ep_handle.gni_handle;
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
    mca_btl_ugni_smsg_send_wtag_arg_t args = {.ep_handle = endpoint->smsg_ep_handle.gni_handle,
                                              .hdr = hdr, .hdr_len = hdr_len, .payload = payload,
                                              .payload_len = payload_len, .msg_id = msg_id,
                                              .tag = tag};
    mca_btl_ugni_device_t *device = endpoint->smsg_ep_handle.device;
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
    /* NTH: normally there would be a check for any outstanding CQ operations but there seems
     * to be a reason to check the local SMSG completion queue anyway. since this function
     * only handled the SMSG local completion queue not checking here should be fine and
     * should not impact performance. */
    return (int) mca_btl_ugni_device_serialize (device, (mca_btl_ugni_device_serialize_fn_t) mca_btl_ugni_cq_get_event_device, &args);
}

static inline int mca_btl_ugni_gni_cq_get_event (mca_btl_ugni_device_t *device, gni_cq_handle_t cq, gni_cq_entry_t *event_data)
{
    mca_btl_ugni_gni_cq_get_event_args_t args = {.cq = cq, .event_data = event_data};
    return (int) mca_btl_ugni_device_serialize (device, (mca_btl_ugni_device_serialize_fn_t) mca_btl_ugni_gni_cq_get_event_device, &args);
}

__opal_attribute_always_inline__
static inline int mca_btl_ugni_endpoint_post (mca_btl_ugni_endpoint_t *endpoint, mca_btl_ugni_post_descriptor_t *desc,
                                              mca_btl_ugni_device_serialize_fn_t post_fn)
{
    struct mca_btl_ugni_post_device_args_t args = {.desc = desc};
    mca_btl_ugni_module_t *ugni_module = mca_btl_ugni_ep_btl (endpoint);
    int rc;

    /* use serialize_any as it is responsible for binding devices to threads (if enabled). this generally
     * gives better performance as it reduces contention on any individual device. */
    rc = mca_btl_ugni_device_serialize_any (ugni_module, post_fn, &args);
    if (args.count) {
        mca_btl_ugni_handle_rdma_completions (ugni_module, args.device, args.completed, args.count);
    }

    return rc;
}

__opal_attribute_always_inline__
static inline int mca_btl_ugni_endpoint_post_fma (mca_btl_ugni_endpoint_t *endpoint, mca_btl_ugni_post_descriptor_t *desc)
{
    return mca_btl_ugni_endpoint_post (endpoint, desc, (mca_btl_ugni_device_serialize_fn_t) mca_btl_ugni_post_fma_device);
}

__opal_attribute_always_inline__
static inline int mca_btl_ugni_endpoint_post_rdma (mca_btl_ugni_endpoint_t *endpoint, mca_btl_ugni_post_descriptor_t *desc)
{
    return mca_btl_ugni_endpoint_post (endpoint, desc, (mca_btl_ugni_device_serialize_fn_t) mca_btl_ugni_post_rdma_device);
}

static inline int mca_btl_ugni_endpoint_post_cqwrite (mca_btl_ugni_endpoint_t *endpoint, mca_btl_ugni_post_descriptor_t *desc)
{
    mca_btl_ugni_module_t *ugni_module = mca_btl_ugni_ep_btl (endpoint);
    mca_btl_ugni_device_t *device = ugni_module->devices;
    return (int) mca_btl_ugni_device_serialize (device, (mca_btl_ugni_device_serialize_fn_t) mca_btl_ugni_post_cqwrite_device, desc);
}

__opal_attribute_always_inline__
static inline int mca_btl_ugni_cq_get_completed_desc (mca_btl_ugni_device_t *device, mca_btl_ugni_cq_t *cq,
                                                      mca_btl_ugni_post_descriptor_t *post_desc,
                                                      int count)
{
    mca_btl_ugni_cq_get_completed_desc_arg_t args = {.cq = cq, .post_desc = post_desc, .count = count};
    if (0 == cq->active_operations) {
        return 0;
    }

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
