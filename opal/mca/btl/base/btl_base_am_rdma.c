/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2020-2021 Google, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal/mca/btl/base/btl_base_am_rdma.h"
#include "opal/mca/btl/base/base.h"
#include "opal/mca/btl/base/btl_base_error.h"
#include "opal/mca/threads/mutex.h"

/**
 * @brief data for active-message atomics
 *
 * There is currently only one module but it is defined to allow
 * moving the data pointer into the associated BTL module.
 */
struct mca_btl_base_am_rdma_module_t {
    opal_object_t super;
    /** provides protection for multi-threaded access to module members */
    opal_mutex_t mutex;
    /** responses queued awaiting resources */
    opal_list_t queued_responses;
    /** queued initiator descriptors */
    opal_list_t queued_initiator_descriptors;
};
typedef struct mca_btl_base_am_rdma_module_t mca_btl_base_am_rdma_module_t;

static void mca_btl_base_am_rdma_module_init(mca_btl_base_am_rdma_module_t *module)
{
    OBJ_CONSTRUCT(&module->mutex, opal_mutex_t);
    OBJ_CONSTRUCT(&module->queued_responses, opal_list_t);
    OBJ_CONSTRUCT(&module->queued_initiator_descriptors, opal_list_t);
}

static void mca_btl_base_am_rdma_module_fini(mca_btl_base_am_rdma_module_t *module)
{
    OBJ_DESTRUCT(&module->mutex);
    OBJ_DESTRUCT(&module->queued_responses);
    OBJ_DESTRUCT(&module->queued_initiator_descriptors);
}

static OBJ_CLASS_INSTANCE(mca_btl_base_am_rdma_module_t, opal_object_t,
                          mca_btl_base_am_rdma_module_init, mca_btl_base_am_rdma_module_fini);

/**
 * @brief response header for an active-message RDMA/atomic operation
 */
struct mca_btl_base_rdma_response_hdr_t {
    /** context for the response */
    uint64_t context;
    /** initiator address */
    uint64_t initiator_address;
    /** size of response */
    uint64_t response_size;
    /** response data may follow. the size is implied by the size of the incoming
     * descriptor */
};
typedef struct mca_btl_base_rdma_response_hdr_t mca_btl_base_rdma_response_hdr_t;

/**
 * @brief type of active-message RDMA/atomic operation
 */
enum mca_btl_base_rdma_type_t {
    /** active-message put. May be implemented with send/recv or RDMA get
     * depending on the functions that the BTL implements. */
    MCA_BTL_BASE_AM_PUT,
    /** active-message get. May be implemented with send/recv or RDMA put
     * depending on the functions that the BTL implements. */
    MCA_BTL_BASE_AM_GET,
    /** active-message atomic. */
    MCA_BTL_BASE_AM_ATOMIC,
    /** compare-and-swap */
    MCA_BTL_BASE_AM_CAS,
};
typedef enum mca_btl_base_rdma_type_t mca_btl_base_rdma_type_t;

/**
 * @brief origin-side operation context for an active-message RDMA/atomic operation
 */
struct mca_btl_base_rdma_context_t {
    opal_object_t super;
    /** operation type */
    uint8_t type;
    /** user callback function */
    mca_btl_base_rdma_completion_fn_t cbfunc;
    /** callback context */
    void *cbcontext;
    /** callback data */
    void *cbdata;
    /** sent */
    uint64_t sent;
    /** total size */
    uint64_t total_size;
    /** size acknowledged by target */
    opal_atomic_int64_t acknowledged;
    /** btl descriptor used to sent AM request */
    mca_btl_base_descriptor_t *descriptor;
    /** local address for this request */
    void *local_address;
    /** remote address */
    uint64_t remote_address;
    /** local handle for this request */
    struct mca_btl_base_registration_handle_t *local_handle;
};
typedef struct mca_btl_base_rdma_context_t mca_btl_base_rdma_context_t;

static void mca_btl_base_rdma_context_init(mca_btl_base_rdma_context_t *context)
{
    context->sent = 0;
    context->acknowledged = 0;
    context->descriptor = NULL;
}

static OBJ_CLASS_INSTANCE(mca_btl_base_rdma_context_t, opal_object_t,
                          mca_btl_base_rdma_context_init, NULL);

/**
 * @brief queued initiator descriptor
 */
struct mca_btl_base_am_rdma_queued_descriptor_t {
    opal_list_item_t super;
    mca_btl_base_module_t *btl;
    struct mca_btl_base_endpoint_t *endpoint;
    mca_btl_base_descriptor_t *descriptor;
};
typedef struct mca_btl_base_am_rdma_queued_descriptor_t mca_btl_base_am_rdma_queued_descriptor_t;

static OBJ_CLASS_INSTANCE(mca_btl_base_am_rdma_queued_descriptor_t, opal_list_item_t, NULL, NULL);

/**
 * @brief header for an active-message atomic/RDMA operation
 */
struct mca_btl_base_rdma_hdr_t {
    /** type of operation requested. */
    uint8_t type;
    uint8_t padding[3];

    union {
        struct {
            /** atomic operation type */
            int8_t op;

            /** operation size (bytes) */
            uint8_t size;

            uint8_t padding[2];

            /** atomic operands */
            int64_t operand[2];
        } atomic;
        struct {
            /** indicates whether the target should use RDMA (get or put) for this operation */
            uint8_t use_rdma;

            uint8_t padding[3];

            /** current operation size */
            uint64_t size;

            /** address on initiator */
            uint64_t initiator_address;
        } rdma;
    } data;

    /** address on target */
    uint64_t target_address;

    /* the following fields are not used on the target and are only relevant
     * to the initiator */
    uint64_t context;

    /* registration handles (if required) */
    uint8_t handle_data[];
};
typedef struct mca_btl_base_rdma_hdr_t mca_btl_base_rdma_hdr_t;

/**
 * @brief target-side RDMA/atomic operation
 */
struct mca_btl_base_rdma_operation_t {
    /** these may be stored in lists */
    opal_list_item_t super;
    /** btl module associated with this operation */
    mca_btl_base_module_t *btl;
    /** endpoint used to communicate with the origin */
    struct mca_btl_base_endpoint_t *endpoint;
    /** response descriptor (if allocated). this will be stored
     * if the send operation was unsuccessfull and the response
     * needs to be retried. */
    mca_btl_base_descriptor_t *descriptor;
    /** incoming operation header */
    mca_btl_base_rdma_hdr_t hdr;
    /** local memory handle (if using RDMA) */
    uint8_t local_handle_data[MCA_BTL_REG_HANDLE_MAX_SIZE];
    /** remote memory handle (if using RMDA) */
    uint8_t remote_handle_data[MCA_BTL_REG_HANDLE_MAX_SIZE];
    /** result of the atomic operation */
    uint64_t atomic_response;
    /** rdma/atomic operation is queued for response */
    bool is_queued;
    /** rdma operation was completed (waiting response) */
    bool is_completed;
};
typedef struct mca_btl_base_rdma_operation_t mca_btl_base_rdma_operation_t;

static OBJ_CLASS_INSTANCE(mca_btl_base_rdma_operation_t, opal_list_item_t, NULL, NULL);

static inline size_t size_t_min(size_t a, size_t b)
{
    return (a < b) ? a : b;
}

static inline size_t size_t_max(size_t a, size_t b)
{
    return (a > b) ? a : b;
}

static mca_btl_base_am_rdma_module_t default_module;

static inline bool mca_btl_base_rdma_use_rdma_get(mca_btl_base_module_t *btl)
{
    return !!(btl->btl_flags & MCA_BTL_FLAGS_GET);
}

static inline bool mca_btl_base_rdma_use_rdma_put(mca_btl_base_module_t *btl)
{
    return !!(btl->btl_flags & MCA_BTL_FLAGS_PUT);
}

static inline bool mca_btl_base_rdma_is_atomic(mca_btl_base_rdma_type_t type)
{
    return (MCA_BTL_BASE_AM_PUT != type && MCA_BTL_BASE_AM_GET != type);
}

static inline size_t mca_btl_base_rdma_operation_size(mca_btl_base_module_t *btl,
                                                      mca_btl_base_rdma_type_t type,
                                                      size_t remaining)
{
    switch (type) {
    case MCA_BTL_BASE_AM_PUT:
        if (mca_btl_base_rdma_use_rdma_get(btl)) {
            return size_t_min(remaining, btl->btl_get_limit);
        }
        break;
    case MCA_BTL_BASE_AM_GET:
        if (mca_btl_base_rdma_use_rdma_put(btl)) {
            return size_t_min(remaining, btl->btl_put_limit);
        }
        break;
    case MCA_BTL_BASE_AM_ATOMIC:
        /* fall through */
    case MCA_BTL_BASE_AM_CAS:
        return remaining;
    }

    return size_t_min(remaining, btl->btl_max_send_size - sizeof(mca_btl_base_rdma_hdr_t));
}

static inline int mca_btl_base_rdma_tag(mca_btl_base_rdma_type_t type)
{
    (void) type;
    switch (type) {
    case MCA_BTL_BASE_AM_PUT:
    case MCA_BTL_BASE_AM_GET:
        return MCA_BTL_BASE_TAG_RDMA;
    case MCA_BTL_BASE_AM_ATOMIC:
    case MCA_BTL_BASE_AM_CAS:
        return MCA_BTL_BASE_TAG_ATOMIC;
    }
    return MCA_BTL_BASE_TAG_RDMA_RESP;
}

static inline int mca_btl_base_rdma_resp_tag(void)
{
    return MCA_BTL_BASE_TAG_RDMA_RESP;
}

/**
 * @brief copy data from a segment to a local address
 *
 * @in addr          location to store data
 * @in skip_bytes    bytes of segment data to skip before copying
 * @in segments      segments to copy data from
 * @in segment_count number of segments
 */
static void mca_btl_base_copy_from_segments(uint64_t addr, size_t skip_bytes,
                                            const mca_btl_base_segment_t *segments,
                                            size_t segment_count)
{
    const void *seg0_data = (const void *) ((uintptr_t) segments[0].seg_addr.pval + skip_bytes);
    size_t seg0_len = segments[0].seg_len - skip_bytes;

    if (seg0_len > 0) {
        BTL_VERBOSE(
            ("unpacking %" PRIsize_t " bytes from segment 0 to %p", seg0_len, (void *) addr));
        memcpy((void *) addr, seg0_data, seg0_len);
        addr += seg0_len;
    }

    for (size_t i = 1; i < segment_count; ++i) {
        size_t seg_len = segments[i].seg_len;
        BTL_VERBOSE(("unpacking %" PRIsize_t " bytes from segment %" PRIsize_t " to %p", seg_len, i,
                     (void *) addr));
        memcpy((void *) addr, segments[i].seg_addr.pval, seg_len);
        addr += seg_len;
    }
}

/**
 * @brief copy data from a local address into a segment
 *
 * @in addr          location to read data from
 * @in skip_bytes    bytes of segment data to skip before copying
 * @in segments      segments to copy data to
 * @in segment_count number of segments
 */
static void mca_btl_base_copy_to_segments(uint64_t addr, size_t max_len, size_t skip_bytes,
                                          mca_btl_base_segment_t *segments, size_t segment_count)
{
    void *seg0_data = (void *) ((uintptr_t) segments[0].seg_addr.pval + skip_bytes);
    size_t seg0_len = size_t_min(max_len, segments[0].seg_len - skip_bytes);

    if (seg0_len > 0) {
        BTL_VERBOSE(
            ("packing %" PRIsize_t " bytes from 0x%" PRIx64 " to segment 0", seg0_len, addr));
        memcpy(seg0_data, (const void *) addr, seg0_len);
        addr += seg0_len;
        max_len -= seg0_len;
        segments[0].seg_len = seg0_len + skip_bytes;
    }

    for (size_t i = 1; i < segment_count && max_len; ++i) {
        size_t seg_len = size_t_min(segments[i].seg_len, max_len);

        BTL_VERBOSE(("packing %" PRIsize_t " bytes from 0x%" PRIx64 " to segment %" PRIsize_t,
                     seg_len, addr, i));

        memcpy(segments[i].seg_addr.pval, (const void *) addr, seg_len);
        segments[i].seg_len = seg_len;

        addr += seg_len;
        max_len -= seg_len;
    }
}

static void mca_btl_base_am_queue_initiator_descriptor(mca_btl_base_module_t *btl,
                                                       struct mca_btl_base_endpoint_t *endpoint,
                                                       mca_btl_base_descriptor_t *descriptor)
{
    mca_btl_base_am_rdma_queued_descriptor_t *queued_descriptor = OBJ_NEW(
        mca_btl_base_am_rdma_queued_descriptor_t);

    queued_descriptor->btl = btl;
    queued_descriptor->endpoint = endpoint;
    queued_descriptor->descriptor = descriptor;

    OPAL_THREAD_SCOPED_LOCK(&default_module.mutex,
                            opal_list_append(&default_module.queued_initiator_descriptors,
                                             &queued_descriptor->super));
}

static inline int mca_btl_base_am_rdma_advance(mca_btl_base_module_t *btl,
                                               struct mca_btl_base_endpoint_t *endpoint,
                                               mca_btl_base_rdma_context_t *context,
                                               bool send_descriptor)
{
    const size_t remaining = context->total_size - context->sent;

    if (0 == remaining) {
        if (context->descriptor) {
            btl->btl_free(btl, context->descriptor);
            context->descriptor = NULL;
        }

        /* release the reference retained at context creation */
        OBJ_RELEASE(context);
        /* nothing more needs to be done */
        return OPAL_SUCCESS;
    }

    mca_btl_base_descriptor_t *descriptor = context->descriptor;
    mca_btl_base_rdma_hdr_t *hdr = (mca_btl_base_rdma_hdr_t *) descriptor->des_segments[0]
                                       .seg_addr.pval;
    const size_t packet_size = mca_btl_base_rdma_operation_size(btl, hdr->type, remaining);

    if (!mca_btl_base_rdma_is_atomic(hdr->type)) {
        hdr->data.rdma.size = packet_size;
        hdr->data.rdma.initiator_address = (uint64_t) context->local_address + context->sent;
    } else {
        hdr->data.atomic.size = packet_size;
    }

    hdr->target_address = context->remote_address + context->sent;

    context->sent += packet_size;

    if (MCA_BTL_BASE_AM_PUT == hdr->type && !hdr->data.rdma.use_rdma) {
        /* copy the next block into the fragment buffer */
        mca_btl_base_copy_to_segments(hdr->data.rdma.initiator_address, packet_size, sizeof(*hdr),
                                      descriptor->des_segments, descriptor->des_segment_count);
    }

    if (send_descriptor) {
        return btl->btl_send(btl, endpoint, descriptor, mca_btl_base_rdma_tag(hdr->type));
    }

    /* queue for later to avoid btl_send in callback */
    mca_btl_base_am_queue_initiator_descriptor(btl, endpoint, descriptor);

    return OPAL_SUCCESS;
}

static void mca_btl_base_am_descriptor_complete(mca_btl_base_module_t *btl,
                                                struct mca_btl_base_endpoint_t *endpoint,
                                                mca_btl_base_descriptor_t *descriptor, int status)
{
    (void) mca_btl_base_am_rdma_advance(btl, endpoint,
                                        (mca_btl_base_rdma_context_t *) descriptor->des_context,
                                        /*send_descriptor=*/false);
}

static inline int
mca_btl_base_rdma_start(mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                        int type, uint64_t operand1, uint64_t operand2, mca_btl_base_atomic_op_t op,
                        int order, int flags, size_t size, void *local_address,
                        mca_btl_base_registration_handle_t *local_handle, int64_t remote_address,
                        mca_btl_base_registration_handle_t *remote_handle,
                        mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata)
{
    mca_btl_base_rdma_hdr_t *hdr;
    size_t packet_size = sizeof(*hdr);
    mca_btl_base_descriptor_t *descriptor;
    mca_btl_base_rdma_context_t *context = OBJ_NEW(mca_btl_base_rdma_context_t);

    if (OPAL_UNLIKELY(NULL == context)) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    context->type = type;
    context->cbfunc = cbfunc;
    context->cbcontext = cbcontext;
    context->cbdata = cbdata;
    context->local_address = local_address;
    context->remote_address = remote_address;
    context->local_handle = local_handle;
    context->total_size = size;

    bool use_rdma = false;

    if (MCA_BTL_BASE_AM_PUT == type) {
        if (sizeof(*hdr) + size <= btl->btl_eager_limit) {
            /* just go ahead and send the data */
            packet_size += size;
        } else if (!mca_btl_base_rdma_use_rdma_get(btl)) {
            packet_size += size;
        } else if (!mca_btl_base_rdma_use_rdma_get (btl)) {
            packet_size += size_t_min (size, btl->btl_max_send_size - sizeof (*hdr));
        } else {
            use_rdma = true;
        }
    } else if (MCA_BTL_BASE_AM_GET == type) {
        if (sizeof(mca_btl_base_rdma_response_hdr_t) + size <= btl->btl_eager_limit) {
            packet_size += size;
        } else if (!mca_btl_base_rdma_use_rdma_put(btl)) {
            packet_size += size_t_min(size, btl->btl_max_send_size
                                             - sizeof(mca_btl_base_rdma_response_hdr_t));
        } else {
            use_rdma = true;
        }
    } else {
        /* fetching atomic. always recv result via active message */
        packet_size += size;
    }

    if (use_rdma && btl->btl_register_mem) {
        packet_size += 2 * btl->btl_registration_handle_size;
    }

    BTL_VERBOSE(("Initiating RDMA operation. context=%p, size=%" PRIsize_t
                 ", packet_size=%" PRIsize_t,
                 context, size, packet_size));

    descriptor = btl->btl_alloc(btl, endpoint, order, packet_size,
                                MCA_BTL_DES_SEND_ALWAYS_CALLBACK | MCA_BTL_DES_FLAGS_SIGNAL);
    if (OPAL_UNLIKELY(NULL == descriptor)) {
        OBJ_RELEASE(context);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    context->descriptor = descriptor;

    /* keep a reference around until the descriptor callback is complete. the initial reference may
     * be released on response before the descriptor callback has completed. */
    OBJ_RETAIN(context);

    descriptor->des_cbfunc = mca_btl_base_am_descriptor_complete;
    descriptor->des_cbdata = local_handle;
    descriptor->des_context = context;

    hdr = (mca_btl_base_rdma_hdr_t *) descriptor->des_segments[0].seg_addr.pval;
    hdr->type = type;

    if (!mca_btl_base_rdma_is_atomic(type)) {
        hdr->data.rdma.use_rdma = use_rdma;
    } else {
        hdr->data.atomic.op = op;
        hdr->data.atomic.operand[0] = operand1;
        hdr->data.atomic.operand[1] = operand2;
    }

    hdr->context = (uintptr_t) context;

    if (use_rdma && btl->btl_register_mem) {
        uint8_t *handle_buffer = (uint8_t *) (hdr + 1);
        memcpy(handle_buffer, local_handle, btl->btl_registration_handle_size);
        handle_buffer += btl->btl_registration_handle_size;
        memcpy(handle_buffer, remote_handle, btl->btl_registration_handle_size);
    }

    return mca_btl_base_am_rdma_advance(btl, endpoint, context, /*send_descriptor=*/true);
}

static mca_btl_base_rdma_operation_t *mca_btl_base_rdma_alloc_operation(
    mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
    mca_btl_base_descriptor_t *descriptor, const mca_btl_base_rdma_hdr_t *hdr)
{
    mca_btl_base_rdma_operation_t *operation = OBJ_NEW(mca_btl_base_rdma_operation_t);
    if (NULL == operation) {
        return NULL;
    }

    operation->btl = btl;
    operation->endpoint = endpoint;
    operation->descriptor = descriptor;
    operation->is_completed = false;
    operation->is_queued = false;
    memcpy(&operation->hdr, hdr, sizeof(*hdr));

    if (!mca_btl_base_rdma_is_atomic(hdr->type) && hdr->data.rdma.use_rdma
        && btl->btl_register_mem) {
        const uint8_t *handle_data = (const uint8_t *) (hdr + 1);
        /* the initiator packs these in order of their local and then remote. */
        memcpy(operation->remote_handle_data, handle_data, btl->btl_registration_handle_size);
        handle_data += btl->btl_registration_handle_size;
        memcpy(operation->local_handle_data, handle_data, btl->btl_registration_handle_size);
    }

    return operation;
}

static void mca_btl_base_rdma_queue_operation(mca_btl_base_module_t *btl,
                                              struct mca_btl_base_endpoint_t *endpoint,
                                              mca_btl_base_descriptor_t *descriptor,
                                              uint64_t atomic_response,
                                              const mca_btl_base_rdma_hdr_t *hdr,
                                              mca_btl_base_rdma_operation_t *operation)
{
    if (NULL == operation) {
        operation = mca_btl_base_rdma_alloc_operation(btl, endpoint, descriptor, hdr);
        if (NULL == operation) {
            /* couldn't even allocate a small amount of memory. not much else can be done. */
            BTL_ERROR(("could not allocate memory to queue active-message RDMA operation"));
            abort();
        }
    }

    operation->is_queued = true;
    operation->atomic_response = atomic_response;
    OPAL_THREAD_SCOPED_LOCK(&default_module.mutex,
                            opal_list_append(&default_module.queued_responses, &operation->super));
}

static int mca_btl_base_am_rdma_respond(mca_btl_base_module_t *btl,
                                        struct mca_btl_base_endpoint_t *endpoint,
                                        mca_btl_base_descriptor_t **descriptor, void *addr,
                                        const mca_btl_base_rdma_hdr_t *hdr)
{
    mca_btl_base_descriptor_t *send_descriptor = *descriptor;
    *descriptor = NULL;

    if (NULL == send_descriptor) {
        mca_btl_base_rdma_response_hdr_t *resp_hdr;
        size_t data_size = mca_btl_base_rdma_is_atomic(hdr->type) ? hdr->data.atomic.size
                                                                  : hdr->data.rdma.size;
        size_t packet_size = sizeof(*resp_hdr) + (addr ? data_size : 0);
        send_descriptor = btl->btl_alloc(btl, endpoint, MCA_BTL_NO_ORDER, packet_size,
                                         MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
        if (NULL == send_descriptor) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }

        resp_hdr = (mca_btl_base_rdma_response_hdr_t *) send_descriptor->des_segments[0]
                       .seg_addr.pval;
        resp_hdr->context = hdr->context;
        if (MCA_BTL_BASE_AM_GET == hdr->type) {
            resp_hdr->initiator_address = hdr->data.rdma.initiator_address;
        } else {
            /* initiator will determine the address */
            resp_hdr->initiator_address = 0;
        }
        resp_hdr->response_size = data_size;

        if (NULL != addr) {
            mca_btl_base_copy_to_segments((uint64_t)(uintptr_t) addr, packet_size,
                                          sizeof(*resp_hdr), send_descriptor->des_segments,
                                          send_descriptor->des_segment_count);
        }
    }

    BTL_VERBOSE(("sending descriptor %p", send_descriptor));

    send_descriptor->des_cbfunc = NULL;

    int ret = btl->btl_send(btl, endpoint, send_descriptor, mca_btl_base_rdma_resp_tag());
    if (OPAL_UNLIKELY(OPAL_SUCCESS != ret)) {
        *descriptor = send_descriptor;
    }
    return ret;
}

static void
mca_btl_base_am_rmda_rdma_complete(mca_btl_base_module_t *btl,
                                   struct mca_btl_base_endpoint_t *endpoint, void *local_address,
                                   struct mca_btl_base_registration_handle_t *local_handle,
                                   void *context, void *cbdata, int status)
{
    mca_btl_base_rdma_operation_t *operation = (mca_btl_base_rdma_operation_t *) context;

    BTL_VERBOSE(("BTL RDMA operation complete. status=%d", status));

    assert(OPAL_SUCCESS == status);

    operation->is_completed = true;
    int ret = mca_btl_base_am_rdma_respond(operation->btl, operation->endpoint,
                                           &operation->descriptor, NULL, &operation->hdr);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != ret)) {
        BTL_VERBOSE(
            ("could not send a response. queueing the response for later. endpoint=%p, ret=%d",
             endpoint, ret));
        mca_btl_base_rdma_queue_operation(btl, NULL, NULL, 0, NULL, operation);
    }

    OBJ_RELEASE(operation);
}

static int mca_btl_base_am_rdma_target_get(mca_btl_base_module_t *btl,
                                           struct mca_btl_base_endpoint_t *endpoint,
                                           mca_btl_base_descriptor_t **descriptor,
                                           void *target_address, const mca_btl_base_rdma_hdr_t *hdr,
                                           mca_btl_base_rdma_operation_t **operation)
{
    if (hdr->data.rdma.use_rdma) {
        if (NULL == *operation) {
            *operation = mca_btl_base_rdma_alloc_operation(btl, endpoint, *descriptor, hdr);
            if (NULL == *operation) {
                return OPAL_ERR_OUT_OF_RESOURCE;
            }
        }

        /* btl supports put but not get. emulating get with put */
        OBJ_RETAIN(*operation);
        int ret = btl->btl_put(
            btl, endpoint, target_address, hdr->data.rdma.initiator_address,
            (struct mca_btl_base_registration_handle_t *) (*operation)->local_handle_data,
            (struct mca_btl_base_registration_handle_t *) (*operation)->remote_handle_data,
            hdr->data.rdma.size, /*flags=*/0, MCA_BTL_NO_ORDER, mca_btl_base_am_rmda_rdma_complete,
            *operation, NULL);
        if (OPAL_SUCCESS != ret) {
            OBJ_RELEASE(*operation);
        }
        if (OPAL_ERR_NOT_AVAILABLE != ret) {
            return ret;
        }
    }

    return mca_btl_base_am_rdma_respond(btl, endpoint, descriptor, target_address, hdr);
}

static int mca_btl_base_am_rdma_target_put(mca_btl_base_module_t *btl,
                                           struct mca_btl_base_endpoint_t *endpoint,
                                           mca_btl_base_descriptor_t **descriptor,
                                           const mca_btl_base_segment_t *segments,
                                           size_t segment_count, void *target_address,
                                           const mca_btl_base_rdma_hdr_t *hdr,
                                           mca_btl_base_rdma_operation_t **operation)
{
    if (hdr->data.rdma.use_rdma) {
        if (NULL == *operation) {
            *operation = mca_btl_base_rdma_alloc_operation(btl, endpoint, *descriptor, hdr);
            if (NULL == *operation) {
                return OPAL_ERR_OUT_OF_RESOURCE;
            }
        }

        /* btl supports put but not get. emulating get with put */
        OBJ_RETAIN(*operation);
        int ret = btl->btl_get(
            btl, endpoint, target_address, hdr->data.rdma.initiator_address,
            (struct mca_btl_base_registration_handle_t *) (*operation)->local_handle_data,
            (struct mca_btl_base_registration_handle_t *) (*operation)->remote_handle_data,
            hdr->data.rdma.size, /*flags=*/0, MCA_BTL_NO_ORDER, mca_btl_base_am_rmda_rdma_complete,
            operation, NULL);
        if (OPAL_SUCCESS != ret) {
            OBJ_RELEASE(*operation);
        }

        if (OPAL_ERR_NOT_AVAILABLE != ret) {
            return ret;
        }
    } else if (NULL != segments) {
        mca_btl_base_copy_from_segments(hdr->target_address, sizeof(*hdr), segments, segment_count);
    }

    return mca_btl_base_am_rdma_respond(btl, endpoint, descriptor, NULL, hdr);
}

static void mca_btl_base_rdma_retry_operation(mca_btl_base_rdma_operation_t *operation)
{
    void *target_address = (void *) (intptr_t) operation->hdr.target_address;
    int ret = OPAL_SUCCESS;

    if (!operation->descriptor && !operation->is_completed) {
        switch (operation->hdr.type) {
        case MCA_BTL_BASE_AM_GET:
            ret = mca_btl_base_am_rdma_target_get(operation->btl, operation->endpoint,
                                                  &operation->descriptor, target_address,
                                                  &operation->hdr, &operation);
            break;
        case MCA_BTL_BASE_AM_PUT:
            ret = mca_btl_base_am_rdma_target_put(operation->btl, operation->endpoint,
                                                  &operation->descriptor,
                                                  /*segments=*/NULL,
                                                  /*segment_count=*/0, target_address,
                                                  &operation->hdr, &operation);
            break;
        case MCA_BTL_BASE_AM_ATOMIC:
            /* atomic operation was completed */
            ret = mca_btl_base_am_rdma_respond(operation->btl, operation->endpoint,
                                               &operation->descriptor, &operation->atomic_response,
                                               &operation->hdr);
            break;
        }
    } else {
        ret = mca_btl_base_am_rdma_respond(operation->btl, operation->endpoint,
                                           &operation->descriptor,
                                           /*addr=*/NULL, /*hdr=*/NULL);
    }

    if (OPAL_SUCCESS == ret) {
        if (operation->is_queued) {
            opal_list_remove_item(&default_module.queued_responses, &operation->super);
        }

        OBJ_RELEASE(operation);
    }
}

static int mca_btl_base_am_rdma_progress(void)
{
    if (0 == opal_list_get_size(&default_module.queued_responses)
        && 0 == opal_list_get_size(&default_module.queued_initiator_descriptors)) {
        return 0;
    }

    OPAL_THREAD_SCOPED_LOCK(&default_module.mutex, ({
        mca_btl_base_rdma_operation_t *operation, *next;
        OPAL_LIST_FOREACH_SAFE (operation, next, &default_module.queued_responses,
                                mca_btl_base_rdma_operation_t) {
            mca_btl_base_rdma_retry_operation(operation);
        }
    }));

    OPAL_THREAD_SCOPED_LOCK(&default_module.mutex, ({
        mca_btl_base_am_rdma_queued_descriptor_t *descriptor, *next;
        OPAL_LIST_FOREACH_SAFE (descriptor, next, &default_module.queued_initiator_descriptors,
                                mca_btl_base_am_rdma_queued_descriptor_t) {
            mca_btl_base_rdma_context_t *context = (mca_btl_base_rdma_context_t *)
                                                       descriptor->descriptor->des_context;
            int ret = descriptor->btl->btl_send(descriptor->btl, descriptor->endpoint,
                                                descriptor->descriptor,
                                                mca_btl_base_rdma_tag(context->type));
            if (OPAL_SUCCESS == ret) {
                opal_list_remove_item(&default_module.queued_initiator_descriptors,
                                      &descriptor->super);
            }
        }
    }));

    return 0;
}

static int mca_btl_base_am_atomic_64(int64_t *operand, opal_atomic_int64_t *addr,
                                     mca_btl_base_atomic_op_t op)
{
    int64_t result = 0;

    switch (op) {
    case MCA_BTL_ATOMIC_ADD:
        result = opal_atomic_fetch_add_64(addr, *operand);
        break;
    case MCA_BTL_ATOMIC_AND:
        result = opal_atomic_fetch_and_64(addr, *operand);
        break;
    case MCA_BTL_ATOMIC_OR:
        result = opal_atomic_fetch_or_64(addr, *operand);
        break;
    case MCA_BTL_ATOMIC_XOR:
        result = opal_atomic_fetch_xor_64(addr, *operand);
        break;
    case MCA_BTL_ATOMIC_SWAP:
        result = opal_atomic_swap_64(addr, *operand);
        break;
    case MCA_BTL_ATOMIC_MIN:
        result = opal_atomic_fetch_min_64(addr, *operand);
        break;
    case MCA_BTL_ATOMIC_MAX:
        result = opal_atomic_fetch_max_64(addr, *operand);
        break;
    default:
        return OPAL_ERR_BAD_PARAM;
    }

    *operand = result;
    return OPAL_SUCCESS;
}

static int mca_btl_base_am_atomic_32(int32_t *operand, opal_atomic_int32_t *addr,
                                     mca_btl_base_atomic_op_t op)
{
    int32_t result = 0;

    switch (op) {
    case MCA_BTL_ATOMIC_ADD:
        result = opal_atomic_fetch_add_32(addr, *operand);
        break;
    case MCA_BTL_ATOMIC_AND:
        result = opal_atomic_fetch_and_32(addr, *operand);
        break;
    case MCA_BTL_ATOMIC_OR:
        result = opal_atomic_fetch_or_32(addr, *operand);
        break;
    case MCA_BTL_ATOMIC_XOR:
        result = opal_atomic_fetch_xor_32(addr, *operand);
        break;
    case MCA_BTL_ATOMIC_SWAP:
        result = opal_atomic_swap_32(addr, *operand);
        break;
    case MCA_BTL_ATOMIC_MIN:
        result = opal_atomic_fetch_min_32(addr, *operand);
        break;
    case MCA_BTL_ATOMIC_MAX:
        result = opal_atomic_fetch_max_32(addr, *operand);
        break;
    default:
        return OPAL_ERR_BAD_PARAM;
    }

    *operand = result;
    return OPAL_SUCCESS;
}

static void mca_btl_base_am_rdma_response(mca_btl_base_module_t *btl,
                                          const mca_btl_base_receive_descriptor_t *desc)
{
    mca_btl_base_rdma_response_hdr_t *resp_hdr = (mca_btl_base_rdma_response_hdr_t *) desc
                                                     ->des_segments[0]
                                                     .seg_addr.pval;

    assert(desc->des_segments[0].seg_len >= sizeof(*resp_hdr));

    mca_btl_base_rdma_context_t *context = (mca_btl_base_rdma_context_t *) (uintptr_t)
                                               resp_hdr->context;

    BTL_VERBOSE(("received response for RDMA operation. context=%p, size=%" PRIu64, context,
                 resp_hdr->response_size));

    if (MCA_BTL_BASE_AM_PUT != context->type) {
        uint64_t local_address = resp_hdr->initiator_address ? resp_hdr->initiator_address
                                                             : (uintptr_t) context->local_address;
        if (local_address && MCA_BTL_BASE_AM_PUT != context->type) {
            BTL_VERBOSE(("unpacking response from packet"));
            /* if there is a result copy it out of the incoming buffer. if RDMA is being used
             * (get/put or put/get) then the header should be the only thing in the incoming
             * message. */
            mca_btl_base_copy_from_segments(local_address, sizeof(*resp_hdr), desc->des_segments,
                                            desc->des_segment_count);
        }
    }

    if (context->total_size
        == opal_atomic_add_fetch_64(&context->acknowledged, resp_hdr->response_size)) {
        context->cbfunc(btl, desc->endpoint, context->local_address, context->local_handle,
                        context->cbcontext, context->cbdata, OPAL_SUCCESS);
        OBJ_RELEASE(context);
    }
}

static void mca_btl_base_am_process_rdma(mca_btl_base_module_t *btl,
                                         const mca_btl_base_receive_descriptor_t *desc)
{
    /* not all btls work with these active message atomics. at this time
     * all of the affected btls already have atomic support so there is
     * no need to attempt to get the endpoint here. */
    if (NULL == desc->endpoint) {
        BTL_ERROR(("BTL is not compatible with active-message RDMA"));
        abort();
    }

    const mca_btl_base_rdma_hdr_t *hdr = (mca_btl_base_rdma_hdr_t *) desc->des_segments[0]
                                             .seg_addr.pval;
    void *target_address = (void *) (intptr_t) hdr->target_address;
    mca_btl_base_descriptor_t *descriptor = NULL;
    mca_btl_base_rdma_operation_t *operation = NULL;
    int ret;

    BTL_VERBOSE(("got active-message \"RDMA\" request. hdr->context=0x%" PRIx64
                 ", target_address=%p, "
                 "segment 0 size=%" PRIu64,
                 hdr->context, target_address, desc->des_segments[0].seg_len));

    if (MCA_BTL_BASE_AM_PUT == hdr->type) {
        ret = mca_btl_base_am_rdma_target_put(btl, desc->endpoint, &descriptor, desc->des_segments,
                                              desc->des_segment_count, target_address, hdr,
                                              &operation);
    } else if (MCA_BTL_BASE_AM_GET == hdr->type) {
        ret = mca_btl_base_am_rdma_target_get(btl, desc->endpoint, &descriptor, target_address, hdr,
                                              &operation);
    } else {
        BTL_ERROR(("Unexpected tag when processing active-message RDMA request"));
        abort();
    }

    if (OPAL_SUCCESS != ret) {
        mca_btl_base_rdma_queue_operation(btl, desc->endpoint, descriptor, 0, hdr, operation);
    }
}

static void mca_btl_base_am_process_atomic(mca_btl_base_module_t *btl,
                                           const mca_btl_base_receive_descriptor_t *desc)
{
    /* not all btls work with these active message atomics. at this time
     * all of the affected btls already have atomic support so there is
     * no need to attempt to get the endpoint here. */
    if (NULL == desc->endpoint) {
        BTL_ERROR(("BTL is not compatible with active-message RDMA"));
        abort();
    }

    const mca_btl_base_rdma_hdr_t *hdr = (mca_btl_base_rdma_hdr_t *) desc->des_segments[0]
                                             .seg_addr.pval;
    uint64_t atomic_response = hdr->data.atomic.operand[0];

    if (4 != hdr->data.atomic.size && 8 != hdr->data.atomic.size) {
        BTL_ERROR(("Unexpected atomic operation size: %hu", hdr->data.atomic.size));
        abort();
    }

    BTL_VERBOSE(("got active-message atomic request. hdr->context=0x%" PRIx64
                 ", target_address=%p, "
                 "segment 0 size=%" PRIu64,
                 hdr->context, (void *)(intptr_t)hdr->target_address, desc->des_segments[0].seg_len));

    switch (hdr->type) {
    case MCA_BTL_BASE_AM_ATOMIC:
        if (4 == hdr->data.atomic.size) {
            uint32_t tmp = (uint32_t) atomic_response;
            mca_btl_base_am_atomic_32(&tmp, (opal_atomic_int32_t *) (uintptr_t) hdr->target_address,
                                      hdr->data.atomic.op);
            atomic_response = tmp;
        }
        if (8 == hdr->data.atomic.size) {
            mca_btl_base_am_atomic_64(&atomic_response,
                                      (opal_atomic_int64_t *) (uintptr_t) hdr->target_address,
                                      hdr->data.atomic.op);
        }
        break;
    case MCA_BTL_BASE_AM_CAS:
        if (4 == hdr->data.atomic.size) {
            int32_t tmp = (int32_t) atomic_response;
            opal_atomic_compare_exchange_strong_32((opal_atomic_int32_t *) hdr->target_address,
                                                   &tmp, (int32_t) hdr->data.atomic.operand[1]);
            atomic_response = tmp;
        }
        if (8 == hdr->data.atomic.size) {
            opal_atomic_compare_exchange_strong_64((opal_atomic_int64_t *) hdr->target_address,
                                                   &atomic_response, hdr->data.atomic.operand[1]);
        }
        break;
    default:
        BTL_ERROR(("Unexpected AM atomic request type"));
        abort();
    }

    mca_btl_base_descriptor_t *descriptor = NULL;
    int ret = mca_btl_base_am_rdma_respond(btl, desc->endpoint, &descriptor, &atomic_response, hdr);
    if (OPAL_SUCCESS != ret) {
        mca_btl_base_rdma_queue_operation(btl, desc->endpoint, descriptor, atomic_response, hdr,
                                          NULL);
    }
}

void mca_btl_sm_sc_emu_init(void)
{
    mca_btl_base_active_message_trigger[MCA_BTL_BASE_TAG_RDMA].cbfunc
        = mca_btl_base_am_process_rdma;
    mca_btl_base_active_message_trigger[MCA_BTL_BASE_TAG_ATOMIC].cbfunc
        = mca_btl_base_am_process_atomic;
    mca_btl_base_active_message_trigger[MCA_BTL_BASE_TAG_RDMA_RESP].cbfunc
        = mca_btl_base_am_rdma_response;
}

static int mca_btl_base_am_fop(struct mca_btl_base_module_t *btl,
                               struct mca_btl_base_endpoint_t *endpoint, void *local_address,
                               uint64_t remote_address,
                               mca_btl_base_registration_handle_t *local_handle,
                               mca_btl_base_registration_handle_t *remote_handle,
                               mca_btl_base_atomic_op_t op, uint64_t operand, int flags, int order,
                               mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext,
                               void *cbdata)
{
    size_t size = (flags & MCA_BTL_ATOMIC_FLAG_32BIT) ? 4 : 8;
    return mca_btl_base_rdma_start(btl, endpoint, MCA_BTL_BASE_AM_ATOMIC, operand, 0, op, order,
                                   flags, size, local_address, local_handle, remote_address,
                                   remote_handle, cbfunc, cbcontext, cbdata);
}

static int mca_btl_base_am_cswap(
    struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
    void *local_address, uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
    mca_btl_base_registration_handle_t *remote_handle, uint64_t compare, uint64_t value, int flags,
    int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata)
{
    size_t size = (flags & MCA_BTL_ATOMIC_FLAG_32BIT) ? 4 : 8;
    return mca_btl_base_rdma_start(btl, endpoint, MCA_BTL_BASE_AM_CAS, compare, value, 0, order,
                                   flags, size, local_address, local_handle, remote_address,
                                   remote_handle, cbfunc, cbcontext, cbdata);
}

static int mca_btl_base_am_rdma_get(struct mca_btl_base_module_t *btl,
                                    struct mca_btl_base_endpoint_t *endpoint, void *local_address,
                                    uint64_t remote_address,
                                    struct mca_btl_base_registration_handle_t *local_handle,
                                    struct mca_btl_base_registration_handle_t *remote_handle,
                                    size_t size, int flags, int order,
                                    mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext,
                                    void *cbdata)
{
    return mca_btl_base_rdma_start(btl, endpoint, MCA_BTL_BASE_AM_GET, 0, 0, 0, order, flags, size,
                                   local_address, local_handle, remote_address, remote_handle,
                                   cbfunc, cbcontext, cbdata);
}

static int mca_btl_base_am_rdma_put(struct mca_btl_base_module_t *btl,
                                    struct mca_btl_base_endpoint_t *endpoint, void *local_address,
                                    uint64_t remote_address,
                                    struct mca_btl_base_registration_handle_t *local_handle,
                                    struct mca_btl_base_registration_handle_t *remote_handle,
                                    size_t size, int flags, int order,
                                    mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext,
                                    void *cbdata)
{
    return mca_btl_base_rdma_start(btl, endpoint, MCA_BTL_BASE_AM_PUT, 0, 0, 0, order, flags, size,
                                   local_address, local_handle, remote_address, remote_handle,
                                   cbfunc, cbcontext, cbdata);
}

int mca_btl_base_am_rdma_init(mca_btl_base_module_t *btl)
{
    static bool progress_registered = false;

    if ((btl->btl_flags & (MCA_BTL_FLAGS_RDMA | MCA_BTL_FLAGS_ATOMIC_FOPS))
        == (MCA_BTL_FLAGS_RDMA | MCA_BTL_FLAGS_ATOMIC_FOPS)) {
        /* nothing to do */
        return OPAL_SUCCESS;
    }

    size_t max_operation_size = btl->btl_max_send_size;
    size_t operation_alignment = 1;
    if (mca_btl_base_rdma_use_rdma_get(btl)) {
        /* implement put over get. */
        max_operation_size = btl->btl_get_limit;
        operation_alignment = btl->btl_get_alignment;
    } else if (mca_btl_base_rdma_use_rdma_put(btl)) {
        /* implement get over put. */
        max_operation_size = btl->btl_put_limit;
        operation_alignment = btl->btl_put_alignment;
    }

    if (!(btl->btl_flags & MCA_BTL_FLAGS_PUT)) {
        BTL_VERBOSE(("Enabling AM-based RDMA put for BTL %p", btl));
        btl->btl_flags |= MCA_BTL_FLAGS_PUT_AM;
        btl->btl_put_limit = max_operation_size;
        btl->btl_put_alignment = operation_alignment;
        btl->btl_put = mca_btl_base_am_rdma_put;
    }

    if (!(btl->btl_flags & MCA_BTL_FLAGS_GET)) {
        BTL_VERBOSE(("Enabling AM-based RDMA get for BTL %p", btl));
        btl->btl_flags |= MCA_BTL_FLAGS_GET_AM;
        btl->btl_get_limit = max_operation_size;
        btl->btl_get_alignment = operation_alignment;
        btl->btl_get = mca_btl_base_am_rdma_get;
    }

    if (!(btl->btl_flags & MCA_BTL_FLAGS_ATOMIC_FOPS)) {
        BTL_VERBOSE(("Enabling AM-based FOPs get for BTL %p", btl));
        btl->btl_flags |= MCA_BTL_FLAGS_ATOMIC_AM_FOP;

        btl->btl_atomic_fop = mca_btl_base_am_fop;
        btl->btl_atomic_cswap = mca_btl_base_am_cswap;

        /* emulated RDMA atomics can support the full range of atomics. for
         * now only a handful are supported. */
        btl->btl_atomic_flags = MCA_BTL_ATOMIC_SUPPORTS_GLOB | MCA_BTL_ATOMIC_SUPPORTS_CSWAP
                                | MCA_BTL_ATOMIC_SUPPORTS_32BIT | MCA_BTL_ATOMIC_SUPPORTS_ADD
                                | MCA_BTL_ATOMIC_SUPPORTS_AND | MCA_BTL_ATOMIC_SUPPORTS_OR
                                | MCA_BTL_ATOMIC_SUPPORTS_XOR | MCA_BTL_ATOMIC_SUPPORTS_SWAP
                                | MCA_BTL_ATOMIC_SUPPORTS_MIN | MCA_BTL_ATOMIC_SUPPORTS_MAX;
    }

    if (!progress_registered) {
        progress_registered = true;
        opal_progress_register(mca_btl_base_am_rdma_progress);
        mca_btl_sm_sc_emu_init();
        OBJ_CONSTRUCT(&default_module, mca_btl_base_am_rdma_module_t);
    }

    return OPAL_SUCCESS;
}
