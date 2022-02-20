/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2020-2021 Google, LLC. All rights reserved.
 * Copyright (c) 2021-2022 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <assert.h>

#include "opal/mca/btl/base/btl_base_am_rdma.h"
#include "opal/mca/btl/base/base.h"
#include "opal/mca/btl/base/btl_base_error.h"
#include "opal/mca/threads/mutex.h"
#include "opal/util/minmax.h"


/**
 * @brief global data for active message wrapper
 *
 * While individual entries in queued_responses and
 * queued_initiator_descriptors are module-specific (ie, per BTL
 * module), they are progressed in a common progress function.  It is
 * much more efficient to have one list of work to do, rather than
 * having to poll through all active btls to find the work to do.
 */
struct am_rdma_component_t {
    opal_object_t super;
    /** provides protection for multi-threaded access to module members */
    opal_mutex_t mutex;
    /** responses queued awaiting resources */
    opal_list_t queued_responses;
    /** queued initiator descriptors */
    opal_list_t queued_initiator_descriptors;
};
typedef struct am_rdma_component_t am_rdma_component_t;

static am_rdma_component_t default_component;

static void am_rdma_component_init(am_rdma_component_t *component)
{
    OBJ_CONSTRUCT(&component->mutex, opal_mutex_t);
    OBJ_CONSTRUCT(&component->queued_responses, opal_list_t);
    OBJ_CONSTRUCT(&component->queued_initiator_descriptors, opal_list_t);
}

static void am_rdma_component_fini(am_rdma_component_t *component)
{
    OBJ_DESTRUCT(&component->mutex);
    OBJ_DESTRUCT(&component->queued_responses);
    OBJ_DESTRUCT(&component->queued_initiator_descriptors);
}

static OBJ_CLASS_INSTANCE(am_rdma_component_t, opal_object_t,
                          am_rdma_component_init, am_rdma_component_fini);


OBJ_CLASS_INSTANCE(mca_btl_base_am_rdma_module_t, opal_object_t,
                   NULL, NULL);


/**
 * @brief response header for an active-message RDMA/atomic operation
 */
struct am_rdma_response_hdr_t {
    /** context for the response */
    uint64_t context;
    /** initiator address */
    uint64_t initiator_address;
    /** size of response */
    uint64_t response_size;
    /** response data may follow. the size is implied by the size of the incoming
     * descriptor */
};
typedef struct am_rdma_response_hdr_t am_rdma_response_hdr_t;


/**
 * @brief type of active-message RDMA/atomic operation
 */
enum am_rdma_type_t {
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
typedef enum am_rdma_type_t am_rdma_type_t;


/**
 * @brief origin-side operation context for an active-message RDMA/atomic operation
 */
struct am_rdma_context_t {
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
typedef struct am_rdma_context_t am_rdma_context_t;

static void am_rdma_context_init(am_rdma_context_t *context)
{
    context->sent = 0;
    context->acknowledged = 0;
    context->descriptor = NULL;
}

static OBJ_CLASS_INSTANCE(am_rdma_context_t, opal_object_t,
                          am_rdma_context_init, NULL);


/**
 * @brief queued initiator descriptor
 */
struct am_rdma_queued_descriptor_t {
    opal_list_item_t super;
    mca_btl_base_am_rdma_module_t *am_module;
    struct mca_btl_base_endpoint_t *endpoint;
    mca_btl_base_descriptor_t *descriptor;
};
typedef struct am_rdma_queued_descriptor_t am_rdma_queued_descriptor_t;

static OBJ_CLASS_INSTANCE(am_rdma_queued_descriptor_t, opal_list_item_t, NULL, NULL);


/**
 * @brief header for an active-message atomic/RDMA operation
 */
struct am_rdma_hdr_t {
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
};
typedef struct am_rdma_hdr_t am_rdma_hdr_t;

/**
 * @brief target-side RDMA/atomic operation
 */
struct am_rdma_operation_t {
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
    am_rdma_hdr_t hdr;
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
typedef struct am_rdma_operation_t am_rdma_operation_t;

static OBJ_CLASS_INSTANCE(am_rdma_operation_t, opal_list_item_t, NULL, NULL);


static inline bool am_rdma_is_atomic(am_rdma_type_t type)
{
    return (MCA_BTL_BASE_AM_PUT != type && MCA_BTL_BASE_AM_GET != type);
}


static inline size_t am_rdma_operation_size(mca_btl_base_am_rdma_module_t *am_module,
                                            am_rdma_type_t type,
                                            size_t remaining)
{
    mca_btl_base_module_t *btl = am_module->btl;

    switch (type) {
    case MCA_BTL_BASE_AM_PUT:
        if (am_module->use_rdma_get) {
            return opal_min(remaining, btl->btl_get_limit);
        }
        break;
    case MCA_BTL_BASE_AM_GET:
        if (am_module->use_rdma_put) {
            return opal_min(remaining, btl->btl_put_limit);
        }
        break;
    case MCA_BTL_BASE_AM_ATOMIC:
        /* fall through */
    case MCA_BTL_BASE_AM_CAS:
        return remaining;
    }

    return opal_min(remaining, btl->btl_max_send_size - sizeof(am_rdma_hdr_t));
}


static inline int am_rdma_tag(am_rdma_type_t type)
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


static inline int am_rdma_resp_tag(void)
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
static void am_rdma_copy_from_segments(uint64_t addr, size_t skip_bytes,
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
static void am_rdma_copy_to_segments(uint64_t addr, size_t max_len, size_t skip_bytes,
                                     mca_btl_base_segment_t *segments, size_t segment_count)
{
    void *seg0_data = (void *) ((uintptr_t) segments[0].seg_addr.pval + skip_bytes);
    size_t seg0_len = opal_min(max_len, segments[0].seg_len - skip_bytes);

    if (seg0_len > 0) {
        BTL_VERBOSE(
            ("packing %" PRIsize_t " bytes from 0x%" PRIx64 " to segment 0", seg0_len, addr));
        memcpy(seg0_data, (const void *) addr, seg0_len);
        addr += seg0_len;
        max_len -= seg0_len;
        segments[0].seg_len = seg0_len + skip_bytes;
    }

    for (size_t i = 1; i < segment_count && max_len; ++i) {
        size_t seg_len = opal_min(segments[i].seg_len, max_len);

        BTL_VERBOSE(("packing %" PRIsize_t " bytes from 0x%" PRIx64 " to segment %" PRIsize_t,
                     seg_len, addr, i));

        memcpy(segments[i].seg_addr.pval, (const void *) addr, seg_len);
        segments[i].seg_len = seg_len;

        addr += seg_len;
        max_len -= seg_len;
    }
}


static void am_rdma_queue_initiator_descriptor(mca_btl_base_am_rdma_module_t *am_module,
                                               struct mca_btl_base_endpoint_t *endpoint,
                                               mca_btl_base_descriptor_t *descriptor)
{
    am_rdma_queued_descriptor_t *queued_descriptor = OBJ_NEW(am_rdma_queued_descriptor_t);

    queued_descriptor->am_module = am_module;
    queued_descriptor->endpoint = endpoint;
    queued_descriptor->descriptor = descriptor;

    OPAL_THREAD_SCOPED_LOCK(&default_component.mutex,
                            opal_list_append(&default_component.queued_initiator_descriptors,
                                             &queued_descriptor->super));
}


static inline int am_rdma_advance(mca_btl_base_am_rdma_module_t *am_module,
                                  struct mca_btl_base_endpoint_t *endpoint,
                                  am_rdma_context_t *context,
                                  bool send_descriptor)
{
    mca_btl_base_module_t *btl = am_module->btl;
    int ret;
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
    am_rdma_hdr_t *hdr = (am_rdma_hdr_t *) descriptor->des_segments[0]
                                       .seg_addr.pval;
    const size_t packet_size = am_rdma_operation_size(am_module, hdr->type, remaining);

    if (!am_rdma_is_atomic(hdr->type)) {
        hdr->data.rdma.size = packet_size;
        hdr->data.rdma.initiator_address = (uint64_t) context->local_address + context->sent;
    } else {
        /* atomics today are single datatype entries */
        assert(packet_size < UINT8_MAX);
        hdr->data.atomic.size = packet_size;
    }

    hdr->target_address = context->remote_address + context->sent;

    context->sent += packet_size;

    if (MCA_BTL_BASE_AM_PUT == hdr->type && !hdr->data.rdma.use_rdma) {
        /* copy the next block into the fragment buffer */
        am_rdma_copy_to_segments(hdr->data.rdma.initiator_address, packet_size, sizeof(*hdr),
                                 descriptor->des_segments, descriptor->des_segment_count);
    }

    if (send_descriptor) {
        assert(0 != (descriptor->des_flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK));
        ret = btl->btl_send(btl, endpoint, descriptor, am_rdma_tag(hdr->type));
        if (ret == 1) {
            ret = OPAL_SUCCESS;
        }
        return ret;
    }

    /* queue for later to avoid btl_send in callback */
    am_rdma_queue_initiator_descriptor(am_module, endpoint, descriptor);

    return OPAL_SUCCESS;
}


static void am_rdma_descriptor_complete(mca_btl_base_module_t *btl,
                                        struct mca_btl_base_endpoint_t *endpoint,
                                        mca_btl_base_descriptor_t *descriptor, int status)
{
    mca_btl_base_am_rdma_module_t *am_module = (mca_btl_base_am_rdma_module_t *)descriptor->des_cbdata;

    (void) am_rdma_advance(am_module, endpoint,
                           (am_rdma_context_t *) descriptor->des_context,
                           /*send_descriptor=*/false);
}


static inline int am_rdma_start(mca_btl_base_am_rdma_module_t *am_module, struct mca_btl_base_endpoint_t *endpoint,
                                int type, uint64_t operand1, uint64_t operand2, mca_btl_base_atomic_op_t op,
                                int order, int flags, size_t size, void *local_address,
                                mca_btl_base_registration_handle_t *local_handle, int64_t remote_address,
                                mca_btl_base_registration_handle_t *remote_handle,
                                mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata)
{
    mca_btl_base_module_t *btl = am_module->btl;
    am_rdma_hdr_t *hdr;
    size_t packet_size = sizeof(*hdr);
    mca_btl_base_descriptor_t *descriptor;
    am_rdma_context_t *context = OBJ_NEW(am_rdma_context_t);

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
        } else if (!am_module->use_rdma_get) {
            packet_size += opal_min (size, btl->btl_max_send_size - sizeof (*hdr));
        } else {
            use_rdma = true;
        }
    } else if (MCA_BTL_BASE_AM_GET == type) {
        if (!am_module->use_rdma_put) {
            packet_size += opal_min(size, btl->btl_max_send_size - sizeof(*hdr));
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
                 (void*) context, size, packet_size));

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

    descriptor->des_cbfunc = am_rdma_descriptor_complete;
    descriptor->des_cbdata = am_module;
    descriptor->des_context = context;

    hdr = (am_rdma_hdr_t *) descriptor->des_segments[0].seg_addr.pval;
    hdr->type = type;

    if (!am_rdma_is_atomic(type)) {
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

    return am_rdma_advance(am_module, endpoint, context, /*send_descriptor=*/true);
}


static am_rdma_operation_t *am_rdma_alloc_operation(mca_btl_base_module_t *btl,
                                                    struct mca_btl_base_endpoint_t *endpoint,
                                                    mca_btl_base_descriptor_t *descriptor,
                                                    const am_rdma_hdr_t *hdr)
{
    am_rdma_operation_t *operation = OBJ_NEW(am_rdma_operation_t);
    if (NULL == operation) {
        return NULL;
    }

    operation->btl = btl;
    operation->endpoint = endpoint;
    operation->descriptor = descriptor;
    operation->is_completed = false;
    operation->is_queued = false;
    memcpy(&operation->hdr, hdr, sizeof(*hdr));

    if (!am_rdma_is_atomic(hdr->type) && hdr->data.rdma.use_rdma
        && btl->btl_register_mem) {
        const uint8_t *handle_data = (const uint8_t *) (hdr + 1);
        /* the initiator packs these in order of their local and then remote. */
        memcpy(operation->remote_handle_data, handle_data, btl->btl_registration_handle_size);
        handle_data += btl->btl_registration_handle_size;
        memcpy(operation->local_handle_data, handle_data, btl->btl_registration_handle_size);
    }

    return operation;
}


static void am_rdma_queue_operation(mca_btl_base_module_t *btl,
                                    struct mca_btl_base_endpoint_t *endpoint,
                                    mca_btl_base_descriptor_t *descriptor,
                                    uint64_t atomic_response,
                                    const am_rdma_hdr_t *hdr,
                                    am_rdma_operation_t *operation)
{
    if (NULL == operation) {
        operation = am_rdma_alloc_operation(btl, endpoint, descriptor, hdr);
        if (NULL == operation) {
            /* couldn't even allocate a small amount of memory. not much else can be done. */
            BTL_ERROR(("could not allocate memory to queue active-message RDMA operation"));
            abort();
        }
    }

    operation->is_queued = true;
    operation->atomic_response = atomic_response;
    OPAL_THREAD_SCOPED_LOCK(&default_component.mutex,
                            opal_list_append(&default_component.queued_responses, &operation->super));
}


static int am_rdma_respond(mca_btl_base_module_t *btl,
                           struct mca_btl_base_endpoint_t *endpoint,
                           mca_btl_base_descriptor_t **descriptor, void *addr,
                           const am_rdma_hdr_t *hdr)
{
    mca_btl_base_descriptor_t *send_descriptor = *descriptor;
    *descriptor = NULL;

    if (NULL == send_descriptor) {
        am_rdma_response_hdr_t *resp_hdr;
        size_t data_size = am_rdma_is_atomic(hdr->type) ? hdr->data.atomic.size
                                                                  : hdr->data.rdma.size;
        size_t packet_size = sizeof(*resp_hdr) + (addr ? data_size : 0);
        send_descriptor = btl->btl_alloc(btl, endpoint, MCA_BTL_NO_ORDER, packet_size,
                                         MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
        if (NULL == send_descriptor) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }

        resp_hdr = (am_rdma_response_hdr_t *) send_descriptor->des_segments[0]
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
            am_rdma_copy_to_segments((uint64_t)(uintptr_t) addr, packet_size,
                                     sizeof(*resp_hdr), send_descriptor->des_segments,
                                     send_descriptor->des_segment_count);
        }
    }

    BTL_VERBOSE(("sending descriptor %p", (void*) send_descriptor));

    send_descriptor->des_cbfunc = NULL;

    /* There is no callback for the response descriptor, therefore it is
     * safe to treat 0 and 1 return codes the same
     */
    int ret = btl->btl_send(btl, endpoint, send_descriptor, am_rdma_resp_tag());
    if (ret == 1) {
        ret = OPAL_SUCCESS;
    }

    if (OPAL_UNLIKELY(OPAL_SUCCESS != ret)) {
        *descriptor = send_descriptor;
    }
    return ret;
}


static void
am_rdma_rdma_complete(mca_btl_base_module_t *btl,
                      struct mca_btl_base_endpoint_t *endpoint, void *local_address,
                      struct mca_btl_base_registration_handle_t *local_handle,
                      void *context, void *cbdata, int status)
{
    am_rdma_operation_t *operation = (am_rdma_operation_t *) context;

    BTL_VERBOSE(("BTL RDMA operation complete. status=%d", status));

    assert(OPAL_SUCCESS == status);

    operation->is_completed = true;
    int ret = am_rdma_respond(operation->btl, operation->endpoint,
                              &operation->descriptor, NULL, &operation->hdr);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != ret)) {
        BTL_VERBOSE(
            ("could not send a response. queueing the response for later. endpoint=%p, ret=%d",
             (void*) endpoint, ret));
        am_rdma_queue_operation(btl, NULL, NULL, 0, NULL, operation);
    }

    OBJ_RELEASE(operation);
}


static int am_rdma_target_put(mca_btl_base_module_t *btl,
                              struct mca_btl_base_endpoint_t *endpoint,
                              mca_btl_base_descriptor_t **descriptor,
                              const mca_btl_base_segment_t *segments,
                              size_t segment_count, void *target_address,
                              const am_rdma_hdr_t *hdr,
                              am_rdma_operation_t **operation)
{
    if (hdr->data.rdma.use_rdma) {
        if (NULL == *operation) {
            *operation = am_rdma_alloc_operation(btl, endpoint, *descriptor, hdr);
            if (NULL == *operation) {
                return OPAL_ERR_OUT_OF_RESOURCE;
            }
        }

        /* btl supports get but not put. emulating put with get */
        OBJ_RETAIN(*operation);
        int ret = btl->btl_get(
            btl, endpoint, target_address, hdr->data.rdma.initiator_address,
            (struct mca_btl_base_registration_handle_t *) (*operation)->local_handle_data,
            (struct mca_btl_base_registration_handle_t *) (*operation)->remote_handle_data,
            hdr->data.rdma.size, /*flags=*/0, MCA_BTL_NO_ORDER, am_rdma_rdma_complete,
            *operation, NULL);
        if (OPAL_SUCCESS != ret) {
            OBJ_RELEASE(*operation);
        }

        if (OPAL_ERR_NOT_AVAILABLE != ret) {
            return ret;
        }
    } else if (NULL != segments) {
        am_rdma_copy_from_segments(hdr->target_address, sizeof(*hdr), segments, segment_count);
    }

    return am_rdma_respond(btl, endpoint, descriptor, NULL, hdr);
}


static int am_rdma_target_get(mca_btl_base_module_t *btl,
                              struct mca_btl_base_endpoint_t *endpoint,
                              mca_btl_base_descriptor_t **descriptor,
                              void *target_address, const am_rdma_hdr_t *hdr,
                              am_rdma_operation_t **operation)
{
    if (hdr->data.rdma.use_rdma) {
        if (NULL == *operation) {
            *operation = am_rdma_alloc_operation(btl, endpoint, *descriptor, hdr);
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
            hdr->data.rdma.size, /*flags=*/0, MCA_BTL_NO_ORDER, am_rdma_rdma_complete,
            *operation, NULL);
        if (OPAL_SUCCESS != ret) {
            OBJ_RELEASE(*operation);
        }
        if (OPAL_ERR_NOT_AVAILABLE != ret) {
            return ret;
        }
    }

    return am_rdma_respond(btl, endpoint, descriptor, target_address, hdr);
}


static void am_rdma_retry_operation(am_rdma_operation_t *operation)
{
    void *target_address = (void *) (intptr_t) operation->hdr.target_address;
    int ret = OPAL_SUCCESS;

    if (!operation->descriptor && !operation->is_completed) {
        switch (operation->hdr.type) {
        case MCA_BTL_BASE_AM_GET:
            ret = am_rdma_target_get(operation->btl, operation->endpoint,
                                     &operation->descriptor, target_address,
                                     &operation->hdr, &operation);
            break;
        case MCA_BTL_BASE_AM_PUT:
            ret = am_rdma_target_put(operation->btl, operation->endpoint,
                                     &operation->descriptor,
                                     /*segments=*/NULL,
                                     /*segment_count=*/0, target_address,
                                     &operation->hdr, &operation);
            break;
        case MCA_BTL_BASE_AM_ATOMIC:
            /* atomic operation was completed */
            ret = am_rdma_respond(operation->btl, operation->endpoint,
                                  &operation->descriptor, &operation->atomic_response,
                                  &operation->hdr);
            break;
        }
    } else {
        ret = am_rdma_respond(operation->btl, operation->endpoint,
                              &operation->descriptor,
                              /*addr=*/NULL, /*hdr=*/NULL);
    }

    if (OPAL_SUCCESS == ret) {
        if (operation->is_queued) {
            opal_list_remove_item(&default_component.queued_responses, &operation->super);
        }

        OBJ_RELEASE(operation);
    }
}


static int am_rdma_progress(void)
{
    if (0 == opal_list_get_size(&default_component.queued_responses)
        && 0 == opal_list_get_size(&default_component.queued_initiator_descriptors)) {
        return 0;
    }

    // It's a little cleaner, stylistically, to make the multi-line
    // ACTION argument to OPAL_THREAD_SCOPED_LOCK be a macro itself
    // (vs. using continuation characters in the use of
    // OPAL_THREAD_SCOPED_LOCK).
#define ACTION1                                                         \
    am_rdma_operation_t *operation, *next;                              \
    OPAL_LIST_FOREACH_SAFE(operation, next,                             \
                           &default_component.queued_responses,         \
                           am_rdma_operation_t) {                       \
        am_rdma_retry_operation(operation);                             \
    }

    OPAL_THREAD_SCOPED_LOCK(&default_component.mutex, ACTION1);

#define ACTION2                                                         \
    am_rdma_queued_descriptor_t *descriptor, *next;                     \
    OPAL_LIST_FOREACH_SAFE(descriptor, next,                            \
                           &default_component.queued_initiator_descriptors, \
                           am_rdma_queued_descriptor_t) {               \
        am_rdma_context_t *context =                                    \
            (am_rdma_context_t *) descriptor->descriptor->des_context;  \
        mca_btl_base_module_t *btl = descriptor->am_module->btl;        \
        assert(0 != (descriptor->descriptor->des_flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK)); \
        int ret = btl->btl_send(btl,                                    \
                                descriptor->endpoint,                   \
                                descriptor->descriptor,                 \
                                am_rdma_tag(context->type));            \
        if (OPAL_SUCCESS == ret || 1 == ret) {                          \
            opal_list_remove_item(&default_component.queued_initiator_descriptors, \
                                  &descriptor->super);                  \
        }                                                               \
    }

    OPAL_THREAD_SCOPED_LOCK(&default_component.mutex, ACTION2);

    return 0;
}


static int am_rdma_atomic_64(int64_t *operand, opal_atomic_int64_t *addr,
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


static int am_rdma_atomic_32(int32_t *operand, opal_atomic_int32_t *addr,
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


static void am_rdma_response(mca_btl_base_module_t *btl,
                             const mca_btl_base_receive_descriptor_t *desc)
{
    am_rdma_response_hdr_t *resp_hdr = (am_rdma_response_hdr_t *) desc
                                                     ->des_segments[0]
                                                     .seg_addr.pval;

    assert(desc->des_segments[0].seg_len >= sizeof(*resp_hdr));

    am_rdma_context_t *context = (am_rdma_context_t *) (uintptr_t)
                                               resp_hdr->context;

    BTL_VERBOSE(("received response for RDMA operation. context=%p, size=%" PRIu64, (void*) context,
                 resp_hdr->response_size));

    if (MCA_BTL_BASE_AM_PUT != context->type) {
        uint64_t local_address = resp_hdr->initiator_address ? resp_hdr->initiator_address
                                                             : (uintptr_t) context->local_address;
        if (local_address && MCA_BTL_BASE_AM_PUT != context->type) {
            BTL_VERBOSE(("unpacking response from packet"));
            /* if there is a result copy it out of the incoming buffer. if RDMA is being used
             * (get/put or put/get) then the header should be the only thing in the incoming
             * message. */
            am_rdma_copy_from_segments(local_address, sizeof(*resp_hdr), desc->des_segments,
                                       desc->des_segment_count);
        }
    }

    if (context->total_size
        == (uint64_t) opal_atomic_add_fetch_64(&context->acknowledged, resp_hdr->response_size)) {
        context->cbfunc(btl, desc->endpoint, context->local_address, context->local_handle,
                        context->cbcontext, context->cbdata, OPAL_SUCCESS);
        OBJ_RELEASE(context);
    }
}


static void am_rdma_process_rdma(mca_btl_base_module_t *btl,
                                 const mca_btl_base_receive_descriptor_t *desc)
{
    /* not all btls work with these active message atomics. at this time
     * all of the affected btls already have atomic support so there is
     * no need to attempt to get the endpoint here. */
    if (NULL == desc->endpoint) {
        BTL_ERROR(("BTL is not compatible with active-message RDMA"));
        abort();
    }

    const am_rdma_hdr_t *hdr = (am_rdma_hdr_t *) desc->des_segments[0]
                                             .seg_addr.pval;
    void *target_address = (void *) (intptr_t) hdr->target_address;
    mca_btl_base_descriptor_t *descriptor = NULL;
    am_rdma_operation_t *operation = NULL;
    int ret;

    BTL_VERBOSE(("got active-message \"RDMA\" request. hdr->context=0x%" PRIx64
                 ", target_address=%p, "
                 "segment 0 size=%" PRIu64,
                 hdr->context, target_address, desc->des_segments[0].seg_len));

    if (MCA_BTL_BASE_AM_PUT == hdr->type) {
        ret = am_rdma_target_put(btl, desc->endpoint, &descriptor, desc->des_segments,
                                              desc->des_segment_count, target_address, hdr,
                                              &operation);
    } else if (MCA_BTL_BASE_AM_GET == hdr->type) {
        ret = am_rdma_target_get(btl, desc->endpoint, &descriptor, target_address, hdr,
                                              &operation);
    } else {
        BTL_ERROR(("Unexpected tag when processing active-message RDMA request"));
        abort();
    }

    if (OPAL_SUCCESS != ret) {
        am_rdma_queue_operation(btl, desc->endpoint, descriptor, 0, hdr, operation);
    }
}


static void am_rdma_process_atomic(mca_btl_base_module_t *btl,
                                   const mca_btl_base_receive_descriptor_t *desc)
{
    /* not all btls work with these active message atomics. at this time
     * all of the affected btls already have atomic support so there is
     * no need to attempt to get the endpoint here. */
    if (NULL == desc->endpoint) {
        BTL_ERROR(("BTL is not compatible with active-message RDMA"));
        abort();
    }

    const am_rdma_hdr_t *hdr = (am_rdma_hdr_t *) desc->des_segments[0]
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
            int32_t tmp = (int32_t) atomic_response;
            am_rdma_atomic_32(&tmp, (opal_atomic_int32_t *) hdr->target_address,
                              hdr->data.atomic.op);
            atomic_response = tmp;
        } else if (8 == hdr->data.atomic.size) {
            int64_t tmp = (int64_t) atomic_response;
            am_rdma_atomic_64(&tmp,
                              (opal_atomic_int64_t *) hdr->target_address,
                              hdr->data.atomic.op);
            atomic_response = tmp;
        }
        break;
    case MCA_BTL_BASE_AM_CAS:
        if (4 == hdr->data.atomic.size) {
            int32_t tmp = (int32_t) atomic_response;
            opal_atomic_compare_exchange_strong_32((opal_atomic_int32_t *) hdr->target_address,
                                                   &tmp, (int32_t) hdr->data.atomic.operand[1]);
            atomic_response = tmp;
        } else if (8 == hdr->data.atomic.size) {
            int64_t tmp = (int64_t) atomic_response;
            opal_atomic_compare_exchange_strong_64((opal_atomic_int64_t *) hdr->target_address,
                                                   &tmp, hdr->data.atomic.operand[1]);
            atomic_response = tmp;
        }
        break;
    default:
        BTL_ERROR(("Unexpected AM atomic request type"));
        abort();
    }

    mca_btl_base_descriptor_t *descriptor = NULL;
    int ret = am_rdma_respond(btl, desc->endpoint, &descriptor, &atomic_response, hdr);
    if (OPAL_SUCCESS != ret) {
        am_rdma_queue_operation(btl, desc->endpoint, descriptor, atomic_response, hdr,
                                          NULL);
    }
}


static int am_rdma_put(mca_btl_base_am_rdma_module_t *am_module,
                       struct mca_btl_base_endpoint_t *endpoint, void *local_address,
                       uint64_t remote_address,
                       struct mca_btl_base_registration_handle_t *local_handle,
                       struct mca_btl_base_registration_handle_t *remote_handle,
                       size_t size, int flags, int order,
                       mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext,
                       void *cbdata)
{
    return am_rdma_start(am_module, endpoint, MCA_BTL_BASE_AM_PUT, 0, 0, 0, order, flags, size,
                         local_address, local_handle, remote_address, remote_handle,
                         cbfunc, cbcontext, cbdata);
}


static int am_rdma_get(mca_btl_base_am_rdma_module_t *am_module,
                       struct mca_btl_base_endpoint_t *endpoint, void *local_address,
                       uint64_t remote_address,
                       struct mca_btl_base_registration_handle_t *local_handle,
                       struct mca_btl_base_registration_handle_t *remote_handle,
                       size_t size, int flags, int order,
                       mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext,
                       void *cbdata)
{
    return am_rdma_start(am_module, endpoint, MCA_BTL_BASE_AM_GET, 0, 0, 0, order, flags, size,
                         local_address, local_handle, remote_address, remote_handle,
                         cbfunc, cbcontext, cbdata);
}


static int am_rdma_fop(mca_btl_base_am_rdma_module_t *am_module,
                       struct mca_btl_base_endpoint_t *endpoint, void *local_address,
                       uint64_t remote_address,
                       mca_btl_base_registration_handle_t *local_handle,
                       mca_btl_base_registration_handle_t *remote_handle,
                       mca_btl_base_atomic_op_t op, uint64_t operand, int flags, int order,
                       mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext,
                       void *cbdata)
{
    size_t size = (flags & MCA_BTL_ATOMIC_FLAG_32BIT) ? 4 : 8;

    return am_rdma_start(am_module, endpoint, MCA_BTL_BASE_AM_ATOMIC, operand, 0, op, order,
                         flags, size, local_address, local_handle, remote_address,
                         remote_handle, cbfunc, cbcontext, cbdata);
}


static int am_rdma_cswap(mca_btl_base_am_rdma_module_t *am_module,
                         struct mca_btl_base_endpoint_t *endpoint,
                         void *local_address, uint64_t remote_address,
                         mca_btl_base_registration_handle_t *local_handle,
                         mca_btl_base_registration_handle_t *remote_handle,
                         uint64_t compare, uint64_t value, int flags, int order,
                         mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext,
                         void *cbdata)
{
    size_t size = (flags & MCA_BTL_ATOMIC_FLAG_32BIT) ? 4 : 8;

    return am_rdma_start(am_module, endpoint, MCA_BTL_BASE_AM_CAS, compare, value, 0, order,
                         flags, size, local_address, local_handle, remote_address,
                         remote_handle, cbfunc, cbcontext, cbdata);
}


static mca_btl_base_am_rdma_module_t *am_rdma_get_module(struct mca_btl_base_module_t *btl)
{
    assert(NULL != btl->btl_am_data);
    return (mca_btl_base_am_rdma_module_t *)btl->btl_am_data;
}


static int am_rdma_put_wrapper(struct mca_btl_base_module_t *btl,
                               struct mca_btl_base_endpoint_t *endpoint, void *local_address,
                               uint64_t remote_address,
                               struct mca_btl_base_registration_handle_t *local_handle,
                               struct mca_btl_base_registration_handle_t *remote_handle,
                               size_t size, int flags, int order,
                               mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext,
                               void *cbdata)
{
    mca_btl_base_am_rdma_module_t *am_module = am_rdma_get_module(btl);

    return am_rdma_start(am_module, endpoint, MCA_BTL_BASE_AM_PUT, 0, 0, 0, order, flags, size,
                         local_address, local_handle, remote_address, remote_handle,
                         cbfunc, cbcontext, cbdata);
}


static int am_rdma_get_wrapper(struct mca_btl_base_module_t *btl,
                               struct mca_btl_base_endpoint_t *endpoint, void *local_address,
                               uint64_t remote_address,
                               struct mca_btl_base_registration_handle_t *local_handle,
                               struct mca_btl_base_registration_handle_t *remote_handle,
                               size_t size, int flags, int order,
                               mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext,
                               void *cbdata)
{
    mca_btl_base_am_rdma_module_t *am_module = am_rdma_get_module(btl);

    return am_rdma_start(am_module, endpoint, MCA_BTL_BASE_AM_GET, 0, 0, 0, order, flags, size,
                         local_address, local_handle, remote_address, remote_handle,
                         cbfunc, cbcontext, cbdata);
}


static int am_rdma_fop_wrapper(struct mca_btl_base_module_t *btl,
                               struct mca_btl_base_endpoint_t *endpoint, void *local_address,
                               uint64_t remote_address,
                               mca_btl_base_registration_handle_t *local_handle,
                               mca_btl_base_registration_handle_t *remote_handle,
                               mca_btl_base_atomic_op_t op, uint64_t operand, int flags, int order,
                               mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext,
                               void *cbdata)
{
    mca_btl_base_am_rdma_module_t *am_module = am_rdma_get_module(btl);
    size_t size = (flags & MCA_BTL_ATOMIC_FLAG_32BIT) ? 4 : 8;

    return am_rdma_start(am_module, endpoint, MCA_BTL_BASE_AM_ATOMIC, operand, 0, op, order,
                         flags, size, local_address, local_handle, remote_address,
                         remote_handle, cbfunc, cbcontext, cbdata);
}


static int am_rdma_cswap_wrapper(struct mca_btl_base_module_t *btl,
                                 struct mca_btl_base_endpoint_t *endpoint,
                                 void *local_address, uint64_t remote_address,
                                 mca_btl_base_registration_handle_t *local_handle,
                                 mca_btl_base_registration_handle_t *remote_handle,
                                 uint64_t compare, uint64_t value, int flags, int order,
                                 mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext,
                                 void *cbdata)
{
    mca_btl_base_am_rdma_module_t *am_module = am_rdma_get_module(btl);
    size_t size = (flags & MCA_BTL_ATOMIC_FLAG_32BIT) ? 4 : 8;

    return am_rdma_start(am_module, endpoint, MCA_BTL_BASE_AM_CAS, compare, value, 0, order,
                         flags, size, local_address, local_handle, remote_address,
                         remote_handle, cbfunc, cbcontext, cbdata);
}


static void am_rdma_register_callbacks(void)
{
    mca_btl_base_active_message_trigger[MCA_BTL_BASE_TAG_RDMA].cbfunc
        = am_rdma_process_rdma;
    mca_btl_base_active_message_trigger[MCA_BTL_BASE_TAG_ATOMIC].cbfunc
        = am_rdma_process_atomic;
    mca_btl_base_active_message_trigger[MCA_BTL_BASE_TAG_RDMA_RESP].cbfunc
        = am_rdma_response;
}


static int am_rdma_internal_init(mca_btl_base_module_t *btl,
                                 uint32_t flags_requested,
                                 bool no_memory_registration,
                                 mca_btl_base_am_rdma_module_t **new_module)
{
    static bool initialized = false;
    static opal_mutex_t initialized_mutex = OPAL_MUTEX_STATIC_INIT;
    mca_btl_base_am_rdma_module_t *module;
    size_t max_operation_size;
    size_t operation_alignment;

    opal_mutex_lock(&initialized_mutex);
    if (!initialized) {
        initialized = true;
        OBJ_CONSTRUCT(&default_component, am_rdma_component_t);
        opal_progress_register(am_rdma_progress);
        am_rdma_register_callbacks();
    }
    opal_mutex_unlock(&initialized_mutex);

    module = OBJ_NEW(mca_btl_base_am_rdma_module_t);
    if (NULL == module) {
        return OPAL_ERR_TEMP_OUT_OF_RESOURCE;
    }

    module->btl = btl;
    module->use_rdma_put = !!(btl->btl_flags & MCA_BTL_FLAGS_PUT);
    module->use_rdma_get = !!(btl->btl_flags & MCA_BTL_FLAGS_GET);

    /* if the requester asked for remote completion and the btl does
     * not provide remove completion, we can not use put.
     */
    if (!(btl->btl_flags & MCA_BTL_FLAGS_RDMA_REMOTE_COMPLETION)) {
        module->use_rdma_put = false;
    }

    /* if the requester does not want to do memory registration and
     * the BTL requires memory registration, disable the use of RDMA.
     */
    if (no_memory_registration && NULL != btl->btl_register_mem) {
        module->use_rdma_put = false;
        module->use_rdma_get = false;
    }

    if (module->use_rdma_get) {
        /* implement operations over get. */
        max_operation_size = btl->btl_get_limit;
        operation_alignment = btl->btl_get_alignment;
        BTL_VERBOSE(("am_rdma_init: btl %p using get.  operation size %zu, alignment %zu",
                     (void *)btl, max_operation_size, operation_alignment));
    } else if (module->use_rdma_put) {
        /* implement operations over put. */
        max_operation_size = btl->btl_put_limit;
        operation_alignment = btl->btl_put_alignment;
        BTL_VERBOSE(("am_rdma_init: btl %p using put.  operation size %zu, alignment %zu",
                     (void *)btl, max_operation_size, operation_alignment));
    } else {
        /* implement operations over send. */
        max_operation_size = btl->btl_max_send_size;
        operation_alignment = 1;
        BTL_VERBOSE(("am_rdma_init: btl %p using send.  operation size %zu, alignment %zu",
                     (void *)btl, max_operation_size, operation_alignment));
    }

    module->am_btl_put_limit = max_operation_size - sizeof(am_rdma_hdr_t);
    module->am_btl_put_alignment = operation_alignment;
    module->am_btl_get_limit = max_operation_size - sizeof(am_rdma_response_hdr_t);
    module->am_btl_get_alignment = operation_alignment;

    module->am_btl_put = am_rdma_put;
    module->am_btl_get = am_rdma_get;
    module->am_btl_atomic_fop = am_rdma_fop;
    module->am_btl_atomic_cswap = am_rdma_cswap;

    *new_module = module;

    return OPAL_SUCCESS;
}


static int am_rdma_internal_fini(mca_btl_base_am_rdma_module_t *am_rdma_module)
{
    OBJ_RELEASE(am_rdma_module);

    return OPAL_SUCCESS;
}


int mca_btl_base_am_rdma_init(mca_btl_base_module_t *btl)
{
    mca_btl_base_am_rdma_module_t *am_module;
    int ret;

    BTL_VERBOSE(("am_rdma_init: called for btl %s (%p)",
                 btl->btl_component->btl_version.mca_component_name, (void *)btl));

    if ((btl->btl_flags & (MCA_BTL_FLAGS_RDMA | MCA_BTL_FLAGS_ATOMIC_FOPS))
        == (MCA_BTL_FLAGS_RDMA | MCA_BTL_FLAGS_ATOMIC_FOPS)) {
        BTL_VERBOSE(("am_rdma_init: btl %p already supports rdma", (void *)btl));
        return OPAL_SUCCESS;
    }

    /*
     * note that it is not safe to access any am rdma functionality
     * (even default_component global data) until internal_init returns
     * successfully.
     */
    ret = am_rdma_internal_init(btl, 0, false, &am_module);
    if (OPAL_SUCCESS != ret) {
        BTL_VERBOSE(("am_rdma_init: btl %p internal_init failure %d",
                     (void *)btl, ret));
        return ret;
    }

    /*
     * we can't lock any field on the BTL structure (because it's not
     * ours to poke at), so take the global am rdma lock.  I suppose we
     * could do a cswap of the btl_am_data pointer to the same result,
     * but that seems too cute for something that should be a relatively
     * rare event.
     */
    opal_mutex_lock(&default_component.mutex);
    if (NULL != btl->btl_am_data) {
        BTL_VERBOSE(("am_rdma_init: btl %p already initialized", (void *)btl));
        am_rdma_internal_fini(am_module);
        opal_mutex_unlock(&default_component.mutex);
        return OPAL_SUCCESS;
    }
    opal_mutex_unlock(&default_component.mutex);

    btl->btl_am_data = am_module;

    /* TODO: Ideally, we would swap the BTL's flush for our own
     * implementation which completed all outstanding transactions on
     * that BTL and then called the underlying flush().  Given the
     * work and the lack of use case today, we instead just remove
     * flush support from the underlying BTL. */
    btl->btl_flush = NULL;

    if (!(btl->btl_flags & MCA_BTL_FLAGS_PUT)) {
        btl->btl_flags |= MCA_BTL_FLAGS_PUT_AM;
        btl->btl_put_limit = am_module->am_btl_put_limit;
        btl->btl_put_alignment = am_module->am_btl_put_alignment;
        btl->btl_put = am_rdma_put_wrapper;
        BTL_VERBOSE(("am_rdma_init: Enabling AM-based RDMA put for BTL %p. max put = %zu", (void*)btl, btl->btl_put_limit));
    }

    if (!(btl->btl_flags & MCA_BTL_FLAGS_GET)) {
        btl->btl_flags |= MCA_BTL_FLAGS_GET_AM;
        btl->btl_get_limit = am_module->am_btl_get_limit;
        btl->btl_get_alignment = am_module->am_btl_get_alignment;
        btl->btl_get = am_rdma_get_wrapper;
        BTL_VERBOSE(("Enabling AM-based RDMA get for BTL %p. max get = %zu", (void*)btl, btl->btl_get_limit));
    }

    if (!(btl->btl_flags & MCA_BTL_FLAGS_ATOMIC_FOPS)) {
        btl->btl_flags |= MCA_BTL_FLAGS_ATOMIC_AM_FOP;

        btl->btl_atomic_fop = am_rdma_fop_wrapper;
        btl->btl_atomic_cswap = am_rdma_cswap_wrapper;

        /* emulated RDMA atomics can support the full range of atomics. for
         * now only a handful are supported. */
        btl->btl_atomic_flags = MCA_BTL_ATOMIC_SUPPORTS_GLOB | MCA_BTL_ATOMIC_SUPPORTS_CSWAP
                                | MCA_BTL_ATOMIC_SUPPORTS_32BIT | MCA_BTL_ATOMIC_SUPPORTS_ADD
                                | MCA_BTL_ATOMIC_SUPPORTS_AND | MCA_BTL_ATOMIC_SUPPORTS_OR
                                | MCA_BTL_ATOMIC_SUPPORTS_XOR | MCA_BTL_ATOMIC_SUPPORTS_SWAP
                                | MCA_BTL_ATOMIC_SUPPORTS_MIN | MCA_BTL_ATOMIC_SUPPORTS_MAX;
        BTL_VERBOSE(("Enabling AM-based FOPs get for BTL %p", (void*)btl));
    }

    return OPAL_SUCCESS;
}


int opal_btl_base_am_rdma_create(mca_btl_base_module_t *btl,
                                 uint32_t flags_requested,
                                 bool no_memory_registration,
                                 mca_btl_base_am_rdma_module_t **am_module)
{
    int ret;

    BTL_VERBOSE(("am_rdma_create: called for btl %s (%p)",
                 btl->btl_component->btl_version.mca_component_name, (void *)btl));

    ret = am_rdma_internal_init(btl, flags_requested, no_memory_registration, am_module);
    if (OPAL_SUCCESS != ret) {
        BTL_VERBOSE(("am_rdma_create: btl %p internal_init failure %d",
                     (void *)btl, ret));
        return ret;
    }

    return OPAL_SUCCESS;
}


int opal_btl_base_am_rdma_destroy(mca_btl_base_am_rdma_module_t *am_module)
{
    return am_rdma_internal_fini(am_module);
}
