/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2016 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "osc_rdma_comm.h"
#include "osc_rdma_sync.h"
#include "osc_rdma_request.h"
#include "osc_rdma_dynamic.h"

#include "ompi/mca/osc/base/osc_base_obj_convert.h"
#include "opal/align.h"

static int ompi_osc_rdma_get_contig (ompi_osc_rdma_sync_t *sync, ompi_osc_rdma_peer_t *peer, uint64_t source_address,
                                     mca_btl_base_registration_handle_t *source_handle, void *target_buffer, size_t size,
                                     ompi_osc_rdma_request_t *request);

static void ompi_osc_get_data_complete (struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                                        void *local_address, mca_btl_base_registration_handle_t *local_handle,
                                        void *context, void *data, int status)
{
    assert (OPAL_SUCCESS == status);
    ((bool *) context)[0]  = true;
}

int ompi_osc_get_data_blocking (ompi_osc_rdma_module_t *module, struct mca_btl_base_endpoint_t *endpoint,
                                uint64_t source_address, mca_btl_base_registration_handle_t *source_handle,
                                void *data, size_t len)
{
    mca_btl_base_registration_handle_t *local_handle = NULL;
    ompi_osc_rdma_frag_t *frag = NULL;
    volatile bool read_complete = false;
    char *ptr = data;
    int ret;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "reading state data from endpoint %p. source: 0x%" PRIx64 ", len: %lu",
                     (void *) endpoint, source_address, (unsigned long) len);

    if (module->selected_btl->btl_register_mem && len >= module->selected_btl->btl_get_local_registration_threshold) {
        ret = ompi_osc_rdma_frag_alloc (module, len, &frag, &ptr);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_ERROR, "error allocating temporary buffer");
            return ret;
        }

        local_handle = frag->handle;
        OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "allocated temporary buffer %p in fragment %p", ptr,
                         (void *) frag);
    }

    assert (!(source_address & ALIGNMENT_MASK(module->selected_btl->btl_get_alignment)));

    do {
        ret = module->selected_btl->btl_get (module->selected_btl, endpoint, ptr, source_address,
                                             local_handle, source_handle, len, 0, MCA_BTL_NO_ORDER,
                                             ompi_osc_get_data_complete, (void *) &read_complete, NULL);
        if (OPAL_LIKELY(OMPI_ERR_OUT_OF_RESOURCE != ret)) {
            break;
        }

        ompi_osc_rdma_progress (module);
    } while (1);

    if (OPAL_UNLIKELY(OMPI_SUCCESS > ret)) {
        OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_ERROR, "btl get failed with opal error code %d", ret);

        if (frag) {
            ompi_osc_rdma_frag_complete (frag);
        }

        return ret;
    }

    /* block until the callback is called */
    while (!read_complete) {
        ompi_osc_rdma_progress (module);
    }

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "finished reading state data from endpoint %p", (void *) endpoint);

    opal_memchecker_base_mem_defined (ptr, len);

    if (frag) {
        memcpy (data, ptr, len);

        /* done with the fragment */
        ompi_osc_rdma_frag_complete (frag);
    }

    return OMPI_SUCCESS;
}

/**
 * @brief function signature for the rdma transfer function used by ompi_osc_rdma_master_noncontig()
 *
 * @param[in] peer            peer object for remote peer
 * @param[in] remote_address  base of remote region (destination for put, source for get)
 * @param[in] remote_handle   btl registration handle for remote region (must be valid for the entire region)
 * @param[in] local_address   base of local region (source for put, destination for get)
 * @param[in] size            number of bytes to transfer
 * @param[in] module          osc rdma module
 * @param[in] request         osc rdma request if used (can be NULL)
 *
 * @returns OMPI_SUCCESS on success
 * @returns OMPI_ERR_OUT_OF_RESOURCE on temporary error
 * @returns other OMPI error on fatal error
 *
 * This function does the work of scheduling a contiguous transfer between the local and remote regions.
 */
typedef int (*ompi_osc_rdma_fn_t) (ompi_osc_rdma_sync_t *sync, ompi_osc_rdma_peer_t *peer, uint64_t remote_address,
                                   mca_btl_base_registration_handle_t *remote_handle, void *local_address, size_t size,
                                   ompi_osc_rdma_request_t *request);

/**
 * @brief break down rdma transaction into contiguous regions
 *
 * @param[in] local_address    base of local region (source for put, destination for get)
 * @param[in] local_count      number of elements in local region
 * @param[in] local_datatype   datatype of local region
 * @param[in] peer             peer object for remote peer
 * @param[in] remote_address   base of remote region (destination for put, source for get)
 * @param[in] remote_handle    btl registration handle for remote region (must be valid for the entire region)
 * @param[in] remote_count     number of elements in remote region
 * @param[in] remote_datatype  datatype of remote region
 * @param[in] module           osc rdma module
 * @param[in] request          osc rdma request if used (can be NULL)
 * @param[in] max_rdma_len     maximum length of an rdma request (usually btl limitation)
 * @param[in] rdma_fn          function to use for contiguous rdma operations
 * @param[in] alloc_reqs       true if rdma_fn requires a valid request object (any allocated objects will be marked internal)
 *
 * This function does the work of breaking a non-contiguous rdma transfer into contiguous components. It will
 * continue to submit rdma transfers until the entire region is transferred or a fatal error occurs.
 */
static int ompi_osc_rdma_master_noncontig (ompi_osc_rdma_sync_t *sync, void *local_address, int local_count, ompi_datatype_t *local_datatype,
                                           ompi_osc_rdma_peer_t *peer, uint64_t remote_address,
                                           mca_btl_base_registration_handle_t *remote_handle, int remote_count,
                                           ompi_datatype_t *remote_datatype, ompi_osc_rdma_request_t *request, const size_t max_rdma_len,
                                           const ompi_osc_rdma_fn_t rdma_fn, const bool alloc_reqs)
{
    ompi_osc_rdma_module_t *module = sync->module;
    struct iovec local_iovec[OMPI_OSC_RDMA_DECODE_MAX], remote_iovec[OMPI_OSC_RDMA_DECODE_MAX];
    opal_convertor_t local_convertor, remote_convertor;
    uint32_t local_iov_count, remote_iov_count;
    uint32_t local_iov_index, remote_iov_index;
    /* needed for opal_convertor_raw but not used */
    size_t local_size, remote_size, rdma_len;
    ompi_osc_rdma_request_t *subreq;
    int ret;
    bool done;

    subreq = NULL;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "scheduling rdma on non-contiguous datatype(s)");

    /* prepare convertors for the source and target. these convertors will be used to determine the
     * contiguous segments within the source and target. */
    OBJ_CONSTRUCT(&remote_convertor, opal_convertor_t);
    ret = opal_convertor_copy_and_prepare_for_send (ompi_mpi_local_convertor, &remote_datatype->super, remote_count,
                                                    (void *) (intptr_t) remote_address, 0, &remote_convertor);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    OBJ_CONSTRUCT(&local_convertor, opal_convertor_t);
    ret = opal_convertor_copy_and_prepare_for_send (ompi_mpi_local_convertor, &local_datatype->super, local_count,
                                                    local_address, 0, &local_convertor);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    if (request) {
        /* keep the request from completing until all the transfers have started */
        request->outstanding_requests = 1;
    }

    local_iov_index = 0;
    local_iov_count = 0;

    do {
        /* decode segments of the remote data */
        remote_iov_count = OMPI_OSC_RDMA_DECODE_MAX;
        remote_iov_index = 0;

        /* opal_convertor_raw returns done when it has reached the end of the data */
        done = opal_convertor_raw (&remote_convertor, remote_iovec, &remote_iov_count, &remote_size);

        /* loop on the target segments until we have exhaused the decoded source data */
        while (remote_iov_index != remote_iov_count) {
            if (local_iov_index == local_iov_count) {
                /* decode segments of the target buffer */
                local_iov_count = OMPI_OSC_RDMA_DECODE_MAX;
                local_iov_index = 0;
                (void) opal_convertor_raw (&local_convertor, local_iovec, &local_iov_count, &local_size);
            }

            /* we already checked that the target was large enough. this should be impossible */
            assert (0 != local_iov_count);

            /* determine how much to transfer in this operation */
            rdma_len = min(min(local_iovec[local_iov_index].iov_len, remote_iovec[remote_iov_index].iov_len), max_rdma_len);

            /* execute the get */
            if (!subreq && alloc_reqs) {
                OMPI_OSC_RDMA_REQUEST_ALLOC(module, peer, subreq);
                subreq->internal = true;
                subreq->type = OMPI_OSC_RDMA_TYPE_RDMA;
                subreq->parent_request = request;

                if (request) {
                    (void) OPAL_THREAD_ADD32 (&request->outstanding_requests, 1);
                }
            } else if (!alloc_reqs) {
                subreq = request;
            }

            OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "performing rdma on contiguous region. local: %p, remote: %p, len: %lu",
                             local_iovec[local_iov_index].iov_base, remote_iovec[remote_iov_index].iov_base,
                             (unsigned long) remote_iovec[remote_iov_index].iov_len);

            ret = rdma_fn (sync, peer, (uint64_t) (intptr_t) remote_iovec[remote_iov_index].iov_base, remote_handle,
                           local_iovec[local_iov_index].iov_base, rdma_len, subreq);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                if (OPAL_UNLIKELY(OMPI_ERR_OUT_OF_RESOURCE != ret)) {
                    if (request) {
                        (void) OPAL_THREAD_ADD32 (&request->outstanding_requests, -1);
                    }

                    if (alloc_reqs) {
                        OMPI_OSC_RDMA_REQUEST_RETURN(subreq);
                    }

                    /* something bad happened. need to figure out best way to handle rma errors */
                    return ret;
                }

                /* progress and try again */
                ompi_osc_rdma_progress (module);
                continue;
            }
            subreq = NULL;

            /* adjust io vectors */
            local_iovec[local_iov_index].iov_len -= rdma_len;
            remote_iovec[remote_iov_index].iov_len -= rdma_len;
            local_iovec[local_iov_index].iov_base = (void *)((intptr_t) local_iovec[local_iov_index].iov_base + rdma_len);
            remote_iovec[remote_iov_index].iov_base = (void *)((intptr_t) remote_iovec[remote_iov_index].iov_base + rdma_len);

            local_iov_index += (0 == local_iovec[local_iov_index].iov_len);
            remote_iov_index += (0 == remote_iovec[remote_iov_index].iov_len);
        }
    } while (!done);

    if (request) {
        /* release our reference so the request can complete */
        if (1 == request->outstanding_requests) {
            ompi_osc_rdma_request_complete (request, OMPI_SUCCESS);
        }

        (void) OPAL_THREAD_ADD32 (&request->outstanding_requests, -1);
    }

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "finished scheduling rdma on non-contiguous datatype(s)");

    /* clean up convertors */
    opal_convertor_cleanup (&local_convertor);
    OBJ_DESTRUCT(&local_convertor);
    opal_convertor_cleanup (&remote_convertor);
    OBJ_DESTRUCT(&remote_convertor);

    return OMPI_SUCCESS;
}

static inline int ompi_osc_rdma_master (ompi_osc_rdma_sync_t *sync, void *local_address, int local_count,
                                        ompi_datatype_t *local_datatype, ompi_osc_rdma_peer_t *peer,
                                        uint64_t remote_address, mca_btl_base_registration_handle_t *remote_handle,
                                        int remote_count, ompi_datatype_t *remote_datatype,
                                        ompi_osc_rdma_request_t *request, const size_t max_rdma_len,
                                        const ompi_osc_rdma_fn_t rdma_fn, const bool alloc_reqs)
{
    size_t rdma_len;
    ptrdiff_t lb, extent;
    int ret;

    rdma_len = local_datatype->super.size * local_count;

    /* fast path for contiguous rdma */
    if (OPAL_LIKELY(ompi_datatype_is_contiguous_memory_layout (local_datatype, local_count) &&
                    ompi_datatype_is_contiguous_memory_layout (remote_datatype, remote_count) &&
                    rdma_len <= max_rdma_len)) {
        if (NULL == request && alloc_reqs) {
            ompi_osc_rdma_module_t *module = sync->module;
            OMPI_OSC_RDMA_REQUEST_ALLOC(module, peer, request);
            request->internal = true;
            request->type = OMPI_OSC_RDMA_TYPE_RDMA;
        }

        /* ignore failure here */
        (void) ompi_datatype_get_extent (local_datatype, &lb, &extent);
        local_address = (void *)((intptr_t) local_address + lb);

        (void) ompi_datatype_get_extent (remote_datatype, &lb, &extent);
        remote_address += lb;

        OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "performing rdma on contiguous region. local: %p, "
                         "remote: 0x%lx, length: %lu", local_address, (unsigned long) remote_address,
                         rdma_len);

        do {
            ret = rdma_fn (sync, peer, remote_address, remote_handle, local_address, rdma_len, request);
            if (OPAL_LIKELY(OPAL_SUCCESS == ret)) {
                return OMPI_SUCCESS;
            }

            ompi_osc_rdma_progress (sync->module);
        } while (1);
    }

    return ompi_osc_rdma_master_noncontig (sync, local_address, local_count, local_datatype, peer, remote_address,
                                           remote_handle, remote_count, remote_datatype, request,
                                           max_rdma_len, rdma_fn, alloc_reqs);
}

static int ompi_osc_rdma_copy_local (const void *source, int source_count, ompi_datatype_t *source_datatype,
                                     void *target, int target_count, ompi_datatype_t *target_datatype,
                                     ompi_osc_rdma_request_t *request)
{
    int ret;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "performing local copy from %p -> %p", source, target);

    opal_atomic_mb ();
    ret = ompi_datatype_sndrcv (source, source_count, source_datatype, target, target_count, target_datatype);

    if (request) {
        ompi_osc_rdma_request_complete (request, ret);
    }

    return ret;
}

static void ompi_osc_rdma_put_complete (struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                                        void *local_address, mca_btl_base_registration_handle_t *local_handle,
                                        void *context, void *data, int status)
{
    ompi_osc_rdma_sync_t *sync = (ompi_osc_rdma_sync_t *) context;
    ompi_osc_rdma_frag_t *frag = (ompi_osc_rdma_frag_t *) data;
    ompi_osc_rdma_request_t *request = NULL;

    assert (OPAL_SUCCESS == status);

    /* the lowest bit is used as a flag indicating this put operation has a request */
    if ((intptr_t) context & 0x1) {
        request = (ompi_osc_rdma_request_t *) ((intptr_t) context & ~1);
        sync = request->sync;

        /* NTH -- TODO: better error handling */
        ompi_osc_rdma_request_complete (request, status);
    }

    OSC_RDMA_VERBOSE(status ? MCA_BASE_VERBOSE_ERROR : MCA_BASE_VERBOSE_TRACE, "btl put complete on sync %p. local "
                     "address %p. opal status %d", (void *) sync, local_address, status);

    if (frag) {
        ompi_osc_rdma_frag_complete (frag);
    } else {
        ompi_osc_rdma_deregister (sync->module, local_handle);
    }

    ompi_osc_rdma_sync_rdma_dec (sync);
}

static void ompi_osc_rdma_aggregate_put_complete (struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                                                  void *local_address, mca_btl_base_registration_handle_t *local_handle,
                                                  void *context, void *data, int status)
{
    ompi_osc_rdma_aggregation_t *aggregation = (ompi_osc_rdma_aggregation_t *) context;
    ompi_osc_rdma_sync_t *sync = aggregation->sync;
    ompi_osc_rdma_frag_t *frag = aggregation->frag;

    assert (OPAL_SUCCESS == status);

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "aggregate put complete %p on sync %p. local address %p. status %d",
                     (void *) aggregation, (void *) sync, local_address, status);

    ompi_osc_rdma_frag_complete (frag);
    ompi_osc_rdma_aggregation_return (aggregation);

    /* make sure the aggregation is returned before marking the operation as complete */
    opal_atomic_wmb ();

    ompi_osc_rdma_sync_rdma_dec (sync);
}

static int ompi_osc_rdma_put_real (ompi_osc_rdma_sync_t *sync, ompi_osc_rdma_peer_t *peer, uint64_t target_address,
                                   mca_btl_base_registration_handle_t *target_handle, void *ptr,
                                   mca_btl_base_registration_handle_t *local_handle, size_t size,
                                   mca_btl_base_rdma_completion_fn_t cb, void *context, void *cbdata) {
    ompi_osc_rdma_module_t *module = sync->module;
    int ret;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "initiating btl put of %lu bytes to remote address %" PRIx64 ", sync "
                     "object %p...", (unsigned long) size, target_address, (void *) sync);

    /* flag outstanding rma requests */
    ompi_osc_rdma_sync_rdma_inc (sync);

    do {
        ret = module->selected_btl->btl_put (module->selected_btl, peer->data_endpoint, ptr, target_address,
                                             local_handle, target_handle, size, 0, MCA_BTL_NO_ORDER,
                                             cb, context, cbdata);
        if (OPAL_UNLIKELY(OMPI_SUCCESS == ret)) {
            return OMPI_SUCCESS;
        }

        ++module->put_retry_count;

        if (OPAL_ERR_OUT_OF_RESOURCE != ret && OPAL_ERR_TEMP_OUT_OF_RESOURCE != ret) {
            break;
        }

        /* spin a bit on progress */
        for (int i = 0 ; i < 10 ; ++i) {
            ompi_osc_rdma_progress (module);
        }
    } while (1);

    OSC_RDMA_VERBOSE(10, "btl put failed with opal error code %d", ret);

    return ret;
}

static void ompi_osc_rdma_aggregate_append (ompi_osc_rdma_aggregation_t *aggregation, ompi_osc_rdma_request_t *request,
                                            void *source_buffer, size_t size)
{
    size_t offset = aggregation->buffer_used;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "appending %lu bytes of data from %p to aggregate fragment %p with start "
                     "address 0x%lx", (unsigned long) size, source_buffer, (void *) aggregation,
                     (unsigned long) aggregation->target_address);

    memcpy (aggregation->buffer + offset, source_buffer, size);

    aggregation->buffer_used += size;

    if (request) {
        /* the local buffer is now available */
        ompi_osc_rdma_request_complete (request, 0);
    }
}

static int ompi_osc_rdma_aggregate_alloc (ompi_osc_rdma_sync_t *sync, ompi_osc_rdma_peer_t *peer, uint64_t target_address,
                                          mca_btl_base_registration_handle_t *target_handle, void *source_buffer, size_t size,
                                          ompi_osc_rdma_request_t *request, int type)
{
    ompi_osc_rdma_module_t *module = sync->module;
    ompi_osc_rdma_aggregation_t *aggregation;
    int ret;

    aggregation = (ompi_osc_rdma_aggregation_t *) opal_free_list_get (&mca_osc_rdma_component.aggregate);
    if (OPAL_UNLIKELY(NULL == aggregation)) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    ret = ompi_osc_rdma_frag_alloc (module, mca_osc_rdma_component.aggregation_limit, &aggregation->frag,
                                    &aggregation->buffer);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        opal_free_list_return(&mca_osc_rdma_component.aggregate, (opal_free_list_item_t *) aggregation);
        return ret;
    }

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "allocated new aggregate fragment %p for target %d", (void *) aggregation,
                     peer->rank);

    peer->aggregate = aggregation;

    aggregation->target_address = target_address;
    aggregation->target_handle = target_handle;
    aggregation->buffer_size = mca_osc_rdma_component.aggregation_limit;
    aggregation->sync = sync;
    aggregation->peer = peer;
    aggregation->type = type;
    aggregation->buffer_used = 0;

    ompi_osc_rdma_aggregate_append (aggregation, request, source_buffer, size);

    opal_list_append (&sync->aggregations, (opal_list_item_t *) aggregation);

    return OMPI_SUCCESS;
}

static int ompi_osc_rdma_put_contig (ompi_osc_rdma_sync_t *sync, ompi_osc_rdma_peer_t *peer, uint64_t target_address,
                                     mca_btl_base_registration_handle_t *target_handle, void *source_buffer, size_t size,
                                     ompi_osc_rdma_request_t *request)
{
    ompi_osc_rdma_module_t *module = sync->module;
    ompi_osc_rdma_aggregation_t *aggregation = peer->aggregate;
    mca_btl_base_registration_handle_t *local_handle = NULL;
    ompi_osc_rdma_frag_t *frag = NULL;
    char *ptr = source_buffer;
    void *cbcontext;
    int ret;

    if (aggregation) {
        if (size <= (aggregation->buffer_size - aggregation->buffer_used) && (target_handle == aggregation->target_handle) &&
            (target_address == aggregation->target_address + aggregation->buffer_used)) {
            assert (OMPI_OSC_RDMA_TYPE_PUT == aggregation->type);
            ompi_osc_rdma_aggregate_append (aggregation, request, source_buffer, size);
            return OMPI_SUCCESS;
        }

        /* can't aggregate this operation. flush the previous segment */
        ret = ompi_osc_rdma_peer_aggregate_flush (peer);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            return ret;
        }
    }

    if (size <= (mca_osc_rdma_component.aggregation_limit >> 2)) {
        ret = ompi_osc_rdma_aggregate_alloc (sync, peer, target_address, target_handle, source_buffer, size, request,
                                             OMPI_OSC_RDMA_TYPE_PUT);
        if (OPAL_LIKELY(OMPI_SUCCESS == ret)) {
            if (request) {

            }
            return ret;
        }
    }

    if (module->selected_btl->btl_register_mem && size > module->selected_btl->btl_put_local_registration_threshold) {
        ret = ompi_osc_rdma_frag_alloc (module, size, &frag, &ptr);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            ret = ompi_osc_rdma_register (module, peer->data_endpoint, source_buffer, size, 0, &local_handle);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                return ret;
            }
        } else {
            memcpy (ptr, source_buffer, size);
            local_handle = frag->handle;
        }
    }

    /* increment the outstanding request counter in the request object */
    if (request) {
        (void) OPAL_THREAD_ADD32 (&request->outstanding_requests, 1);
        cbcontext = (void *) ((intptr_t) request | 1);
        request->sync = sync;
    } else {
        cbcontext = (void *) sync;
    }

    ret = ompi_osc_rdma_put_real (sync, peer, target_address, target_handle, ptr, local_handle, size, ompi_osc_rdma_put_complete,
                                  cbcontext, frag);
    if (OPAL_UNLIKELY(OMPI_SUCCESS == ret)) {
        return OMPI_SUCCESS;
    }

    ompi_osc_rdma_cleanup_rdma (sync, frag, local_handle, request);

    return ret;
}

static void ompi_osc_rdma_get_complete (struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                                        void *local_address, mca_btl_base_registration_handle_t *local_handle,
                                        void *context, void *data, int status)
{
    ompi_osc_rdma_request_t *request = (ompi_osc_rdma_request_t *) context;
    intptr_t source = (intptr_t) local_address + request->offset;
    ompi_osc_rdma_frag_t *frag = (ompi_osc_rdma_frag_t *) data;
    ompi_osc_rdma_sync_t *sync = request->sync;
    void *origin_addr = request->origin_addr;

    OSC_RDMA_VERBOSE(status ? MCA_BASE_VERBOSE_ERROR : MCA_BASE_VERBOSE_TRACE, "btl get complete on sync %p. local "
                     "address %p. origin %p. opal status %d", (void *) sync, local_address, origin_addr, status);

    assert (OPAL_SUCCESS == status);

    if (request->buffer || NULL != frag) {
        if (OPAL_LIKELY(OMPI_SUCCESS == status)) {
            memcpy (origin_addr, (void *) source, request->len);
        }
    }

    if (NULL != frag) {
        ompi_osc_rdma_frag_complete (frag);
    } else {
        ompi_osc_rdma_deregister (sync->module, local_handle);
    }

    ompi_osc_rdma_sync_rdma_dec (sync);

    ompi_osc_rdma_request_complete (request, status);
}

int ompi_osc_rdma_peer_aggregate_flush (ompi_osc_rdma_peer_t *peer)
{
    ompi_osc_rdma_aggregation_t *aggregation = peer->aggregate;
    int ret;

    if (NULL == aggregation) {
        return OMPI_SUCCESS;
    }

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "flusing aggregate fragment %p", (void *) aggregation);

    assert (OMPI_OSC_RDMA_TYPE_PUT == aggregation->type);

    ret = ompi_osc_rdma_put_real (aggregation->sync, peer, aggregation->target_address, aggregation->target_handle,
                                  aggregation->buffer, aggregation->frag->handle, aggregation->buffer_used,
                                  ompi_osc_rdma_aggregate_put_complete, (void *) aggregation, NULL);

    peer->aggregate = NULL;

    if (OPAL_UNLIKELY(OMPI_SUCCESS == ret)) {
        return OMPI_SUCCESS;
    }

    ompi_osc_rdma_cleanup_rdma (aggregation->sync, aggregation->frag, NULL, NULL);

    ompi_osc_rdma_aggregation_return (aggregation);

    return ret;

}

static int ompi_osc_rdma_get_partial (ompi_osc_rdma_sync_t *sync, ompi_osc_rdma_peer_t *peer, uint64_t source_address,
                                      mca_btl_base_registration_handle_t *source_handle, void *target_buffer, size_t size,
                                      ompi_osc_rdma_request_t *request) {
    ompi_osc_rdma_module_t *module = sync->module;
    ompi_osc_rdma_request_t *subreq;
    int ret;

    OMPI_OSC_RDMA_REQUEST_ALLOC(module, peer, subreq);
    subreq->internal = true;
    subreq->type = OMPI_OSC_RDMA_TYPE_RDMA;
    subreq->parent_request = request;
    (void) OPAL_THREAD_ADD32 (&request->outstanding_requests, 1);

    ret = ompi_osc_rdma_get_contig (sync, peer, source_address, source_handle, target_buffer, size, subreq);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        OMPI_OSC_RDMA_REQUEST_RETURN(subreq);
        (void) OPAL_THREAD_ADD32 (&request->outstanding_requests, -1);
    }

    return ret;
}

static int ompi_osc_rdma_get_contig (ompi_osc_rdma_sync_t *sync, ompi_osc_rdma_peer_t *peer, uint64_t source_address,
                                     mca_btl_base_registration_handle_t *source_handle, void *target_buffer, size_t size,
                                     ompi_osc_rdma_request_t *request)
{
    ompi_osc_rdma_module_t *module = sync->module;
    const size_t btl_alignment_mask = ALIGNMENT_MASK(module->selected_btl->btl_get_alignment);
    mca_btl_base_registration_handle_t *local_handle = NULL;
    ompi_osc_rdma_frag_t *frag = NULL;
    osc_rdma_size_t aligned_len;
    osc_rdma_base_t aligned_source_base, aligned_source_bound;
    char *ptr = target_buffer;
    int ret;

    aligned_source_base = source_address & ~btl_alignment_mask;
    aligned_source_bound = (source_address + size + btl_alignment_mask) & ~btl_alignment_mask;
    aligned_len = aligned_source_bound - aligned_source_base;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "initiating get of %lu bytes from remote ptr %" PRIx64 " to local ptr %p",
                     size, source_address, target_buffer);

    if ((module->selected_btl->btl_register_mem && size > module->selected_btl->btl_get_local_registration_threshold) ||
        (((uint64_t) target_buffer | size | source_address) & btl_alignment_mask)) {

        ret = ompi_osc_rdma_frag_alloc (module, aligned_len, &frag, &ptr);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            if (OMPI_ERR_VALUE_OUT_OF_BOUNDS == ret) {
                /* region is too large for a buffered read */
                size_t subsize;

                if ((source_address & btl_alignment_mask) && (source_address & btl_alignment_mask) == ((intptr_t) target_buffer & btl_alignment_mask)) {
                    /* remote region has the same alignment but the base is not aligned. perform a small
                     * buffered get of the beginning of the remote region */
                    aligned_source_base = OPAL_ALIGN(source_address, module->selected_btl->btl_get_alignment, osc_rdma_base_t);
                    subsize = (size_t) (aligned_source_base - source_address);

                    ret = ompi_osc_rdma_get_partial (sync, peer, source_address, source_handle, target_buffer, subsize, request);
                    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                        return ret;
                    }

                    source_address += subsize;
                    target_buffer = (void *) ((intptr_t) target_buffer + subsize);
                    size -= subsize;

                    aligned_len = aligned_source_bound - aligned_source_base;
                }

                if (!(((uint64_t) target_buffer | source_address) & btl_alignment_mask) &&
                    (size & btl_alignment_mask)) {
                    /* remote region bases are aligned but the bounds are not. perform a
                     * small buffered get of the end of the remote region */
                    aligned_len = size & ~btl_alignment_mask;
                    subsize = size - aligned_len;
                    size = aligned_len;
                    ret = ompi_osc_rdma_get_partial (sync, peer, source_address + aligned_len, source_handle,
                                                     (void *) ((intptr_t) target_buffer + aligned_len), subsize, request);
                    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                        return ret;
                    }
                }
                /* (remaining) user request is now correctly aligned */
            }

            if ((((uint64_t) target_buffer | size | source_address) & btl_alignment_mask)) {
                /* local and remote alignments differ */
                request->buffer = ptr = malloc (aligned_len);
            } else {
                ptr = target_buffer;
            }

            if (NULL != ptr) {
                (void) ompi_osc_rdma_register (module, peer->data_endpoint, ptr, aligned_len, MCA_BTL_REG_FLAG_LOCAL_WRITE,
                                               &local_handle);
            }

            if (OPAL_UNLIKELY(NULL == local_handle)) {
                free (request->buffer);
                request->buffer = NULL;
                return ret;
            }
        } else {
            OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "using internal buffer %p in fragment %p for get of size %lu bytes, source address 0x%lx",
                             ptr, (void *) frag, aligned_len, (unsigned long) aligned_source_base);
            local_handle = frag->handle;
        }
    }

    request->offset = source_address - aligned_source_base;
    request->len = size;
    request->origin_addr = target_buffer;
    request->sync = sync;

    ompi_osc_rdma_sync_rdma_inc (sync);

    do {
        ret = module->selected_btl->btl_get (module->selected_btl, peer->data_endpoint, ptr, aligned_source_base, local_handle,
                                             source_handle, aligned_len, 0, MCA_BTL_NO_ORDER, ompi_osc_rdma_get_complete,
                                             request, frag);
        if (OPAL_UNLIKELY(OMPI_SUCCESS == ret)) {
            return OMPI_SUCCESS;
        }

        ++module->get_retry_count;

        if (OPAL_ERR_OUT_OF_RESOURCE != ret && OPAL_ERR_TEMP_OUT_OF_RESOURCE != ret) {
            break;
        }

        /* spin a bit on progress */
        for (int i = 0 ; i < 10 ; ++i) {
            ompi_osc_rdma_progress (module);
        }
    } while (1);

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_ERROR, "btl get failed with opal error code %d", ret);

    ompi_osc_rdma_cleanup_rdma (sync, frag, local_handle, request);

    return ret;
}

static inline int ompi_osc_rdma_put_w_req (ompi_osc_rdma_sync_t *sync, const void *origin_addr, int origin_count,
                                           struct ompi_datatype_t *origin_datatype, ompi_osc_rdma_peer_t *peer,
                                           OPAL_PTRDIFF_TYPE target_disp, int target_count,
                                           struct ompi_datatype_t *target_datatype, ompi_osc_rdma_request_t *request)
{
    ompi_osc_rdma_module_t *module = sync->module;
    mca_btl_base_registration_handle_t *target_handle;
    uint64_t target_address;
    int ret;

    /* short-circuit case */
    if (0 == origin_count || 0 == target_count) {
        if (request) {
            ompi_osc_rdma_request_complete (request, MPI_SUCCESS);
        }

        return OMPI_SUCCESS;
    }

    ret = osc_rdma_get_remote_segment (module, peer, target_disp, target_datatype->super.size * target_count,
                                       &target_address, &target_handle);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    /* optimize communication with peers that we can do direct load and store operations on */
    if (ompi_osc_rdma_peer_local_base (peer)) {
        return ompi_osc_rdma_copy_local (origin_addr, origin_count, origin_datatype, (void *) (intptr_t) target_address,
                                         target_count, target_datatype, request);
    }

    return ompi_osc_rdma_master (sync, (void *) origin_addr, origin_count, origin_datatype, peer, target_address, target_handle,
                                 target_count, target_datatype, request, module->selected_btl->btl_put_limit,
                                 ompi_osc_rdma_put_contig, false);
}

static inline int ompi_osc_rdma_get_w_req (ompi_osc_rdma_sync_t *sync, void *origin_addr, int origin_count, struct ompi_datatype_t *origin_datatype,
                                           ompi_osc_rdma_peer_t *peer, OPAL_PTRDIFF_TYPE source_disp, int source_count,
                                           struct ompi_datatype_t *source_datatype, ompi_osc_rdma_request_t *request)
{
    ompi_osc_rdma_module_t *module = sync->module;
    mca_btl_base_registration_handle_t *source_handle;
    uint64_t source_address;
    int ret;

    /* short-circuit case */
    if (0 == origin_count || 0 == source_count) {
        if (request) {
            ompi_osc_rdma_request_complete (request, MPI_SUCCESS);
        }

        return OMPI_SUCCESS;
    }

    ret = osc_rdma_get_remote_segment (module, peer, source_disp, source_datatype->super.size * source_count,
                                       &source_address, &source_handle);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    /* optimize self/local communication */
    if (ompi_osc_rdma_peer_local_base (peer)) {
        return ompi_osc_rdma_copy_local ((void *) (intptr_t) source_address, source_count, source_datatype,
                                         origin_addr, origin_count, origin_datatype, request);
    }

    return ompi_osc_rdma_master (sync, origin_addr, origin_count, origin_datatype, peer, source_address,
                                 source_handle, source_count, source_datatype, request,
                                 module->selected_btl->btl_get_limit, ompi_osc_rdma_get_contig, true);
}
int ompi_osc_rdma_put (const void *origin_addr, int origin_count, struct ompi_datatype_t *origin_datatype,
                       int target_rank, OPAL_PTRDIFF_TYPE target_disp, int target_count,
                       struct ompi_datatype_t *target_datatype, ompi_win_t *win)
{
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    ompi_osc_rdma_peer_t *peer;
    ompi_osc_rdma_sync_t *sync;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "put: 0x%lx, %d, %s, %d, %d, %d, %s, %s", (unsigned long) origin_addr,
                     origin_count, origin_datatype->name, target_rank, (int) target_disp, target_count,
                     target_datatype->name, win->w_name);

    sync = ompi_osc_rdma_module_sync_lookup (module, target_rank, &peer);
    if (OPAL_UNLIKELY(NULL == sync)) {
        return OMPI_ERR_RMA_SYNC;
    }

    return ompi_osc_rdma_put_w_req (sync, origin_addr, origin_count, origin_datatype, peer, target_disp,
                                    target_count, target_datatype, NULL);
}

int ompi_osc_rdma_rput (const void *origin_addr, int origin_count, struct ompi_datatype_t *origin_datatype,
                        int target_rank, OPAL_PTRDIFF_TYPE target_disp, int target_count,
                        struct ompi_datatype_t *target_datatype, struct ompi_win_t *win,
                        struct ompi_request_t **request)
{
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    ompi_osc_rdma_peer_t *peer;
    ompi_osc_rdma_request_t *rdma_request;
    ompi_osc_rdma_sync_t *sync;
    int ret;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "rput: 0x%lx, %d, %s, %d, %d, %d, %s, %s", (unsigned long) origin_addr, origin_count,
                     origin_datatype->name, target_rank, (int) target_disp, target_count, target_datatype->name, win->w_name);

    sync = ompi_osc_rdma_module_sync_lookup (module, target_rank, &peer);
    if (OPAL_UNLIKELY(NULL == sync)) {
        return OMPI_ERR_RMA_SYNC;
    }

    OMPI_OSC_RDMA_REQUEST_ALLOC(module, peer, rdma_request);

    rdma_request->type = OMPI_OSC_RDMA_TYPE_PUT;

    ret = ompi_osc_rdma_put_w_req (sync, origin_addr, origin_count, origin_datatype, peer, target_disp,
                                   target_count, target_datatype, rdma_request);
    if (OMPI_SUCCESS != ret) {
        OMPI_OSC_RDMA_REQUEST_RETURN(rdma_request);
        return ret;
    }

    *request = (ompi_request_t *) rdma_request;

    return OMPI_SUCCESS;
}

int ompi_osc_rdma_get (void *origin_addr, int origin_count, struct ompi_datatype_t *origin_datatype,
                       int source_rank, OPAL_PTRDIFF_TYPE source_disp, int source_count,
                       struct ompi_datatype_t *source_datatype, struct ompi_win_t *win)
{
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    ompi_osc_rdma_peer_t *peer;
    ompi_osc_rdma_sync_t *sync;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "get: 0x%lx, %d, %s, %d, %d, %d, %s, %s", (unsigned long) origin_addr,
                     origin_count, origin_datatype->name, source_rank, (int) source_disp, source_count,
                     source_datatype->name, win->w_name);

    sync = ompi_osc_rdma_module_sync_lookup (module, source_rank, &peer);
    if (OPAL_UNLIKELY(NULL == sync)) {
        return OMPI_ERR_RMA_SYNC;
    }

    return ompi_osc_rdma_get_w_req (sync, origin_addr, origin_count, origin_datatype, peer,
                                    source_disp, source_count, source_datatype, NULL);
}

int ompi_osc_rdma_rget (void *origin_addr, int origin_count, struct ompi_datatype_t *origin_datatype,
                        int source_rank, OPAL_PTRDIFF_TYPE source_disp, int source_count,
                        struct ompi_datatype_t *source_datatype, struct ompi_win_t *win,
                        struct ompi_request_t **request)
{
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    ompi_osc_rdma_peer_t *peer;
    ompi_osc_rdma_request_t *rdma_request;
    ompi_osc_rdma_sync_t *sync;
    int ret;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "rget: 0x%lx, %d, %s, %d, %d, %d, %s, %s", (unsigned long) origin_addr,
                     origin_count, origin_datatype->name, source_rank, (int) source_disp, source_count,
                     source_datatype->name, win->w_name);

    sync = ompi_osc_rdma_module_sync_lookup (module, source_rank, &peer);
    if (OPAL_UNLIKELY(NULL == sync)) {
        return OMPI_ERR_RMA_SYNC;
    }

    OMPI_OSC_RDMA_REQUEST_ALLOC(module, peer, rdma_request);

    rdma_request->type = OMPI_OSC_RDMA_TYPE_GET;
    ret = ompi_osc_rdma_get_w_req (sync, origin_addr, origin_count, origin_datatype, peer,
                                   source_disp, source_count, source_datatype, rdma_request);
    if (OMPI_SUCCESS != ret) {
        OMPI_OSC_RDMA_REQUEST_RETURN(rdma_request);
        return ret;
    }

    *request = (ompi_request_t *) rdma_request;

    return OMPI_SUCCESS;
}
