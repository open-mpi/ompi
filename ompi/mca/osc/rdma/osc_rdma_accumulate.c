/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2015 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2017 IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "osc_rdma_accumulate.h"
#include "osc_rdma_request.h"
#include "osc_rdma_comm.h"
#include "opal/util/show_help.h"

#include "ompi/mca/osc/base/osc_base_obj_convert.h"

static int ompi_osc_rdma_gacc_local (const void *source_buffer, int source_count, ompi_datatype_t *source_datatype,
                                     void *result_buffer, int result_count, ompi_datatype_t *result_datatype,
                                     ompi_osc_rdma_peer_t *peer, uint64_t target_address,
                                     mca_btl_base_registration_handle_t *target_handle, int target_count,
                                     ompi_datatype_t *target_datatype, ompi_op_t *op, ompi_osc_rdma_module_t *module,
                                     ompi_osc_rdma_request_t *request)
{
    int ret = OMPI_SUCCESS;

    do {
        OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "performing accumulate with local regions");

        if (!ompi_osc_rdma_peer_is_exclusive (peer)) {
            (void) ompi_osc_rdma_lock_acquire_exclusive (module, peer, offsetof (ompi_osc_rdma_state_t, accumulate_lock));
        }

        if (NULL != result_buffer) {
            /* get accumulate */

            ret = ompi_datatype_sndrcv ((void *) (intptr_t) target_address, target_count, target_datatype,
                                        result_buffer, result_count, result_datatype);

            if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                break;
            }
        }

        if (&ompi_mpi_op_no_op.op != op) {
            if (&ompi_mpi_op_replace.op != op) {
                ret = ompi_osc_base_sndrcv_op (source_buffer, source_count, source_datatype, (void *) (intptr_t) target_address,
                                               target_count, target_datatype, op);
            } else {
                ret = ompi_datatype_sndrcv (source_buffer, source_count, source_datatype, (void *) (intptr_t) target_address,
                                            target_count, target_datatype);
            }
        }

        if (!ompi_osc_rdma_peer_is_exclusive (peer)) {
            (void) ompi_osc_rdma_lock_release_exclusive (module, peer, offsetof (ompi_osc_rdma_state_t, accumulate_lock));
        }
    } while (0);

    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_ERROR, "local accumulate failed with ompi error code %d", ret);
        return ret;
    }

    if (request) {
        /* NTH: is it ok to use an ompi error code here? */
        ompi_osc_rdma_request_complete (request, ret);
    }

    return ret;
}

static inline int ompi_osc_rdma_cas_local (const void *source_buffer, const void *compare_buffer, void *result_buffer,
                                           ompi_datatype_t *datatype, ompi_osc_rdma_peer_t *peer,
                                           uint64_t target_address, mca_btl_base_registration_handle_t *target_handle,
                                           ompi_osc_rdma_module_t *module)
{
    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "performing compare-and-swap with local regions");

    ompi_osc_rdma_lock_acquire_exclusive (module, peer, offsetof (ompi_osc_rdma_state_t, accumulate_lock));

    memcpy (result_buffer, (void *) (uintptr_t) target_address, datatype->super.size);

    if (0 == memcmp (compare_buffer, result_buffer, datatype->super.size)) {
        memcpy ((void *) (uintptr_t) target_address, source_buffer, datatype->super.size);
    }

    ompi_osc_rdma_lock_release_exclusive (module, peer, offsetof (ompi_osc_rdma_state_t, accumulate_lock));

    return OMPI_SUCCESS;
}

/* completion of an accumulate put */
static void ompi_osc_rdma_acc_put_complete (struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                                            void *local_address, mca_btl_base_registration_handle_t *local_handle,
                                            void *context, void *data, int status)
{
    ompi_osc_rdma_request_t *request = (ompi_osc_rdma_request_t *) context;
    ompi_osc_rdma_sync_t *sync = request->sync;
    ompi_osc_rdma_peer_t *peer = request->peer;

    OSC_RDMA_VERBOSE(status ? MCA_BASE_VERBOSE_ERROR : MCA_BASE_VERBOSE_TRACE, "remote accumulate (put/get) complete on "
                     "sync %p. local address %p. opal status %d", (void *) sync, local_address, status);

    ompi_osc_rdma_frag_complete (request->frag);
    ompi_osc_rdma_request_complete (request, status);

    if (!ompi_osc_rdma_peer_is_exclusive (peer)) {
        (void) ompi_osc_rdma_lock_release_exclusive (sync->module, peer, offsetof (ompi_osc_rdma_state_t, accumulate_lock));
    }

    ompi_osc_rdma_sync_rdma_dec (sync);
    peer->flags &= ~OMPI_OSC_RDMA_PEER_ACCUMULATING;
}

/* completion of an accumulate get operation */
static void ompi_osc_rdma_acc_get_complete (struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                                            void *local_address, mca_btl_base_registration_handle_t *local_handle,
                                            void *context, void *data, int status)
{
    ompi_osc_rdma_request_t *request = (ompi_osc_rdma_request_t *) context;
    intptr_t source = (intptr_t) local_address + request->offset;
    ompi_osc_rdma_sync_t *sync = request->sync;
    ompi_osc_rdma_module_t *module = sync->module;

    assert (OMPI_SUCCESS == status);

    OSC_RDMA_VERBOSE(status ? MCA_BASE_VERBOSE_ERROR : MCA_BASE_VERBOSE_TRACE, "remote accumulate get complete on sync %p. "
                     "status %d. request type %d", (void *) sync, status, request->type);

    if (OMPI_SUCCESS == status && OMPI_OSC_RDMA_TYPE_GET_ACC == request->type) {
        OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "unpacking get accumulate result into user buffer");
        if (NULL == request->result_addr) {
            /* result buffer is not necessarily contiguous. use the opal datatype engine to
             * copy the data over in this case */
            struct iovec iov = {.iov_base = (void *) source, request->len};
            uint32_t iov_count = 1;
            size_t size = request->len;

            opal_convertor_unpack (&request->convertor, &iov, &iov_count, &size);
            opal_convertor_cleanup (&request->convertor);
        } else {
            /* copy contiguous data to the result buffer */
            ompi_datatype_sndrcv ((void *) source, request->len, MPI_BYTE, request->result_addr,
                                  request->result_count, request->result_dt);
        }

        if (&ompi_mpi_op_no_op.op == request->op) {
            /* this is a no-op. nothing more to do except release resources and the accumulate lock */
            ompi_osc_rdma_acc_put_complete (btl, endpoint, local_address, local_handle, context, data, status);

            return;
        }
    }

    /* accumulate the data */
    if (&ompi_mpi_op_replace.op != request->op) {
        ompi_op_reduce (request->op, request->origin_addr, (void *) source, request->origin_count, request->origin_dt);
    } else {
        memcpy ((void *) source, request->origin_addr, request->len);
    }

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "putting locally accumulated result into target window");

    /* initiate the put of the accumulated data */
    status = module->selected_btl->btl_put (module->selected_btl, endpoint, (void *) source,
                                            request->target_address, local_handle,
                                            (mca_btl_base_registration_handle_t *) request->ctx,
                                            request->len, 0, MCA_BTL_NO_ORDER, ompi_osc_rdma_acc_put_complete,
                                            request, NULL);
    /* TODO -- we can do better. probably should queue up the next step and handle it in progress */
    assert (OPAL_SUCCESS == status);
}

static inline int ompi_osc_rdma_gacc_contig (ompi_osc_rdma_sync_t *sync, const void *source, int source_count, ompi_datatype_t *source_datatype,
                                             void *result, int result_count, ompi_datatype_t *result_datatype,
                                             ompi_osc_rdma_peer_t *peer, uint64_t target_address,
                                             mca_btl_base_registration_handle_t *target_handle, int target_count,
                                             ompi_datatype_t *target_datatype, ompi_op_t *op, ompi_osc_rdma_request_t *request)
{
    ompi_osc_rdma_module_t *module = sync->module;
    const size_t btl_alignment_mask = ALIGNMENT_MASK(module->selected_btl->btl_get_alignment);
    unsigned long len = target_count * target_datatype->super.size;
    ompi_osc_rdma_frag_t *frag = NULL;
    unsigned long aligned_len, offset;
    char *ptr = NULL;
    int ret;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "initiating accumulate on contiguous region of %lu bytes to remote address %" PRIx64
                     ", sync %p", len, target_address, (void *) sync);

    offset = target_address & btl_alignment_mask;;
    aligned_len = (len + offset + btl_alignment_mask) & ~btl_alignment_mask;

    ret = ompi_osc_rdma_frag_alloc (module, aligned_len, &frag, &ptr);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_WARN, "could not allocate a temporary buffer for accumulate");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    OPAL_THREAD_LOCK(&module->lock);
    /* to ensure order wait until the previous accumulate completes */
    while (ompi_osc_rdma_peer_is_accumulating (peer)) {
        OPAL_THREAD_UNLOCK(&module->lock);
        ompi_osc_rdma_progress (module);
        OPAL_THREAD_LOCK(&module->lock);
    }

    peer->flags |= OMPI_OSC_RDMA_PEER_ACCUMULATING;
    OPAL_THREAD_UNLOCK(&module->lock);

    if (!ompi_osc_rdma_peer_is_exclusive (peer)) {
        (void) ompi_osc_rdma_lock_acquire_exclusive (module, peer, offsetof (ompi_osc_rdma_state_t, accumulate_lock));
    }

    /* set up the request */
    request->frag          = frag;
    request->origin_addr   = (void *) source;
    request->origin_dt     = source_datatype;
    request->origin_count  = source_count;
    request->ctx           = (void *) target_handle;
    request->result_addr   = result;
    request->result_count  = result_count;
    request->result_dt     = result_datatype;
    request->offset        = (ptrdiff_t) target_address & btl_alignment_mask;
    request->target_address = target_address;
    request->len           = len;
    request->op            = op;
    request->sync          = sync;

    ompi_osc_rdma_sync_rdma_inc (sync);

    if (&ompi_mpi_op_replace.op != op || OMPI_OSC_RDMA_TYPE_GET_ACC == request->type) {
        /* align the target address */
        target_address = target_address & ~btl_alignment_mask;

        OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "initiating btl get. local: %p (handle %p), remote: 0x%" PRIx64
                         " (handle %p)", ptr, (void *) frag->handle, target_address, (void *) target_handle);

        ret = module->selected_btl->btl_get (module->selected_btl, peer->data_endpoint, ptr,
                                             target_address, frag->handle, target_handle, aligned_len,
                                             0, MCA_BTL_NO_ORDER, ompi_osc_rdma_acc_get_complete,
                                             request, NULL);
    } else {
        /* copy the put accumulate data */
        memcpy (ptr, source, len);

        OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "initiating btl put. local: %p (handle %p), remote: 0x%" PRIx64
                         " (handle %p)", ptr, (void *) frag->handle, target_address, (void *) target_handle);

        ret = module->selected_btl->btl_put (module->selected_btl, peer->data_endpoint, ptr,
                                             target_address, frag->handle, target_handle, len, 0,
                                             MCA_BTL_NO_ORDER, ompi_osc_rdma_acc_put_complete,
                                             request, NULL);
    }

    if (OPAL_UNLIKELY(OMPI_SUCCESS == ret)) {
        return OMPI_SUCCESS;
    }

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_INFO, "accumulate btl operation faile with opal error code %d", ret);

    ompi_osc_rdma_cleanup_rdma (sync, frag, NULL, NULL);

    return ret;
}

static inline int ompi_osc_rdma_gacc_master (ompi_osc_rdma_sync_t *sync, const void *source_buffer, int source_count,
                                             ompi_datatype_t *source_datatype, void *result_buffer, int result_count,
                                             ompi_datatype_t *result_datatype, ompi_osc_rdma_peer_t *peer, uint64_t target_address,
                                             mca_btl_base_registration_handle_t *target_handle, int target_count,
                                             ompi_datatype_t *target_datatype, ompi_op_t *op, ompi_osc_rdma_request_t *request)
{
    ompi_osc_rdma_module_t *module = sync->module;
    struct iovec source_iovec[OMPI_OSC_RDMA_DECODE_MAX], target_iovec[OMPI_OSC_RDMA_DECODE_MAX];
    const size_t acc_limit = (mca_osc_rdma_component.buffer_size >> 3);
    uint32_t source_primitive_count, target_primitive_count;
    opal_convertor_t source_convertor, target_convertor;
    uint32_t source_iov_count, target_iov_count;
    uint32_t source_iov_index, target_iov_index;
    ompi_datatype_t *source_primitive, *target_primitive;
    /* needed for opal_convertor_raw but not used */
    size_t source_size, target_size;
    ompi_osc_rdma_request_t *subreq;
    size_t result_position;
    ptrdiff_t lb, extent;
    int ret, acc_len;
    bool done;

    (void) ompi_datatype_get_extent (target_datatype, &lb, &extent);
    target_address += lb;

    /* fast path for accumulate on built-in types */
    if (OPAL_LIKELY((!source_count || ompi_datatype_is_predefined (source_datatype)) &&
                    ompi_datatype_is_predefined (target_datatype) &&
                    (!result_count || ompi_datatype_is_predefined (result_datatype)) &&
                    (target_datatype->super.size * target_count <= acc_limit))) {
        if (NULL == request) {
            OMPI_OSC_RDMA_REQUEST_ALLOC(module, peer, request);
            request->internal = true;
        }

        request->type = result_datatype ? OMPI_OSC_RDMA_TYPE_GET_ACC : OMPI_OSC_RDMA_TYPE_ACC;

        if (source_datatype) {
            (void) ompi_datatype_get_extent (source_datatype, &lb, &extent);
            source_buffer = (void *)((intptr_t) source_buffer + lb);
        }

        if (result_datatype) {
            (void) ompi_datatype_get_extent (result_datatype, &lb, &extent);
            result_buffer = (void *)((intptr_t) result_buffer + lb);
        }

        ret = ompi_osc_rdma_gacc_contig (sync, source_buffer, source_count, source_datatype, result_buffer,
                                         result_count, result_datatype, peer, target_address,
                                         target_handle, target_count, target_datatype, op,
                                         request);
        if (OPAL_LIKELY(OMPI_SUCCESS == ret)) {
            return OMPI_SUCCESS;
        }

        if (source_datatype) {
            /* the convertors will handle the lb */
            (void) ompi_datatype_get_extent (source_datatype, &lb, &extent);
            source_buffer = (void *)((intptr_t) source_buffer - lb);
        }

        if (result_datatype) {
            (void) ompi_datatype_get_extent (result_datatype, &lb, &extent);
            result_buffer = (void *)((intptr_t) result_buffer - lb);
        }
    }

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "scheduling accumulate on non-contiguous datatype(s)");

    /* the convertor will handle lb from here */
    (void) ompi_datatype_get_extent (target_datatype, &lb, &extent);
    target_address -= lb;

    /* get the primitive datatype info */
    ret = ompi_osc_base_get_primitive_type_info (target_datatype, &target_primitive, &target_primitive_count);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        /* target datatype is not made up of a single basic datatype */
        return ret;
    }

    if (source_datatype) {
        ret = ompi_osc_base_get_primitive_type_info (source_datatype, &source_primitive, &source_primitive_count);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            /* target datatype is not made up of a single basic datatype */
            return ret;
        }

        if (OPAL_UNLIKELY(source_primitive != target_primitive)) {
            return MPI_ERR_TYPE;
        }
    }

    /* prepare convertors for the source and target. these convertors will be used to determine the
     * contiguous segments within the source and target. */
    /* the source may be NULL if using MPI_OP_NO_OP with MPI_Get_accumulate */
    if (source_datatype) {
        OBJ_CONSTRUCT(&source_convertor, opal_convertor_t);
        ret = opal_convertor_copy_and_prepare_for_send (ompi_mpi_local_convertor, &source_datatype->super, source_count, source_buffer,
                                                       0, &source_convertor);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            return ret;
        }
    }

    /* target_datatype can never be NULL */
    OBJ_CONSTRUCT(&target_convertor, opal_convertor_t);
    ret = opal_convertor_copy_and_prepare_for_send (ompi_mpi_local_convertor, &target_datatype->super, target_count,
                                                    (void *) (intptr_t) target_address, 0, &target_convertor);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    if (request) {
        /* keep the request from completing until all the transfers have started */
        request->outstanding_requests = 1;
    }

    target_iov_index = 0;
    target_iov_count = 0;
    result_position = 0;

    do {
        /* decode segments of the source data */
        source_iov_count = OMPI_OSC_RDMA_DECODE_MAX;
        source_iov_index = 0;
        /* opal_convertor_raw returns done when it has reached the end of the data */
        if (!source_datatype) {
            done = true;
            source_iovec[0].iov_len = (size_t) -1;
            source_iovec[0].iov_base = NULL;
            source_iov_count = 1;
        } else {
            done = opal_convertor_raw (&source_convertor, source_iovec, &source_iov_count, &source_size);
        }

        /* loop on the target segments until we have exhaused the decoded source data */
        while (source_iov_index != source_iov_count) {
            if (target_iov_index == target_iov_count) {
                /* decode segments of the target buffer */
                target_iov_count = OMPI_OSC_RDMA_DECODE_MAX;
                target_iov_index = 0;
                (void) opal_convertor_raw (&target_convertor, target_iovec, &target_iov_count, &target_size);
            }

            /* we already checked that the target was large enough. this should be impossible */
            assert (0 != target_iov_count);

            /* determine how much to put in this operation */
            acc_len = min(target_iovec[target_iov_index].iov_len, source_iovec[source_iov_index].iov_len);
            acc_len = min((size_t) acc_len, acc_limit);

            /* execute the get */
            OMPI_OSC_RDMA_REQUEST_ALLOC(module, peer, subreq);
            subreq->internal = true;
            subreq->parent_request = request;
            if (request) {
                (void) OPAL_THREAD_ADD32 (&request->outstanding_requests, 1);
            }

            if (result_datatype) {
                /* prepare a convertor for this part of the result */
                opal_convertor_copy_and_prepare_for_recv (ompi_mpi_local_convertor, &result_datatype->super, result_count,
                                                          result_buffer, 0, &subreq->convertor);
                opal_convertor_set_position (&subreq->convertor, &result_position);
                subreq->type = OMPI_OSC_RDMA_TYPE_GET_ACC;
            } else {
                subreq->type = OMPI_OSC_RDMA_TYPE_ACC;
            }

            ret = ompi_osc_rdma_gacc_contig (sync, source_iovec[source_iov_index].iov_base, acc_len / target_primitive->super.size,
                                             target_primitive, NULL, 0, NULL, peer, (uint64_t) (intptr_t) target_iovec[target_iov_index].iov_base,
                                             target_handle, acc_len / target_primitive->super.size, target_primitive, op, subreq);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                if (OPAL_UNLIKELY(OMPI_ERR_OUT_OF_RESOURCE != ret)) {
                    /* something bad happened. need to figure out how to handle these errors */
                    return ret;
                }

                /* progress and try again */
                ompi_osc_rdma_progress (module);
                continue;
            }

            /* adjust io vectors */
            target_iovec[target_iov_index].iov_len -= acc_len;
            source_iovec[source_iov_index].iov_len -= acc_len;
            target_iovec[target_iov_index].iov_base = (void *)((intptr_t) target_iovec[target_iov_index].iov_base + acc_len);
            source_iovec[source_iov_index].iov_base = (void *)((intptr_t) source_iovec[source_iov_index].iov_base + acc_len);
            result_position += acc_len;

            source_iov_index += !source_datatype || (0 == source_iovec[source_iov_index].iov_len);
            target_iov_index += (0 == target_iovec[target_iov_index].iov_len);
        }
    } while (!done);

    if (request) {
        /* release our reference so the request can complete */
        (void) OPAL_THREAD_ADD32 (&request->outstanding_requests, -1);
    }

    if (source_datatype) {
        opal_convertor_cleanup (&source_convertor);
        OBJ_DESTRUCT(&source_convertor);
    }

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "finished scheduling rdma on non-contiguous datatype(s)");

    opal_convertor_cleanup (&target_convertor);
    OBJ_DESTRUCT(&target_convertor);

    return OMPI_SUCCESS;
}

#if 0
static void ompi_osc_rdma_cas_atomic_complete (struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                                               void *local_address, mca_btl_base_registration_handle_t *local_handle,
                                               void *context, void *data, int status)
{
    ompi_osc_rdma_sync_t *sync = (ompi_osc_rdma_sync_t *) context;
    ompi_osc_rdma_frag_t *frag = (ompi_osc_rdma_frag_t *) data;
    void *result_buffer = (void *)(intptr_t) ((int64_t *) local_address)[1];

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "atomic compare-and-swap complete. result: 0x%" PRIx64,
                     *((int64_t *) local_address));

    /* copy the result */
    memcpy (result_buffer, local_address, 8);

    ompi_osc_rdma_sync_rdma_dec (sync);
    ompi_osc_rdma_frag_complete (frag);
}

static inline int ompi_osc_rdma_cas_atomic (ompi_osc_rdma_sync_t *sync, const void *source_buffer, const void *compare_buffer,
                                            void *result_buffer, ompi_datatype_t *datatype, ompi_osc_rdma_peer_t *peer,
                                            uint64_t target_address, mca_btl_base_registration_handle_t *target_handle)
{
    ompi_osc_rdma_module_t *module = sync->module;
    ompi_osc_rdma_frag_t *frag = NULL;
    char *ptr;
    int ret;

    /* XXX -- TODO -- Update the BTL interface to allow for other CAS sizes */
    if (datatype->super.size != 8) {
        return OMPI_ERR_NOT_SUPPORTED;
    }

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "initiating compare-and-swap using 64-bit btl atomics. compare: 0x%"
                     PRIx64 ", origin: 0x%" PRIx64, *((int64_t *) compare_buffer), *((int64_t *) source_buffer));

    ret = ompi_osc_rdma_frag_alloc (module, 16, &frag, &ptr);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    /* store the destination in the temporary buffer */
    ((int64_t *) ptr)[1] = (intptr_t) result_buffer;

    ret = module->selected_btl->btl_atomic_cswap (module->selected_btl, peer->data_endpoint, ptr, target_address,
                                                  frag->handle, target_handle, ((int64_t *)compare_buffer)[0],
                                                  *((int64_t *) source_buffer), 0, MCA_BTL_NO_ORDER,
                                                  ompi_osc_rdma_cas_atomic_complete, module, frag);
    if (OPAL_UNLIKELY(0 > ret)) {
        return ret;
    }

    if (1 != ret) {
        ompi_osc_rdma_sync_rdma_inc (sync);
    } else {
        memcpy (result_buffer, ptr, 8);

        ompi_osc_rdma_frag_complete (frag);
    }

    return OMPI_SUCCESS;
}
#endif

/**
 * ompi_osc_rdma_cas_get_complete:
 * Note: This function will not work as is in a heterogeneous environment.
 */
static void ompi_osc_rdma_cas_get_complete (struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                                            void *local_address, mca_btl_base_registration_handle_t *local_handle,
                                            void *context, void *data, int status)
{
    ompi_osc_rdma_request_t *request = (ompi_osc_rdma_request_t *) context;
    ompi_osc_rdma_sync_t *sync = request->sync;
    ompi_osc_rdma_module_t *module = sync->module;
    intptr_t source = (intptr_t) local_address + request->offset;
    ompi_osc_rdma_frag_t *frag = request->frag;
    ompi_osc_rdma_peer_t *peer = request->peer;
    int ret;

    OSC_RDMA_VERBOSE(status ? MCA_BASE_VERBOSE_ERROR : MCA_BASE_VERBOSE_TRACE, "remote compare-and-swap get complete on sync %p. "
                     "status %d", (void *) sync, status);

    if (OMPI_SUCCESS == status) {
        /* copy data to the user buffer (for gacc) */
        memcpy (request->result_addr, (void *) source, request->len);

        if (0 == memcmp ((void *) source, request->compare_addr, request->len)) {
            /* the target and compare buffers match so write the source to the target */
            memcpy ((void *) source, request->origin_addr, request->len);

            ret = module->selected_btl->btl_put (module->selected_btl, peer->data_endpoint, local_address,
                                                 request->target_address, local_handle,
                                                 (mca_btl_base_registration_handle_t *) request->ctx,
                                                 request->len, 0, MCA_BTL_NO_ORDER,
                                                 ompi_osc_rdma_acc_put_complete, request, NULL);
            if (OPAL_UNLIKELY(OPAL_SUCCESS != ret)) {
                OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_ERROR, "could not start put to complete accumulate operation. opal return code "
                                 "%d", ret);
            }

            /* TODO -- we can do better. probably should queue up the next step and handle it in progress */
            assert (OPAL_SUCCESS == ret);
        } else {
            /* this is a no-op. nothing more to do except release the accumulate lock */
            ompi_osc_rdma_frag_complete (frag);

            if (!ompi_osc_rdma_peer_is_exclusive (peer)) {
                (void) ompi_osc_rdma_lock_release_exclusive (module, request->peer,
                                                             offsetof (ompi_osc_rdma_state_t, accumulate_lock));
            }

            /* the request is now complete and the outstanding rdma operation is complete */
            ompi_osc_rdma_request_complete (request, status);

            ompi_osc_rdma_sync_rdma_dec (sync);
            peer->flags &= ~OMPI_OSC_RDMA_PEER_ACCUMULATING;
        }
    }
}

static inline int cas_rdma (ompi_osc_rdma_sync_t *sync, const void *source_buffer, const void *compare_buffer, void *result_buffer,
                            ompi_datatype_t *datatype, ompi_osc_rdma_peer_t *peer, uint64_t target_address,
                            mca_btl_base_registration_handle_t *target_handle)
{
    ompi_osc_rdma_module_t *module = sync->module;
    const size_t btl_alignment_mask = ALIGNMENT_MASK(module->selected_btl->btl_get_alignment);
    unsigned long offset, aligned_len, len = datatype->super.size;
    ompi_osc_rdma_frag_t *frag = NULL;
    ompi_osc_rdma_request_t *request;
    char *ptr = NULL;
    int ret;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "initiating compare-and-swap using RMDA on %lu bytes to remote address %" PRIx64
                     ", sync %p", len, target_address, (void *) sync);

    OMPI_OSC_RDMA_REQUEST_ALLOC(module, peer, request);

    request->internal = true;
    request->type = OMPI_OSC_RDMA_TYPE_CSWAP;
    request->sync = sync;

    OPAL_THREAD_LOCK(&module->lock);
    /* to ensure order wait until the previous accumulate completes */
    while (ompi_osc_rdma_peer_is_accumulating (peer)) {
        OPAL_THREAD_UNLOCK(&module->lock);
        ompi_osc_rdma_progress (module);
        OPAL_THREAD_LOCK(&module->lock);
    }
    peer->flags |= OMPI_OSC_RDMA_PEER_ACCUMULATING;
    OPAL_THREAD_UNLOCK(&module->lock);

    offset = target_address & btl_alignment_mask;;
    aligned_len = (len + offset + btl_alignment_mask) & ~btl_alignment_mask;

    do {
        ret = ompi_osc_rdma_frag_alloc (module, aligned_len, &frag, &ptr);
        if (OPAL_UNLIKELY(OMPI_SUCCESS == ret)) {
            break;
        }

        OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_WARN, "could not allocate an rdma fragment for compare-and-swap");
        ompi_osc_rdma_progress (module);
    } while (1);

    if (!ompi_osc_rdma_peer_is_exclusive (peer)) {
        (void) ompi_osc_rdma_lock_acquire_exclusive (module, peer, offsetof (ompi_osc_rdma_state_t, accumulate_lock));
    }

    /* set up the request */
    request->frag         = frag;
    request->origin_addr  = (void *) source_buffer;
    request->ctx          = (void *) target_handle;
    request->result_addr  = result_buffer;
    request->compare_addr = compare_buffer;
    request->result_dt    = datatype;
    request->offset       = (ptrdiff_t) offset;
    request->target_address = target_address;
    request->len          = len;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "RDMA compare-and-swap initiating btl get");

    do {
        ret = module->selected_btl->btl_get (module->selected_btl, peer->data_endpoint, ptr,
                                             target_address, frag->handle, target_handle,
                                             aligned_len, 0, MCA_BTL_NO_ORDER,
                                             ompi_osc_rdma_cas_get_complete, request, NULL);
        if (OPAL_LIKELY(OPAL_SUCCESS == ret)) {
            break;
        }

        if (OPAL_UNLIKELY(OPAL_ERR_OUT_OF_RESOURCE != ret && OPAL_ERR_TEMP_OUT_OF_RESOURCE != ret)) {
            ompi_osc_rdma_frag_complete (frag);
            return ret;
        }

        ompi_osc_rdma_progress (module);
    } while (1);

    ompi_osc_rdma_sync_rdma_inc (sync);

    return OMPI_SUCCESS;
}


int ompi_osc_rdma_compare_and_swap (const void *origin_addr, const void *compare_addr, void *result_addr,
                                    struct ompi_datatype_t *dt, int target_rank, OPAL_PTRDIFF_TYPE target_disp,
                                    struct ompi_win_t *win)
{
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    ompi_osc_rdma_peer_t *peer;
    mca_btl_base_registration_handle_t *target_handle;
    ompi_osc_rdma_sync_t *sync;
    uint64_t target_address;
    int ret;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "cswap: 0x%lx, 0x%lx, 0x%lx, %s, %d, %d, %s",
                     (unsigned long) origin_addr, (unsigned long) compare_addr, (unsigned long) result_addr,
                     dt->name, target_rank, (int) target_disp, win->w_name);

    sync = ompi_osc_rdma_module_sync_lookup (module, target_rank, &peer);
    if (OPAL_UNLIKELY(NULL == sync)) {
        return OMPI_ERR_RMA_SYNC;
    }

    ret = osc_rdma_get_remote_segment (module, peer, target_disp, dt->super.size, &target_address, &target_handle);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != ret)) {
        return ret;
    }

#if 0
    if (MCA_OSC_RDMA_SAME_OP <= module->accumulate_ops) {
        /* the user has indicated that they will only use the same op (or same op and no op)
         * for operations on overlapping memory ranges. that indicates it is safe to go ahead
         * and use network atomic operations. */
        ret = ompi_osc_rdma_cas_atomic (sync, origin_addr, compare_addr, result_addr, dt,
                                        peer, target_address, target_handle);
        if (OMPI_SUCCESS == ret) {
            return OMPI_SUCCESS;
        }
    } else
#endif

    if (ompi_osc_rdma_peer_local_base (peer)) {
        return ompi_osc_rdma_cas_local (origin_addr, compare_addr, result_addr, dt,
                                        peer, target_address, target_handle, module);
    }

    return cas_rdma (sync, origin_addr, compare_addr, result_addr, dt, peer, target_address,
                     target_handle);
}


static inline
int ompi_osc_rdma_rget_accumulate_internal (ompi_osc_rdma_sync_t *sync, const void *origin_addr, int origin_count,
                                            struct ompi_datatype_t *origin_datatype, void *result_addr, int result_count,
                                            struct ompi_datatype_t *result_datatype, ompi_osc_rdma_peer_t *peer,
                                            int target_rank, MPI_Aint target_disp, int target_count,
                                            struct ompi_datatype_t *target_datatype, struct ompi_op_t *op,
                                            ompi_osc_rdma_request_t *request)
{
    ompi_osc_rdma_module_t *module = sync->module;
    mca_btl_base_registration_handle_t *target_handle;
    uint64_t target_address;
    int ret;

    /* short-circuit case. note that origin_count may be 0 if op is MPI_NO_OP */
    if ((result_addr && 0 == result_count) || 0 == target_count) {
        if (request) {
            ompi_osc_rdma_request_complete (request, MPI_SUCCESS);
        }

        return OMPI_SUCCESS;
    }

    /* TODO: Remove the following check when support is added.
     * See the following issue for the current state:
     *   https://github.com/open-mpi/ompi/issues/1666
     */
    if(MPI_MINLOC == op || MPI_MAXLOC == op) {
        if(MPI_SHORT_INT == origin_datatype ||
           MPI_DOUBLE_INT == origin_datatype ||
           MPI_LONG_INT == origin_datatype ||
           MPI_LONG_DOUBLE_INT == origin_datatype) {
           ompi_communicator_t *comm = &ompi_mpi_comm_world.comm;
           opal_show_help("help-mca-osc-base.txt", "unsupported-dt", true,
                          origin_datatype->name,
                          op->o_name,
                          comm->c_my_rank);
           ompi_mpi_abort(comm, -1);
        }
        if(MPI_SHORT_INT == result_datatype ||
           MPI_DOUBLE_INT == result_datatype ||
           MPI_LONG_INT == result_datatype ||
           MPI_LONG_DOUBLE_INT == result_datatype) {
           ompi_communicator_t *comm = &ompi_mpi_comm_world.comm;
           opal_show_help("help-mca-osc-base.txt", "unsupported-dt", true,
                          result_datatype->name,
                          op->o_name,
                          comm->c_my_rank);
           ompi_mpi_abort(comm, -1);
        }
        if(MPI_SHORT_INT == target_datatype ||
           MPI_DOUBLE_INT == target_datatype ||
           MPI_LONG_INT == target_datatype ||
           MPI_LONG_DOUBLE_INT == target_datatype) {
           ompi_communicator_t *comm = &ompi_mpi_comm_world.comm;
           opal_show_help("help-mca-osc-base.txt", "unsupported-dt", true,
                          target_datatype->name,
                          op->o_name,
                          comm->c_my_rank);
           ompi_mpi_abort(comm, -1);
        }
    }

    ret = osc_rdma_get_remote_segment (module, peer, target_disp, target_datatype->super.size * target_count,
                                       &target_address, &target_handle);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    if (ompi_osc_rdma_peer_local_base (peer)) {
        /* local/self optimization */
        return ompi_osc_rdma_gacc_local (origin_addr, origin_count, origin_datatype, result_addr, result_count,
                                         result_datatype, peer, target_address, target_handle, target_count,
                                         target_datatype, op, module, request);
    }

    return ompi_osc_rdma_gacc_master (sync, origin_addr, origin_count, origin_datatype, result_addr, result_count,
                                      result_datatype, peer, target_address, target_handle, target_count,
                                      target_datatype, op, request);
}

int ompi_osc_rdma_get_accumulate (const void *origin_addr, int origin_count,
                                  struct ompi_datatype_t *origin_datatype,
                                  void *result_addr, int result_count,
                                  struct ompi_datatype_t *result_datatype,
                                  int target_rank, MPI_Aint target_disp,
                                  int target_count, struct ompi_datatype_t *target_datatype,
                                  struct ompi_op_t *op, struct ompi_win_t *win)
{
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    ompi_osc_rdma_peer_t *peer;
    ompi_osc_rdma_sync_t *sync;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "get_acc: 0x%lx, %d, %s, 0x%lx, %d, %s, %d, 0x%lx, %d, %s, %s, %s",
                     (unsigned long) origin_addr, origin_count, origin_datatype->name,
                     (unsigned long) result_addr, result_count, result_datatype->name, target_rank,
                     (unsigned long) target_disp, target_count, target_datatype->name, op->o_name,
                     win->w_name);

    sync = ompi_osc_rdma_module_sync_lookup (module, target_rank, &peer);
    if (OPAL_UNLIKELY(NULL == sync)) {
        return OMPI_ERR_RMA_SYNC;
    }

    return ompi_osc_rdma_rget_accumulate_internal (sync, origin_addr, origin_count, origin_datatype,
                                                   result_addr, result_count, result_datatype,
                                                   peer, target_rank, target_disp, target_count,
                                                   target_datatype, op, NULL);
}


int ompi_osc_rdma_rget_accumulate (const void *origin_addr, int origin_count,
                                   struct ompi_datatype_t *origin_datatype,
                                   void *result_addr, int result_count,
                                   struct ompi_datatype_t *result_datatype,
                                   int target_rank, MPI_Aint target_disp,
                                   int target_count, struct ompi_datatype_t *target_datatype,
                                   struct ompi_op_t *op, struct ompi_win_t *win,
                                   ompi_request_t **request)
{
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    ompi_osc_rdma_peer_t *peer;
    ompi_osc_rdma_request_t *rdma_request;
    ompi_osc_rdma_sync_t *sync;
    int ret;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "rget_acc: 0x%lx, %d, %s, 0x%lx, %d, %s, %d, 0x%lx, %d, %s, %s, %s",
                     (unsigned long) origin_addr, origin_count, origin_datatype->name,
                     (unsigned long) result_addr, result_count, result_datatype->name, target_rank,
                     (unsigned long) target_disp, target_count, target_datatype->name, op->o_name,
                     win->w_name);

    sync = ompi_osc_rdma_module_sync_lookup (module, target_rank, &peer);
    if (OPAL_UNLIKELY(NULL == sync)) {
        return OMPI_ERR_RMA_SYNC;
    }

    OMPI_OSC_RDMA_REQUEST_ALLOC(module, peer, rdma_request);

    ret = ompi_osc_rdma_rget_accumulate_internal (sync, origin_addr, origin_count, origin_datatype, result_addr,
                                                  result_count, result_datatype, peer, target_rank, target_disp,
                                                  target_count, target_datatype, op, rdma_request);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        OMPI_OSC_RDMA_REQUEST_RETURN(rdma_request);
        return ret;
    }

    *request = &rdma_request->super;

    return OMPI_SUCCESS;
}

int ompi_osc_rdma_fetch_and_op (const void *origin_addr, void *result_addr, struct ompi_datatype_t *dt, int target_rank,
                                OPAL_PTRDIFF_TYPE target_disp, struct ompi_op_t *op, struct ompi_win_t *win)
{
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    ompi_osc_rdma_peer_t *peer;
    ompi_osc_rdma_sync_t *sync;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "fop: %p, %s, %d, %lu, %s, %s", result_addr, dt->name, target_rank,
                     (unsigned long) target_disp, op->o_name, win->w_name);

    sync = ompi_osc_rdma_module_sync_lookup (module, target_rank, &peer);
    if (OPAL_UNLIKELY(NULL == sync)) {
        return OMPI_ERR_RMA_SYNC;
    }

    return ompi_osc_rdma_rget_accumulate_internal (sync, origin_addr, 1, dt, result_addr, 1, dt, peer, target_rank,
                                                   target_disp, 1, dt, op, NULL);
}


int ompi_osc_rdma_raccumulate (const void *origin_addr, int origin_count,
                               struct ompi_datatype_t *origin_datatype, int target_rank,
                               OPAL_PTRDIFF_TYPE target_disp, int target_count,
                               struct ompi_datatype_t *target_datatype, struct ompi_op_t *op,
                               struct ompi_win_t *win, struct ompi_request_t **request)
{
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    ompi_osc_rdma_peer_t *peer;
    ompi_osc_rdma_request_t *rdma_request;
    ompi_osc_rdma_sync_t *sync;
    int ret;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "racc: 0x%lx, %d, %s, %d, 0x%lx, %d, %s, %s, %s",
                     (unsigned long) origin_addr, origin_count, origin_datatype->name, target_rank,
                     (unsigned long) target_disp, target_count, target_datatype->name, op->o_name, win->w_name);

    sync = ompi_osc_rdma_module_sync_lookup (module, target_rank, &peer);
    if (OPAL_UNLIKELY(NULL == sync)) {
        return OMPI_ERR_RMA_SYNC;
    }

    OMPI_OSC_RDMA_REQUEST_ALLOC(module, peer, rdma_request);

    ret = ompi_osc_rdma_rget_accumulate_internal (sync, origin_addr, origin_count, origin_datatype, NULL, 0,
                                                  NULL, peer, target_rank, target_disp, target_count, target_datatype,
                                                  op, rdma_request);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        OMPI_OSC_RDMA_REQUEST_RETURN(rdma_request);
        return ret;
    }

    *request = &rdma_request->super;

    return OMPI_SUCCESS;
}

int ompi_osc_rdma_accumulate (const void *origin_addr, int origin_count,
                              struct ompi_datatype_t *origin_datatype, int target_rank,
                              OPAL_PTRDIFF_TYPE target_disp, int target_count,
                              struct ompi_datatype_t *target_datatype, struct ompi_op_t *op,
                              struct ompi_win_t *win)
{
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    ompi_osc_rdma_peer_t *peer;
    ompi_osc_rdma_sync_t *sync;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "acc: 0x%lx, %d, %s, %d, 0x%lx, %d, %s, %s, %s",
                     (unsigned long) origin_addr, origin_count, origin_datatype->name, target_rank,
                     (unsigned long) target_disp, target_count, target_datatype->name, op->o_name, win->w_name);

    sync = ompi_osc_rdma_module_sync_lookup (module, target_rank, &peer);
    if (OPAL_UNLIKELY(NULL == sync)) {
        return OMPI_ERR_RMA_SYNC;
    }

    return ompi_osc_rdma_rget_accumulate_internal (sync, origin_addr, origin_count, origin_datatype, NULL, 0,
                                                   NULL, peer, target_rank, target_disp, target_count, target_datatype,
                                                   op, NULL);
}
