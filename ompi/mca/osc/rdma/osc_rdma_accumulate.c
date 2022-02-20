/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2018 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2016-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2016-2018 Intel, Inc. All rights reserved.
 * Copyright (c) 2019      Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2019-2021 Google, LLC. All rights reserved.
 * Copyright (c) 2021      IBM Corporation.  All rights reserved.
 * Copyright (c) 2022      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "osc_rdma_accumulate.h"
#include "osc_rdma_request.h"
#include "osc_rdma_comm.h"
#include "osc_rdma_lock.h"
#include "osc_rdma_btl_comm.h"

#include "opal/util/minmax.h"
#include "ompi/mca/osc/base/base.h"
#include "ompi/mca/osc/base/osc_base_obj_convert.h"

static inline void ompi_osc_rdma_peer_accumulate_cleanup (ompi_osc_rdma_module_t *module, ompi_osc_rdma_peer_t *peer, bool lock_acquired)
{
    if (lock_acquired) {
        (void) ompi_osc_rdma_lock_release_exclusive (module, peer, offsetof (ompi_osc_rdma_state_t, accumulate_lock));
    }

    /* clear out the accumulation flag */
    ompi_osc_rdma_peer_clear_flag (peer, OMPI_OSC_RDMA_PEER_ACCUMULATING);
}

enum ompi_osc_rdma_event_type_t {
    OMPI_OSC_RDMA_EVENT_TYPE_PUT,
};

typedef enum ompi_osc_rdma_event_type_t ompi_osc_rdma_event_type_t;

struct ompi_osc_rdma_event_t {
    opal_event_t super;
    ompi_osc_rdma_module_t *module;
    struct mca_btl_base_endpoint_t *endpoint;
    void *local_address;
    mca_btl_base_registration_handle_t *local_handle;
    uint64_t remote_address;
    mca_btl_base_registration_handle_t *remote_handle;
    uint64_t length;
    mca_btl_base_rdma_completion_fn_t cbfunc;
    void *cbcontext;
    void *cbdata;
};

typedef struct ompi_osc_rdma_event_t ompi_osc_rdma_event_t;

static int ompi_osc_rdma_gacc_local (const void *source_buffer, int source_count, ompi_datatype_t *source_datatype,
                                     void *result_buffer, int result_count, ompi_datatype_t *result_datatype,
                                     ompi_osc_rdma_peer_t *peer, uint64_t target_address,
                                     mca_btl_base_registration_handle_t *target_handle, int target_count,
                                     ompi_datatype_t *target_datatype, ompi_op_t *op, ompi_osc_rdma_module_t *module,
                                     ompi_osc_rdma_request_t *request, bool lock_acquired)
{
    int ret = OMPI_SUCCESS;

    do {
        OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "performing accumulate with local region(s)");

        if (NULL != result_datatype) {
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
    } while (0);

    ompi_osc_rdma_peer_accumulate_cleanup (module, peer, lock_acquired);

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

static inline int ompi_osc_rdma_cas_local (const void *source_addr, const void *compare_addr, void *result_addr,
                                           ompi_datatype_t *datatype, ompi_osc_rdma_peer_t *peer,
                                           uint64_t target_address, mca_btl_base_registration_handle_t *target_handle,
                                           ompi_osc_rdma_module_t *module, bool lock_acquired)
{
    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "performing compare-and-swap with local regions");

    memcpy (result_addr, (void *) (uintptr_t) target_address, datatype->super.size);

    if (0 == memcmp (compare_addr, result_addr, datatype->super.size)) {
        memcpy ((void *) (uintptr_t) target_address, source_addr, datatype->super.size);
    }

    ompi_osc_rdma_peer_accumulate_cleanup (module, peer, lock_acquired);

    return OMPI_SUCCESS;
}

static int ompi_osc_rdma_op_mapping[OMPI_OP_NUM_OF_TYPES + 1] = {
    [OMPI_OP_MAX] = MCA_BTL_ATOMIC_MAX,
    [OMPI_OP_MIN] = MCA_BTL_ATOMIC_MIN,
    [OMPI_OP_SUM] = MCA_BTL_ATOMIC_ADD,
    [OMPI_OP_BAND] = MCA_BTL_ATOMIC_AND,
    [OMPI_OP_BOR] = MCA_BTL_ATOMIC_OR,
    [OMPI_OP_BXOR] = MCA_BTL_ATOMIC_XOR,
    [OMPI_OP_LAND] = MCA_BTL_ATOMIC_LAND,
    [OMPI_OP_LOR] = MCA_BTL_ATOMIC_LOR,
    [OMPI_OP_LXOR] = MCA_BTL_ATOMIC_LXOR,
    [OMPI_OP_REPLACE] = MCA_BTL_ATOMIC_SWAP,
};

/* set the appropriate flags for this atomic */
static inline int ompi_osc_rdma_set_btl_flags(ompi_osc_rdma_module_t *module, ompi_datatype_t *dt, ptrdiff_t extent) {

    int flags = 0;

    if(4 == extent) {
        flags = MCA_BTL_ATOMIC_FLAG_32BIT;
    }

    if (OMPI_DATATYPE_FLAG_DATA_FLOAT & dt->super.flags) {
        flags |= MCA_BTL_ATOMIC_FLAG_FLOAT;
    }

    return flags;
}

static int ompi_osc_rdma_fetch_and_op_atomic (ompi_osc_rdma_sync_t *sync, const void *origin_addr, void *result_addr, ompi_datatype_t *dt,
                                              ptrdiff_t extent, ompi_osc_rdma_peer_t *peer, uint64_t target_address,
                                              mca_btl_base_registration_handle_t *target_handle, ompi_op_t *op, ompi_osc_rdma_request_t *req)
{
    ompi_osc_rdma_module_t *module = sync->module;
    int btl_op, flags;
    int64_t origin;

    if ((8 != extent && !((MCA_BTL_ATOMIC_SUPPORTS_32BIT & module->atomic_flags) && 4 == extent)) ||
        (!(OMPI_DATATYPE_FLAG_DATA_INT & dt->super.flags) && !(MCA_BTL_ATOMIC_SUPPORTS_FLOAT & module->atomic_flags)) ||
        !ompi_op_is_intrinsic (op) || (0 == ompi_osc_rdma_op_mapping[op->op_type])) {
        return OMPI_ERR_NOT_SUPPORTED;
    }

    btl_op = ompi_osc_rdma_op_mapping[op->op_type];

    flags = ompi_osc_rdma_set_btl_flags(module, dt, extent);

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "initiating fetch-and-op using %d-bit btl atomics. origin: 0x%" PRIx64,
                     (4 == extent) ? 32 : 64, *((int64_t *) origin_addr));

    origin = (8 == extent) ? ((int64_t *) origin_addr)[0] : ((int32_t *) origin_addr)[0];

    return ompi_osc_rdma_btl_fop (module, peer->data_btl_index, peer->data_endpoint, target_address, target_handle, btl_op, origin, flags,
                                 result_addr, true, NULL, NULL, NULL);
}

static int ompi_osc_rdma_fetch_and_op_cas (ompi_osc_rdma_sync_t *sync, const void *origin_addr, void *result_addr, ompi_datatype_t *dt,
                                           ptrdiff_t extent, ompi_osc_rdma_peer_t *peer, uint64_t target_address,
                                           mca_btl_base_registration_handle_t *target_handle, ompi_op_t *op, ompi_osc_rdma_request_t *req)
{
    ompi_osc_rdma_module_t *module = sync->module;
    uint64_t address, offset, new_value, old_value;
    int ret;

    if (extent > 8) {
        return OMPI_ERR_NOT_SUPPORTED;
    }

    /* align the address. the user should not call with an unaligned address so don't need to range check here */
    address = target_address & ~7;
    offset = target_address & ~address;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "initiating fetch-and-op using compare-and-swap");

    ret = ompi_osc_get_data_blocking (module, peer->data_btl_index, peer->data_endpoint, address, target_handle, &old_value, 8);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    /* store the destination in the temporary buffer */
    do {
        new_value = old_value;

        if (&ompi_mpi_op_replace.op == op) {
            memcpy ((void *)((ptrdiff_t) &new_value + offset), (void *)((ptrdiff_t) origin_addr + dt->super.true_lb), extent);
        } else if (&ompi_mpi_op_no_op.op != op) {
            ompi_op_reduce (op, (void *) ((ptrdiff_t) origin_addr + dt->super.true_lb), (void*)((ptrdiff_t) &new_value + offset), 1, dt);
        }

        ret = ompi_osc_rdma_btl_cswap (module, peer->data_btl_index, peer->data_endpoint, address, target_handle,
                                       old_value, new_value, 0, (int64_t*)&new_value);
        if (OPAL_SUCCESS != ret || new_value == old_value) {
            break;
        }

        old_value = new_value;
    } while (1);

    if (result_addr) {
        memcpy (result_addr, (void *)((intptr_t) &new_value + offset), extent);
    }

    return ret;
}

static int ompi_osc_rdma_acc_single_atomic (ompi_osc_rdma_sync_t *sync, const void *origin_addr, ompi_datatype_t *dt, ptrdiff_t extent,
                                            ompi_osc_rdma_peer_t *peer, uint64_t target_address,  mca_btl_base_registration_handle_t *target_handle,
                                            ompi_op_t *op, ompi_osc_rdma_request_t *req)
{
    ompi_osc_rdma_module_t *module = sync->module;
    int btl_op, flags;
    int64_t origin;

    if ((8 != extent && !((MCA_BTL_ATOMIC_SUPPORTS_32BIT & module->atomic_flags) && 4 == extent)) ||
        (!(OMPI_DATATYPE_FLAG_DATA_INT & dt->super.flags) && !(MCA_BTL_ATOMIC_SUPPORTS_FLOAT & module->atomic_flags)) ||
        !ompi_op_is_intrinsic (op) || (0 == ompi_osc_rdma_op_mapping[op->op_type])) {
        return OMPI_ERR_NOT_SUPPORTED;
    }

    origin = (8 == extent) ? ((uint64_t *) origin_addr)[0] : ((uint32_t *) origin_addr)[0];

    /* set the appropriate flags for this atomic */
    flags = ompi_osc_rdma_set_btl_flags(module, dt, extent);

    btl_op = ompi_osc_rdma_op_mapping[op->op_type];

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "initiating accumulate using 64-bit btl atomics. origin: 0x%" PRIx64,
                     *((int64_t *) origin_addr));

    /* if we locked the peer its best to wait for completion before returning */
    return ompi_osc_rdma_btl_op (module, peer->data_btl_index, peer->data_endpoint, target_address, target_handle, btl_op, origin,
                                flags, true, NULL, NULL, NULL);
}

static inline int ompi_osc_rdma_gacc_amo (ompi_osc_rdma_module_t *module, ompi_osc_rdma_sync_t *sync, const void *source, void *result,
                                          int result_count, ompi_datatype_t *result_datatype, opal_convertor_t *result_convertor,
                                          ompi_osc_rdma_peer_t *peer, uint64_t target_address,
                                          mca_btl_base_registration_handle_t *target_handle, int count,
                                          ompi_datatype_t *datatype, ompi_op_t *op, ompi_osc_rdma_request_t *request)
{
    const bool use_amo = module->acc_use_amo;
    const size_t dt_size = datatype->super.size;
    void *to_free = NULL;
    int ret;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "using network atomics for accumulate operation with count %d", count);

    if (NULL == result) {
        to_free = result = malloc (request->len);
        if (OPAL_UNLIKELY(NULL == result)) {
            return OMPI_ERR_OUT_OF_RESOURCE;
       }
    }

    for (int i = 0 ; i < count ; ) {
        if (use_amo) {
            if (NULL == result) {
                ret = ompi_osc_rdma_acc_single_atomic (sync, source, datatype, dt_size, peer, target_address, target_handle, op, request);
            } else {
                ret = ompi_osc_rdma_fetch_and_op_atomic (sync, source, result, datatype, dt_size, peer, target_address, target_handle, op,
                                                         request);
            }
        } else {
            ret = ompi_osc_rdma_fetch_and_op_cas (sync, source, result, datatype, dt_size, peer, target_address, target_handle, op,
                                                  request);
        }

        if (OPAL_LIKELY(OMPI_SUCCESS == ret)) {
            if (source) {
                source = (const void *) ((intptr_t) source + dt_size);
            }
            if (result) {
                result = (void *) ((intptr_t) result + dt_size);
            }
            target_address += dt_size;
            ++i;
        } else if (OPAL_UNLIKELY(OMPI_ERR_NOT_SUPPORTED == ret)) {
            return OMPI_ERR_NOT_SUPPORTED;
        }
    }

    if (NULL != result_convertor) {
        /* result buffer is not necessarily contiguous. use the opal datatype engine to
         * copy the data over in this case */
        struct iovec iov = {.iov_base = result, .iov_len = request->len};
        uint32_t iov_count = 1;
        size_t size = request->len;

        opal_convertor_unpack (result_convertor, &iov, &iov_count, &size);
    }

    if (request) {
        ompi_osc_rdma_request_complete (request, MPI_SUCCESS);
    }

    free (to_free);

    return OMPI_SUCCESS;
}

static inline int ompi_osc_rdma_gacc_contig (ompi_osc_rdma_sync_t *sync, const void *source, int source_count,
                                             ompi_datatype_t *source_datatype, void *result, int result_count,
                                             ompi_datatype_t *result_datatype, opal_convertor_t *result_convertor,
                                             ompi_osc_rdma_peer_t *peer, uint64_t target_address,
                                             mca_btl_base_registration_handle_t *target_handle, int target_count,
                                             ompi_datatype_t *target_datatype, ompi_op_t *op, ompi_osc_rdma_request_t *request)
{
    ompi_osc_rdma_module_t *module = sync->module;
    size_t target_dtype_size = target_datatype->super.size;
    unsigned long len = target_count * target_dtype_size;
    char *ptr = NULL;
    int ret;

    request->len = target_dtype_size * module->network_amo_max_count;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "initiating accumulate on contiguous region of %lu bytes to remote address %" PRIx64
                     ", sync %p", len, target_address, (void *) sync);

    /* if the datatype is small enough (and the count is 1) then try to directly use the hardware to execute
     * the atomic operation. this should be safe in all cases as either 1) the user has assured us they will
     * never use atomics with count > 1, 2) we have the accumulate lock, or 3) we have an exclusive lock */
    if ((target_dtype_size <= 8) && (((unsigned long) target_count) <= module->network_amo_max_count) &&
         ompi_osc_base_is_atomic_size_supported(target_address, target_dtype_size)) {
        ret = ompi_osc_rdma_gacc_amo (module, sync, source, result, result_count, result_datatype, result_convertor,
                                      peer, target_address, target_handle, target_count, target_datatype, op, request);
        if (OPAL_LIKELY(OMPI_SUCCESS == ret)) {
            return OMPI_SUCCESS;
        }
        OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "network atomics not available. falling back to get-op-put implementation...");
    }

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "using get-op-put to execute accumulate with count %d", target_count);

    if (&ompi_mpi_op_replace.op != op || OMPI_OSC_RDMA_TYPE_GET_ACC == request->type) {
        ptr = malloc (len);
        if (OPAL_UNLIKELY(NULL == ptr)) {
            OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_WARN, "could not allocate a temporary buffer for accumulate");
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        /* set up the request */
        request->to_free = ptr;

        ret = ompi_osc_get_data_blocking (module, peer->data_btl_index, peer->data_endpoint,
                                          target_address, target_handle, ptr, len);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            return ret;
        }

        if (OMPI_OSC_RDMA_TYPE_GET_ACC == request->type) {
            if (NULL == result) {
                /* result buffer is not necessarily contiguous. use the opal datatype engine to
                 * copy the data over in this case */
                struct iovec iov = {.iov_base = ptr, len};
                uint32_t iov_count = 1;
                size_t size = request->len;

                opal_convertor_unpack (result_convertor, &iov, &iov_count, &size);
            } else {
                /* copy contiguous data to the result buffer */
                ompi_datatype_sndrcv (ptr, len, MPI_BYTE, result, result_count, result_datatype);
            }
        }

        if (&ompi_mpi_op_replace.op == op) {
            return ompi_osc_rdma_put_contig (sync, peer, target_address, target_handle, (void *) source, len, request);
        }

        if (&ompi_mpi_op_no_op.op != op) {
            /* NTH: need to cast away const for the source buffer. the buffer will not be modified by this call */
            ompi_op_reduce (op, (void *) source, ptr, source_count, source_datatype);

            return ompi_osc_rdma_put_contig (sync, peer, target_address, target_handle, ptr, len, request);
        }

        /* nothing more to do for this request */
        ompi_osc_rdma_request_complete (request, MPI_SUCCESS);

        return OMPI_SUCCESS;
    }

    return ompi_osc_rdma_put_contig (sync, peer, target_address, target_handle, (void *) source, len, request);
}

static void ompi_osc_rdma_gacc_master_cleanup (ompi_osc_rdma_request_t *request)
{
    ompi_osc_rdma_peer_accumulate_cleanup (request->module, request->peer, !ompi_osc_rdma_peer_is_exclusive (request->peer));
}

static inline int ompi_osc_rdma_gacc_master (ompi_osc_rdma_sync_t *sync, const void *source_addr, int source_count,
                                             ompi_datatype_t *source_datatype, void *result_addr, int result_count,
                                             ompi_datatype_t *result_datatype, ompi_osc_rdma_peer_t *peer, uint64_t target_address,
                                             mca_btl_base_registration_handle_t *target_handle, int target_count,
                                             ompi_datatype_t *target_datatype, ompi_op_t *op, ompi_osc_rdma_request_t *request)
{
    ompi_osc_rdma_module_t *module = sync->module;
    struct iovec source_iovec[OMPI_OSC_RDMA_DECODE_MAX], target_iovec[OMPI_OSC_RDMA_DECODE_MAX];
    const size_t acc_limit = (mca_osc_rdma_component.buffer_size >> 3);
    uint32_t source_primitive_count, target_primitive_count;
    opal_convertor_t source_convertor, target_convertor, result_convertor;
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

    if (!request) {
        OMPI_OSC_RDMA_REQUEST_ALLOC(module, peer, request);
        request->internal = true;
    }

    if (&ompi_mpi_op_no_op.op == op) {
        /* NTH: just zero these out to catch any coding errors (they should be ignored in the no-op case) */
        source_count = 0;
        source_datatype = NULL;
        source_addr = NULL;
    }

    request->cleanup = ompi_osc_rdma_gacc_master_cleanup;
    request->type = result_datatype ? OMPI_OSC_RDMA_TYPE_GET_ACC : OMPI_OSC_RDMA_TYPE_ACC;

    (void) ompi_datatype_get_extent (target_datatype, &lb, &extent);
    target_address += lb;

    /* fast path for accumulate on built-in types */
    if (OPAL_LIKELY((!source_count || ompi_datatype_is_predefined (source_datatype)) &&
                    ompi_datatype_is_predefined (target_datatype) &&
                    (!result_count || ompi_datatype_is_predefined (result_datatype)) &&
                    (target_datatype->super.size * target_count <= acc_limit))) {
        if (source_datatype) {
            (void) ompi_datatype_get_extent (source_datatype, &lb, &extent);
            source_addr = (void *)((intptr_t) source_addr + lb);
        }

        if (result_datatype) {
            (void) ompi_datatype_get_extent (result_datatype, &lb, &extent);
            result_addr = (void *)((intptr_t) result_addr + lb);
        }

        ret = ompi_osc_rdma_gacc_contig (sync, source_addr, source_count, source_datatype, result_addr,
                                         result_count, result_datatype, NULL, peer, target_address,
                                         target_handle, target_count, target_datatype, op,
                                         request);
        if (OPAL_LIKELY(OMPI_SUCCESS == ret)) {
            return OMPI_SUCCESS;
        }

        if (source_datatype) {
            /* the convertors will handle the lb */
            (void) ompi_datatype_get_extent (source_datatype, &lb, &extent);
            source_addr = (void *)((intptr_t) source_addr - lb);
        }

        if (result_datatype) {
            (void) ompi_datatype_get_extent (result_datatype, &lb, &extent);
            result_addr = (void *)((intptr_t) result_addr - lb);
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
        ret = opal_convertor_copy_and_prepare_for_send (ompi_mpi_local_convertor, &source_datatype->super, source_count, source_addr,
                                                       0, &source_convertor);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            return ret;
        }
        source_iov_count = 0;
    } else {
        source_iovec[0].iov_len = (size_t) -1;
        source_iovec[0].iov_base = NULL;
        source_iov_count = 1;
    }

    if (result_datatype) {
        OBJ_CONSTRUCT(&result_convertor, opal_convertor_t);
        ret = opal_convertor_copy_and_prepare_for_recv (ompi_mpi_local_convertor, &result_datatype->super, result_count, result_addr,
                                                       0, &result_convertor);
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

    /* keep the request from completing until all the transfers have started */
    request->outstanding_requests = 1;

    target_iov_index = 0;
    target_iov_count = 0;
    source_iov_index = 0;
    result_position = 0;
    subreq = NULL;

    do {
        /* decode segments of the target buffer */
        target_iov_count = OMPI_OSC_RDMA_DECODE_MAX;
        target_iov_index = 0;
        done = opal_convertor_raw (&target_convertor, target_iovec, &target_iov_count, &target_size);

        /* loop on the source segments (if any) until we have exhaused the decoded target data */
        while (target_iov_index != target_iov_count) {
            if (source_iov_count == source_iov_index) {
                /* decode segments of the source data */
                source_iov_count = OMPI_OSC_RDMA_DECODE_MAX;
                source_iov_index = 0;
                (void) opal_convertor_raw (&source_convertor, source_iovec, &source_iov_count, &source_size);
            }

            /* we already checked that the target was large enough. this should be impossible */
            assert (0 != target_iov_count);

            /* determine how much to put in this operation */
            if (source_count) {
                acc_len = opal_min(opal_min(target_iovec[target_iov_index].iov_len, source_iovec[source_iov_index].iov_len), acc_limit);
            } else {
                acc_len = opal_min(target_iovec[target_iov_index].iov_len, acc_limit);
            }

            if (0 != acc_len) {
                /* execute the get-accumulate */
                if (!subreq) {
                    OMPI_OSC_RDMA_REQUEST_ALLOC(module, peer, subreq);
                    subreq->internal = true;
                    subreq->parent_request = request;
                    subreq->type = result_datatype ? OMPI_OSC_RDMA_TYPE_GET_ACC : OMPI_OSC_RDMA_TYPE_ACC;
                    (void) OPAL_THREAD_ADD_FETCH32 (&request->outstanding_requests, 1);
                }

                ret = ompi_osc_rdma_gacc_contig (sync, source_iovec[source_iov_index].iov_base, acc_len / target_primitive->super.size,
                                                 target_primitive, NULL, 0, NULL, result_datatype ? &result_convertor : NULL, peer,
                                                 (uint64_t) (intptr_t) target_iovec[target_iov_index].iov_base, target_handle,
                                                 acc_len / target_primitive->super.size, target_primitive, op, subreq);
                if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                    if (OPAL_UNLIKELY(OMPI_ERR_OUT_OF_RESOURCE != ret)) {
                        OMPI_OSC_RDMA_REQUEST_RETURN(subreq);
                        (void) OPAL_THREAD_ADD_FETCH32 (&request->outstanding_requests, -1);
                        /* something bad happened. need to figure out how to handle these errors */
                        return ret;
                    }

                    /* progress and try again */
                    ompi_osc_rdma_progress (module);
                    continue;
                }
            }

            subreq = NULL;

            /* adjust io vectors */
            target_iovec[target_iov_index].iov_len -= acc_len;
            target_iovec[target_iov_index].iov_base = (void *)((intptr_t) target_iovec[target_iov_index].iov_base + acc_len);
            target_iov_index += (0 == target_iovec[target_iov_index].iov_len);

            result_position += acc_len;

            if (source_datatype) {
                source_iov_index += (0 == source_iovec[source_iov_index].iov_len);
                source_iovec[source_iov_index].iov_len -= acc_len;
                source_iovec[source_iov_index].iov_base = (void *)((intptr_t) source_iovec[source_iov_index].iov_base + acc_len);
            }
        }
    } while (!done);

    /* release our reference so the request can complete */
    ompi_osc_rdma_request_deref (request);

    if (source_datatype) {
        opal_convertor_cleanup (&source_convertor);
        OBJ_DESTRUCT(&source_convertor);
    }

    if (result_datatype) {
        opal_convertor_cleanup (&result_convertor);
        OBJ_DESTRUCT(&result_convertor);
    }

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "finished scheduling rdma on non-contiguous datatype(s)");

    opal_convertor_cleanup (&target_convertor);
    OBJ_DESTRUCT(&target_convertor);

    return OMPI_SUCCESS;
}

static inline int ompi_osc_rdma_cas_atomic (ompi_osc_rdma_sync_t *sync, const void *source_addr, const void *compare_addr,
                                            void *result_addr, ompi_datatype_t *datatype, ompi_osc_rdma_peer_t *peer,
                                            uint64_t target_address, mca_btl_base_registration_handle_t *target_handle,
                                            bool lock_acquired)
{
    ompi_osc_rdma_module_t *module = sync->module;
    const size_t size = datatype->super.size;
    int64_t compare, source;
    int flags, ret;

    if (8 != size && !(4 == size && (MCA_BTL_ATOMIC_SUPPORTS_32BIT & module->atomic_flags))) {
        return OMPI_ERR_NOT_SUPPORTED;
    }

    compare = (8 == size) ? ((int64_t *) compare_addr)[0] : ((int32_t *) compare_addr)[0];
    source = (8 == size) ? ((int64_t *) source_addr)[0] : ((int32_t *) source_addr)[0];

    flags = ompi_osc_rdma_set_btl_flags(module, datatype, size);

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "initiating compare-and-swap using %d-bit btl atomics. compare: 0x%"
                     PRIx64 ", origin: 0x%" PRIx64, (int) size * 8, *((int64_t *) compare_addr), *((int64_t *) source_addr));

    ret = ompi_osc_rdma_btl_cswap (module, peer->data_btl_index, peer->data_endpoint, target_address, target_handle,
                                   compare, source, flags, result_addr);
    if (OPAL_LIKELY(OMPI_SUCCESS == ret)) {
        ompi_osc_rdma_peer_accumulate_cleanup (module, peer, lock_acquired);
    }

    return ret;
}

/**
 * ompi_osc_rdma_cas_get_complete:
 * Note: This function will not work as is in a heterogeneous environment.
 */
static void ompi_osc_rdma_cas_put_complete (struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                                            void *local_address, mca_btl_base_registration_handle_t *local_handle,
                                            void *context, void *data, int status)
{
    bool *complete = (bool *) context;

    *complete = true;
}

/**
 * @brief Support for compare-and-swap on arbitraty-sized datatypes
 *
 * This function is necessary to support compare-and-swap on types larger
 * than 64-bits. As of MPI-3.1 this can include MPI_INTEGER16 and possibly
 * MPI_LON_LONG_INT. The former is a 128-bit value and the later *may*
 * be depending on the platform, compiler, etc. This function currently
 * blocks until the operation is complete.
 */
static inline int cas_rdma (ompi_osc_rdma_sync_t *sync, const void *source_addr, const void *compare_addr, void *result_addr,
                            ompi_datatype_t *datatype, ompi_osc_rdma_peer_t *peer, uint64_t target_address,
                            mca_btl_base_registration_handle_t *target_handle, bool lock_acquired)
{
    ompi_osc_rdma_module_t *module = sync->module;
    unsigned long len = datatype->super.size;
    mca_btl_base_registration_handle_t *local_handle = NULL;
    ompi_osc_rdma_frag_t *frag = NULL;
    volatile bool complete = false;
    /* drop the const. this code will not attempt to change the value */
    char *ptr = (char *) source_addr;
    int ret;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "initiating compare-and-swap using RMDA on %lu bytes to remote address %" PRIx64
                     ", sync %p", len, target_address, (void *) sync);

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "RDMA compare-and-swap initiating blocking btl get...");
    ret = ompi_osc_get_data_blocking (module, peer->data_btl_index, peer->data_endpoint, target_address,
                                      target_handle, result_addr, len);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    if (0 != memcmp (result_addr, compare_addr, len)) {
        /* value does not match compare value, nothing more to do*/
        ompi_osc_rdma_peer_accumulate_cleanup (module, peer, lock_acquired);
        return OMPI_SUCCESS;
    }

    if (module->use_memory_registration) {
        mca_btl_base_module_t *btl = ompi_osc_rdma_selected_btl (module, peer->data_btl_index);
        if (len > btl->btl_put_local_registration_threshold) {
            do {
                ret = ompi_osc_rdma_frag_alloc(module, len, &frag, &ptr);
                if (OPAL_UNLIKELY(OMPI_SUCCESS == ret)) {
                    break;
                }

                ompi_osc_rdma_progress (module);
            } while (1);

            memcpy(ptr, source_addr, len);
            local_handle = frag->handle;
        }
    }

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "RDMA compare-and-swap initiating blocking btl put...");

    do {
        ret = ompi_osc_rdma_btl_put(module, peer->data_btl_index, peer->data_endpoint,
                                    ptr, target_address, local_handle, target_handle,
                                    len, 0, MCA_BTL_NO_ORDER,
                                    ompi_osc_rdma_cas_put_complete, (void *) &complete, NULL);
        if (OPAL_SUCCESS == ret || (OPAL_ERR_OUT_OF_RESOURCE != ret && OPAL_ERR_TEMP_OUT_OF_RESOURCE != ret)) {
            break;
        }

        /* spin a bit on progress */
        ompi_osc_rdma_progress (module);
    } while (1);

    if (OPAL_SUCCESS != ret) {
        /* something went horribly wrong */
        return ret;
    }

    while (!complete) {
        ompi_osc_rdma_progress (module);
    }

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "RDMA compare-and-swap compare-and-swap complete");

    if (frag) {
        ompi_osc_rdma_frag_complete (frag);
    }

    ompi_osc_rdma_peer_accumulate_cleanup (module, peer, lock_acquired);

    return ret;
}


int ompi_osc_rdma_compare_and_swap (const void *origin_addr, const void *compare_addr, void *result_addr,
                                    ompi_datatype_t *dt, int target_rank, ptrdiff_t target_disp,
                                    ompi_win_t *win)
{
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    ompi_osc_rdma_peer_t *peer;
    mca_btl_base_registration_handle_t *target_handle;
    ompi_osc_rdma_sync_t *sync;
    uint64_t target_address;
    ptrdiff_t true_lb, true_extent;
    bool lock_acquired = false;
    int ret;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "cswap: 0x%lx, 0x%lx, 0x%lx, %s, %d, %d, %s",
                     (unsigned long) origin_addr, (unsigned long) compare_addr, (unsigned long) result_addr,
                     dt->name, target_rank, (int) target_disp, win->w_name);

    sync = ompi_osc_rdma_module_sync_lookup (module, target_rank, &peer);
    if (OPAL_UNLIKELY(NULL == sync)) {
        return OMPI_ERR_RMA_SYNC;
    }

    ret = ompi_datatype_get_true_extent(dt, &true_lb, &true_extent);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    ret = osc_rdma_get_remote_segment (module, peer, target_disp, true_lb+true_extent, &target_address, &target_handle);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != ret)) {
        return ret;
    }

    /* to ensure order wait until the previous accumulate completes */
    while (!ompi_osc_rdma_peer_test_set_flag (peer, OMPI_OSC_RDMA_PEER_ACCUMULATING)) {
        ompi_osc_rdma_progress (module);
    }

    /* get an exclusive lock on the peer */
    if (!ompi_osc_rdma_peer_is_exclusive (peer) && !(module->acc_single_intrinsic || win->w_acc_ops <= OMPI_WIN_ACCUMULATE_OPS_SAME_OP)) {
        (void) ompi_osc_rdma_lock_acquire_exclusive (module, peer, offsetof (ompi_osc_rdma_state_t, accumulate_lock));
        lock_acquired = true;
    }

    /* operate in (shared) memory if there is only a single node
     * OR if we have an exclusive lock
     * OR if other processes won't try to use the network either */
    bool use_shared_mem = module->single_node ||
                          (ompi_osc_rdma_peer_local_base (peer) &&
                              (ompi_osc_rdma_peer_is_exclusive (peer) ||
                                  !module->acc_single_intrinsic));

    if (!use_shared_mem) {
        /* either we have an exclusive lock (via MPI_Win_lock() or the accumulate lock) or the
         * user has indicated that they will only use the same op (or same op and no op) for
         * operations on overlapping memory ranges. that indicates it is safe to go ahead and
         * use network atomic operations. */
        if(ompi_osc_base_is_atomic_size_supported(target_address, dt->super.size)) {
            ret = ompi_osc_rdma_cas_atomic (sync, origin_addr, compare_addr, result_addr, dt,
                                            peer, target_address, target_handle, lock_acquired);
            if (OMPI_SUCCESS == ret) {
                return OMPI_SUCCESS;
            }
        }
    }

    if (!(lock_acquired || ompi_osc_rdma_peer_is_exclusive (peer))) {
        (void) ompi_osc_rdma_lock_acquire_exclusive (module, peer, offsetof (ompi_osc_rdma_state_t, accumulate_lock));
        lock_acquired = true;
    }

    if (ompi_osc_rdma_peer_local_base (peer)) {
        ret = ompi_osc_rdma_cas_local (origin_addr, compare_addr, result_addr, dt,
                                       peer, target_address, target_handle, module,
                                       lock_acquired);
    } else {
        ret = cas_rdma (sync, origin_addr, compare_addr, result_addr, dt, peer, target_address,
                        target_handle, lock_acquired);
    }

    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        /* operation failed. the application will most likely abort but we still want to leave the window
         * in working state if possible. on successful completion the above calls with clear the lock
         * and accumulate state */
        ompi_osc_rdma_peer_accumulate_cleanup (module, peer, lock_acquired);
    }

    return ret;
}


static inline
int ompi_osc_rdma_rget_accumulate_internal (ompi_win_t *win, const void *origin_addr, int origin_count,
                                            ompi_datatype_t *origin_datatype, void *result_addr, int result_count,
                                            ompi_datatype_t *result_datatype,  int target_rank, MPI_Aint target_disp,
                                            int target_count, ompi_datatype_t *target_datatype, ompi_op_t *op,
                                            ompi_request_t **request_out)
{
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    mca_btl_base_registration_handle_t *target_handle;
    uint64_t target_address;
    ptrdiff_t target_lb, target_span;
    ompi_osc_rdma_request_t *rdma_request = NULL;
    bool lock_acquired = false;
    ompi_osc_rdma_sync_t *sync;
    ompi_osc_rdma_peer_t *peer;
    int ret;

    sync = ompi_osc_rdma_module_sync_lookup (module, target_rank, &peer);
    if (OPAL_UNLIKELY(NULL == sync)) {
        return OMPI_ERR_RMA_SYNC;
    }

    if (request_out) {
        OMPI_OSC_RDMA_REQUEST_ALLOC(module, peer, rdma_request);
        *request_out = &rdma_request->super;
    }

    /* short-circuit case. note that origin_count may be 0 if op is MPI_NO_OP */
    if ((result_addr && 0 == result_count) || 0 == target_count) {
        if (rdma_request) {
            ompi_osc_rdma_request_complete (rdma_request, MPI_SUCCESS);
        }

        return OMPI_SUCCESS;
    }

    target_span = opal_datatype_span(&target_datatype->super, target_count, &target_lb);

    // a buffer defined by (buf, count, dt)
    // will have data starting at buf+offset and ending len bytes later:
    ret = osc_rdma_get_remote_segment (module, peer, target_disp, target_span+target_lb, &target_address, &target_handle);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    /* to ensure order wait until the previous accumulate completes */
    while (!ompi_osc_rdma_peer_test_set_flag (peer, OMPI_OSC_RDMA_PEER_ACCUMULATING)) {
        ompi_osc_rdma_progress (module);
    }

    /* get an exclusive lock on the peer if needed */
    if (!ompi_osc_rdma_peer_is_exclusive (peer) && !module->acc_single_intrinsic) {
        lock_acquired = true;
        (void) ompi_osc_rdma_lock_acquire_exclusive (module, peer, offsetof (ompi_osc_rdma_state_t, accumulate_lock));
    }

    /* could not use network atomics. acquire the lock if needed and continue. */
    if (!lock_acquired && !ompi_osc_rdma_peer_is_exclusive (peer)) {
        lock_acquired = true;
        (void) ompi_osc_rdma_lock_acquire_exclusive (module, peer, offsetof (ompi_osc_rdma_state_t, accumulate_lock));
    }

    if (ompi_osc_rdma_peer_local_base (peer)) {
        /* local/self optimization */
        ret = ompi_osc_rdma_gacc_local (origin_addr, origin_count, origin_datatype, result_addr, result_count,
                                        result_datatype, peer, target_address, target_handle, target_count,
                                        target_datatype, op, module, rdma_request, lock_acquired);
    } else {
        /* do not need to pass the lock acquired flag to this function. the value of the flag can be obtained
         * just by calling ompi_osc_rdma_peer_is_exclusive() in this case. */
        ret = ompi_osc_rdma_gacc_master (sync, origin_addr, origin_count, origin_datatype, result_addr, result_count,
                                         result_datatype, peer, target_address, target_handle, target_count,
                                         target_datatype, op, rdma_request);
    }

    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        if (request_out) {
            *request_out = &ompi_request_null.request;
            OMPI_OSC_RDMA_REQUEST_RETURN(rdma_request);
        }
        ompi_osc_rdma_peer_accumulate_cleanup (module, peer, lock_acquired);
    }

    return ret;
}

int ompi_osc_rdma_get_accumulate (const void *origin_addr, int origin_count, ompi_datatype_t *origin_datatype,
                                  void *result_addr, int result_count, ompi_datatype_t *result_datatype,
                                  int target_rank, MPI_Aint target_disp, int target_count, ompi_datatype_t *target_datatype,
                                  ompi_op_t *op, ompi_win_t *win)
{
    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "get_acc: 0x%lx, %d, %s, 0x%lx, %d, %s, %d, 0x%lx, %d, %s, %s, %s",
                     (unsigned long) origin_addr, origin_count, origin_datatype->name,
                     (unsigned long) result_addr, result_count, result_datatype->name, target_rank,
                     (unsigned long) target_disp, target_count, target_datatype->name, op->o_name,
                     win->w_name);

    return ompi_osc_rdma_rget_accumulate_internal (win, origin_addr, origin_count, origin_datatype,
                                                   result_addr, result_count, result_datatype,
                                                   target_rank, target_disp, target_count,
                                                   target_datatype, op, NULL);
}


int ompi_osc_rdma_rget_accumulate (const void *origin_addr, int origin_count, ompi_datatype_t *origin_datatype,
                                   void *result_addr, int result_count, ompi_datatype_t *result_datatype,
                                   int target_rank, MPI_Aint target_disp, int target_count, ompi_datatype_t *target_datatype,
                                   ompi_op_t *op, ompi_win_t *win, ompi_request_t **request)
{
    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "rget_acc: 0x%lx, %d, %s, 0x%lx, %d, %s, %d, 0x%lx, %d, %s, %s, %s",
                     (unsigned long) origin_addr, origin_count, origin_datatype->name,
                     (unsigned long) result_addr, result_count, result_datatype->name, target_rank,
                     (unsigned long) target_disp, target_count, target_datatype->name, op->o_name,
                     win->w_name);

    return ompi_osc_rdma_rget_accumulate_internal (win, origin_addr, origin_count, origin_datatype, result_addr,
                                                   result_count, result_datatype, target_rank, target_disp,
                                                   target_count, target_datatype, op, request);
}

int ompi_osc_rdma_raccumulate (const void *origin_addr, int origin_count, ompi_datatype_t *origin_datatype, int target_rank,
                               ptrdiff_t target_disp, int target_count, ompi_datatype_t *target_datatype, ompi_op_t *op,
                               ompi_win_t *win, ompi_request_t **request)
{
    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "racc: 0x%lx, %d, %s, %d, 0x%lx, %d, %s, %s, %s",
                     (unsigned long) origin_addr, origin_count, origin_datatype->name, target_rank,
                     (unsigned long) target_disp, target_count, target_datatype->name, op->o_name, win->w_name);

    return ompi_osc_rdma_rget_accumulate_internal (win, origin_addr, origin_count, origin_datatype, NULL, 0,
                                                   NULL, target_rank, target_disp, target_count, target_datatype,
                                                   op, request);
}

int ompi_osc_rdma_accumulate (const void *origin_addr, int origin_count, ompi_datatype_t *origin_datatype, int target_rank,
                              ptrdiff_t target_disp, int target_count, ompi_datatype_t *target_datatype, ompi_op_t *op,
                              ompi_win_t *win)
{
    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "acc: 0x%lx, %d, %s, %d, 0x%lx, %d, %s, %s, %s",
                     (unsigned long) origin_addr, origin_count, origin_datatype->name, target_rank,
                     (unsigned long) target_disp, target_count, target_datatype->name, op->o_name, win->w_name);

    return ompi_osc_rdma_rget_accumulate_internal (win, origin_addr, origin_count, origin_datatype, NULL, 0,
                                                   NULL, target_rank, target_disp, target_count, target_datatype,
                                                   op, NULL);
}


int ompi_osc_rdma_fetch_and_op (const void *origin_addr, void *result_addr, ompi_datatype_t *dt, int target_rank,
                                ptrdiff_t target_disp, ompi_op_t *op, ompi_win_t *win)
{
    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_TRACE, "fop: %p, %s, %d, %lu, %s, %s", result_addr, dt->name,
                     target_rank, (unsigned long) target_disp, op->o_name, win->w_name);

    return ompi_osc_rdma_rget_accumulate_internal (win, origin_addr, 1, dt, result_addr, 1, dt,
                                                   target_rank, target_disp, 1, dt, op, NULL);
}
