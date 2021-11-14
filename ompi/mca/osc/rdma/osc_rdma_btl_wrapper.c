/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2021-2021 Amazon.com, INC. or its affiliates. All rights reserved
 */

#include "ompi_config.h"
#include "osc_rdma_btl_wrapper.h"
#include "opal/mca/btl/base/btl_base_am_rdma.h"

/**
 * @brief create a btl wrapper for a btl_module
 *
 * For a primary btl, this function simply copy the data from btl_module
 * to btl_wrapper
 *
 * For an alternate btl, this function create a btl wrapper that will
 * always use active message RDMA/atomics on the selected btl module.
 * Even when the btl module support RDMA/atomics natively.
 *
 * The reason osc/rdma does not use an alternate btl's native atomics is because
 * When multiple alternate btls are being used, the atomicity accross btl's own
 * atomics is not guaranteed. Therefore, osc/rdma must use active message atomics.
 *
 * The reason osc/rdma does not use an alternate btls' native RDMA put and get is because
 * it signficantly simplified osc/rdma's completion. The simplication came in two
 * areas:
 *
 * First, active message RDMA supports remote completion. Remote completion
 * is required by several key components of osc/rdma:
 *        the usage of cpu atomics to update peer's state,
 *        the usage of local leader to update peer's state,
 *        osc/rdma's fence implementation.
 *
 * If any alternate does not support remote completionsc/rdma do not use active message RDMAs, it will
 * have to keep track of each selected btl's support of remote completion.
 * If any selected btl does not support remote completion, it will have to
 * disable the usage of cpu atomics, disable the usage of local leader,
 * and implement a different fence mechanism.
 *
 * Second, active message RDMA does not use memory registration explicitly,
 * therefore using it eliminates the need to store and exchange multiple
 * memory registrations.
 */
ompi_osc_rdma_btl_wrapper_t *ompi_osc_rdma_btl_wrapper_alloc(mca_btl_base_module_t *btl_module, enum ompi_osc_rdma_btl_type_t btl_type)
{
    mca_btl_base_module_t btl_module_copy;
    ompi_osc_rdma_btl_wrapper_t *btl_wrapper;

    if (NULL == btl_module) {
        return NULL;
    }

    memcpy(&btl_module_copy, btl_module, sizeof(btl_module_copy));
    if (OMPI_OSC_RDMA_BTL_ALTERNATE == btl_type) {
        /* For an alternate btl, osc/rdma must use active message RDMA/atomics on it,
	 * and not use the btl's native support of RDMA/atomics.
	 *
	 * mca_btl_base_am_rdma_init() setup a btl to use AM atomics/RDMA. However,
	 * if a btl has native RDMA/atomics support, this function will not replace
	 * them with active message RDMA/atomic. Therefore, to ensure active message
	 * RDMA/atomic will replace native RDMA/atomics, the input btl_module to
	 * mca_btl_base_am_rdma_init() must has native RDMA/atomics disabled.
	 *
	 * We cannot disable the btl_module's RDMA/atomic though, because btl_module's
	 * native RDMA/atomic may be used by other modules like pml.
	 *
	 * Hence, we created btl_module_copy here, and disabled its native RDMA/atomic
	 * and call mca_btl_base_am_rdma_init() on it. The resulted btl_module_copy
	 * has all the correct property test for AM RDMA/atomics. These properties
	 * are then copied to btl_wrapper.
	 *
	 * osc/rdma uses the btl_wrapper to ensure active message RDMA/atomics are
	 * used for the btl_module.
	 */
        btl_module_copy.btl_flags &= ~(MCA_BTL_FLAGS_RDMA | MCA_BTL_FLAGS_GET | MCA_BTL_FLAGS_PUT | MCA_BTL_FLAGS_ATOMIC_FOPS | MCA_BTL_FLAGS_ATOMIC_OPS);
        mca_btl_base_am_rdma_init(&btl_module_copy);
	/* AM rdma/atomics does not need explicit memory registration
	 */
	btl_module_copy.btl_register_mem = NULL;
	btl_module_copy.btl_deregister_mem = NULL;
	btl_module_copy.btl_registration_handle_size = 0;
    }

    btl_wrapper = (ompi_osc_rdma_btl_wrapper_t *)calloc(1, sizeof(ompi_osc_rdma_btl_wrapper_t));
    if (NULL == btl_wrapper) {
        return NULL;
    }

    btl_wrapper->btl_module = btl_module;

    btl_wrapper->btl_atomic_ops = (btl_module_copy.btl_flags & MCA_BTL_FLAGS_ATOMIC_OPS);
    btl_wrapper->btl_atomic_support_glob = (btl_module_copy.btl_flags & MCA_BTL_ATOMIC_SUPPORTS_GLOB);
    btl_wrapper->btl_rdma_remote_completion = (btl_module_copy.btl_flags & MCA_BTL_FLAGS_RDMA_REMOTE_COMPLETION);

    btl_wrapper->btl_put = btl_module_copy.btl_put;
    btl_wrapper->btl_put_limit = btl_module_copy.btl_put_limit;
    btl_wrapper->btl_put_alignment = btl_module_copy.btl_put_alignment;
    btl_wrapper->btl_put_local_registration_threshold = btl_module_copy.btl_put_local_registration_threshold;

    btl_wrapper->btl_get = btl_module_copy.btl_get;
    btl_wrapper->btl_get_limit = btl_module_copy.btl_get_limit;
    btl_wrapper->btl_get_alignment = btl_module_copy.btl_get_alignment;
    btl_wrapper->btl_get_local_registration_threshold = btl_module_copy.btl_get_local_registration_threshold;

    btl_wrapper->btl_atomic_op = btl_module_copy.btl_atomic_op;
    btl_wrapper->btl_atomic_fop = btl_module_copy.btl_atomic_fop;
    btl_wrapper->btl_atomic_cswap = btl_module_copy.btl_atomic_cswap;
    btl_wrapper->btl_atomic_flags = btl_module_copy.btl_atomic_flags;

    btl_wrapper->btl_register_mem = btl_module_copy.btl_register_mem;
    btl_wrapper->btl_deregister_mem = btl_module_copy.btl_deregister_mem;
    btl_wrapper->btl_registration_handle_size = btl_module_copy.btl_registration_handle_size;

    btl_wrapper->btl_flush = btl_module_copy.btl_flush;
    return btl_wrapper;
}

