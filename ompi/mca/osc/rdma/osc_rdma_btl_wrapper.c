/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2021-2021 Amazon.com, INC. or its affiliates. All rights reserved
 */

#include "ompi_config.h"
#include "osc_rdma_btl_wrapper.h"

ompi_osc_rdma_btl_wrapper_t *ompi_osc_rdma_btl_wrapper_alloc(mca_btl_base_module_t *btl_module, enum ompi_osc_rdma_btl_type_t btl_type)
{
    ompi_osc_rdma_btl_wrapper_t *btl_wrapper;

    if (NULL == btl_module) {
        return NULL;
    }

    btl_wrapper = (ompi_osc_rdma_btl_wrapper_t *)calloc(1, sizeof(ompi_osc_rdma_btl_wrapper_t));
    if (NULL == btl_wrapper) {
        return NULL;
    }

    btl_wrapper->btl_module = btl_module;

    btl_wrapper->btl_atomic_ops = (btl_module->btl_flags & MCA_BTL_FLAGS_ATOMIC_OPS);
    btl_wrapper->btl_atomic_support_glob = (btl_module->btl_flags & MCA_BTL_ATOMIC_SUPPORTS_GLOB);
    btl_wrapper->btl_rdma_remote_completion = (btl_module->btl_flags & MCA_BTL_FLAGS_RDMA_REMOTE_COMPLETION);

    btl_wrapper->btl_put = btl_module->btl_put;
    btl_wrapper->btl_put_limit = btl_module->btl_put_limit;
    btl_wrapper->btl_put_alignment = btl_module->btl_put_alignment;
    btl_wrapper->btl_put_local_registration_threshold = btl_module->btl_put_local_registration_threshold;

    btl_wrapper->btl_get = btl_module->btl_get;
    btl_wrapper->btl_get_limit = btl_module->btl_get_limit;
    btl_wrapper->btl_get_alignment = btl_module->btl_get_alignment;
    btl_wrapper->btl_get_local_registration_threshold = btl_module->btl_get_local_registration_threshold;

    btl_wrapper->btl_atomic_op = btl_module->btl_atomic_op;
    btl_wrapper->btl_atomic_fop = btl_module->btl_atomic_fop;
    btl_wrapper->btl_atomic_cswap = btl_module->btl_atomic_cswap;
    btl_wrapper->btl_atomic_flags = btl_module->btl_atomic_flags;

    btl_wrapper->btl_register_mem = btl_module->btl_register_mem;
    btl_wrapper->btl_deregister_mem = btl_module->btl_deregister_mem;
    btl_wrapper->btl_registration_handle_size = btl_module->btl_registration_handle_size;

    btl_wrapper->btl_flush = btl_module->btl_flush;
    return btl_wrapper;
}

