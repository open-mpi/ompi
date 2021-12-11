/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2021-2021 Amazon.com, INC. or its affiliates. All rights reserved
 */

#ifndef OMPI_OSC_RDMA_BTL_WRAPPER_T
#define OMPI_OSC_RDMA_BTL_WRAPPER_T

#include "opal/mca/btl/btl.h"

enum ompi_osc_rdma_btl_type_t {
    OMPI_OSC_RDMA_BTL_PRIMARY,
    OMPI_OSC_RDMA_BTL_ALTERNATE,
};

/**
 * @brief ompi_osc_rdma_btl_warpper_t is a subset of mca_btl_base_module_t
 *        that are used by osc/rdma component
 */
struct ompi_osc_rdma_btl_wrapper_t {
    mca_btl_base_module_t *btl_module;

    bool btl_atomic_ops;
    bool btl_rdma_remote_completion;
    bool btl_atomic_support_glob;

    mca_btl_base_module_put_fn_t btl_put;
    size_t btl_put_limit;
    size_t btl_put_alignment;
    size_t btl_put_local_registration_threshold;

    mca_btl_base_module_get_fn_t btl_get;
    size_t btl_get_limit; 
    size_t btl_get_alignment;
    size_t btl_get_local_registration_threshold;

    mca_btl_base_module_atomic_op64_fn_t btl_atomic_op;
    mca_btl_base_module_atomic_fop64_fn_t btl_atomic_fop;
    mca_btl_base_module_atomic_cswap64_fn_t btl_atomic_cswap;
    uint32_t btl_atomic_flags;

    mca_btl_base_module_register_mem_fn_t btl_register_mem;
    mca_btl_base_module_deregister_mem_fn_t btl_deregister_mem;
    size_t btl_registration_handle_size;

    mca_btl_base_module_flush_fn_t btl_flush;
};

typedef struct ompi_osc_rdma_btl_wrapper_t ompi_osc_rdma_btl_wrapper_t;

ompi_osc_rdma_btl_wrapper_t *ompi_osc_rdma_btl_wrapper_alloc(mca_btl_base_module_t *btl_module, enum ompi_osc_rdma_btl_type_t btl_type);

#endif
