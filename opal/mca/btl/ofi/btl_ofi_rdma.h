/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2018      Intel, Inc, All rights reserved
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef BTL_OFI_RDMA_H
#define BTL_OFI_RDMA_H

#include "opal/threads/thread_usage.h"

#include "btl_ofi.h"
#include "btl_ofi_endpoint.h"

mca_btl_ofi_rdma_completion_t *mca_btl_ofi_rdma_completion_alloc (
                                         mca_btl_base_module_t *btl,
                                         mca_btl_base_endpoint_t *endpoint,
                                         mca_btl_ofi_context_t *ofi_context,
                                         void *local_address,
                                         mca_btl_base_registration_handle_t *local_handle,
                                         mca_btl_base_rdma_completion_fn_t cbfunc,
                                         void *cbcontext, void *cbdata,
                                         int type);

#define MCA_BTL_OFI_NUM_RDMA_INC(module)                                                \
            OPAL_THREAD_ADD_FETCH64(&(module)->outstanding_rdma, 1);                    \
            if (module->outstanding_rdma > mca_btl_ofi_component.progress_threshold){   \
                mca_btl_ofi_component.super.btl_progress();                             \
            }

#define MCA_BTL_OFI_NUM_RDMA_DEC(module)                            \
            OPAL_THREAD_ADD_FETCH64(&(module)->outstanding_rdma, -1);

#endif /* !defined(BTL_OFI_RDMA_H) */

