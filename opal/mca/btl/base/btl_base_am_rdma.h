/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2020      Google, LLC. All rights reserved.
 * Copyright (c) 2021      Amazon.com, Inc. or its affiliates.  All Rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * This file provides support for active-message (send/recv) based RDMA.
 * It can be used with any btl that provides a minimum of send support but
 * can also be used with partial-RDMA BTLs (put only, get only, etc)
 * to provide a complete RDMA interface.
 *
 * There are two modes of using this interface, depending on your
 * requirements:
 *
 * First, this interface can be used to provide a complete
 * put/get/atomic interface for BTLs which do not natively provide
 * such an interface.  In this mode, active message rdma functions are
 * only used if the underlying implementation does not already provide
 * the required functionality.  For example, if a BTL natively
 * supports put but not get, the interface would provide an emulated
 * get.  The registration, completion and atomicity semantics of the
 * BTL remain the native interface's capabilities.  That is, if the
 * native interface does not provide remote completion or atomics that
 * are atomic with processor atomics, neither will the interface after
 * initializing the am rdma interface for that BTL.  This mode will
 * likely give better performance than the second mode for transfers
 * that fit within the BTL's native semantics.  In this mode, the BTL
 * interface is updated so that the btl_{put, get, atomic_fop,
 * atomic_cswap} function pointers are usage.  However, the btl
 * capability flags will not be updated to indicate native support of
 * the emulated functionality (for example, if btl_get() is emulated,
 * MCA_BTL_FLAGS_GET will not be set).  Instead, the emulated flags
 * will be set (MCA_BTL_FLAGS_PUT_AM, MCA_BTL_FLAGS_GET_AM,
 * MCA_BTL_FLAGS_ATOMIC_AM_FOP, etc.).
 *
 * Second, this interface can be used to provide different
 * sementicsthan a BTL natively provides.  This mode is not
 * transparent to the caller (unlike the first mode).  Instead, the
 * caller must manage calling the active message put/get/atomic
 * interface directly (rather than through the BTL function pointers).
 * For interfaces which require strict remote completion or require
 * implicit memory registration, this can greatly simplify the code,
 * in return for marginally more management complexity and lower
 * performance.
 *
 * While the calling convention and initialization are different, the
 * communication routines uses by the active message rdma
 * implementation are identical in both modes of operation.
 */

#include "opal_config.h"
#include "opal/mca/btl/btl.h"

#if !defined(OPAL_MCA_BTL_BASE_AM_RDMA_H)
#    define OPAL_MCA_BTL_BASE_AM_RDMA_H

/**
 * @brief initialize active-message RDMA/atomic support
 *
 * @param  btl[in,out]  btl module to augment
 *
 * @retval OPAL_SUCCESS  btl successfully updated, btl already
 *                       updated, or btl has all available
 *                       functionality natively.
 * @retval OPAL_ERR_TEMP_OUT_OF_RESOURCE Allocating BTL-level data
 *                       structure failed.
 *
 * This function adds functionality to the btl for any missing RDMA/atomic
 * operation. Atomic operations are entirely emulated using send/recv and
 * work best with a btl that also has async-progress enabled. Put/get
 * support will use either send/recv or get (for put)/put (for get) (if
 * available).
 *
 * Note that calling this function will change the BTL interface.
 * Care must be taken to not call this function outside of early
 * initialization routines.
 */
int mca_btl_base_am_rdma_init(mca_btl_base_module_t *btl);

struct mca_btl_base_am_rdma_module_t;

typedef int (*mca_btl_base_am_rdma_module_put_fn_t)(
    struct mca_btl_base_am_rdma_module_t *am_btl, struct mca_btl_base_endpoint_t *endpoint,
    void *local_address, uint64_t remote_address,
    struct mca_btl_base_registration_handle_t *local_handle,
    struct mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags, int order,
    mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata);

typedef int (*mca_btl_base_am_rdma_module_get_fn_t)(
    struct mca_btl_base_am_rdma_module_t *am_btl, struct mca_btl_base_endpoint_t *endpoint,
    void *local_address, uint64_t remote_address,
    struct mca_btl_base_registration_handle_t *local_handle,
    struct mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags, int order,
    mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata);

typedef int (*mca_btl_base_am_rdma_module_atomic_fop64_fn_t)(
    struct mca_btl_base_am_rdma_module_t *am_btl, struct mca_btl_base_endpoint_t *endpoint,
    void *local_address, uint64_t remote_address,
    struct mca_btl_base_registration_handle_t *local_handle,
    struct mca_btl_base_registration_handle_t *remote_handle, mca_btl_base_atomic_op_t op,
    uint64_t operand, int flags, int order, mca_btl_base_rdma_completion_fn_t cbfunc,
    void *cbcontext, void *cbdata);

typedef int (*mca_btl_base_am_rdma_module_atomic_cswap64_fn_t)(
    struct mca_btl_base_am_rdma_module_t *am_btl, struct mca_btl_base_endpoint_t *endpoint,
    void *local_address, uint64_t remote_address,
    struct mca_btl_base_registration_handle_t *local_handle,
    struct mca_btl_base_registration_handle_t *remote_handle, uint64_t compare, uint64_t value,
    int flags, int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata);

struct mca_btl_base_am_rdma_module_t {
    opal_object_t super;
    mca_btl_base_module_t *btl;
    bool use_rdma_put;
    bool use_rdma_get;

    size_t am_btl_put_limit;
    size_t am_btl_put_alignment;
    size_t am_btl_get_limit;
    size_t am_btl_get_alignment;

    mca_btl_base_am_rdma_module_put_fn_t am_btl_put;
    mca_btl_base_am_rdma_module_get_fn_t am_btl_get;
    mca_btl_base_am_rdma_module_atomic_fop64_fn_t am_btl_atomic_fop;
    mca_btl_base_am_rdma_module_atomic_cswap64_fn_t am_btl_atomic_cswap;
};
typedef struct mca_btl_base_am_rdma_module_t mca_btl_base_am_rdma_module_t;

OPAL_DECLSPEC OBJ_CLASS_DECLARATION(mca_btl_base_am_rdma_module_t);


/**
 * @brief create active-message RDMA/atomics functions
 */
int opal_btl_base_am_rdma_create(mca_btl_base_module_t *btl,
                                 uint32_t flags_requested,
                                 bool no_memory_registration,
                                 mca_btl_base_am_rdma_module_t **am_module);

int opal_btl_base_am_rdma_destroy(mca_btl_base_am_rdma_module_t *am_module);

#endif /* OPAL_MCA_BTL_BASE_AM_RDMA_H */
