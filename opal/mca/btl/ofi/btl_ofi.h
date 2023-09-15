/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2018 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2015-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2018-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * Copyright (c) 2022      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_BTL_OFI_H
#define MCA_BTL_OFI_H

#include "opal_config.h"
#include <string.h>
#include <sys/types.h>

/* Open MPI includes */
#include "opal/mca/btl/base/base.h"
#include "opal/mca/btl/base/btl_base_error.h"
#include "opal/mca/btl/btl.h"
#include "opal/mca/mpool/mpool.h"
#include "opal/mca/pmix/pmix-internal.h"
#include "opal/mca/rcache/base/base.h"
#include "opal/util/event.h"

#include "opal/class/opal_hash_table.h"

#include <rdma/fabric.h>
#include <rdma/fi_cm.h>
#include <rdma/fi_domain.h>
#include <rdma/fi_endpoint.h>
#include <rdma/fi_errno.h>
#include <rdma/fi_rma.h>

BEGIN_C_DECLS
#define MCA_BTL_OFI_MAX_MODULES  16
#define MCA_BTL_OFI_NUM_CQE_READ 64

#define MCA_BTL_OFI_DEFAULT_RD_NUM             10
#define MCA_BTL_OFI_DEFAULT_MAX_CQE            128
#define MCA_BTL_OFI_DEFAULT_PROGRESS_THRESHOLD 64

#define MCA_BTL_OFI_ABORT(args) mca_btl_ofi_exit(args)

#define TWO_SIDED_ENABLED mca_btl_ofi_component.two_sided_enabled

enum mca_btl_ofi_mode {
    MCA_BTL_OFI_MODE_ONE_SIDED = 0,
    MCA_BTL_OFI_MODE_TWO_SIDED,
    MCA_BTL_OFI_MODE_FULL_SUPPORT,
    MCA_BTL_OFI_MODE_TOTAL
};

enum mca_btl_ofi_hdr_type {
    MCA_BTL_OFI_TYPE_PUT = 0,
    MCA_BTL_OFI_TYPE_GET,
    MCA_BTL_OFI_TYPE_AOP,
    MCA_BTL_OFI_TYPE_AFOP,
    MCA_BTL_OFI_TYPE_CSWAP,
    MCA_BTL_OFI_TYPE_SEND,
    MCA_BTL_OFI_TYPE_RECV,
    MCA_BTL_OFI_TYPE_TOTAL
};

struct mca_btl_ofi_context_t {
    int32_t context_id;

    /* transmit context */
    struct fid_ep *tx_ctx;
    struct fid_ep *rx_ctx;

    /* completion queue */
    struct fid_cq *cq;

    /* completion info freelist */
    /* We have it per context to reduce the thread contention
     * on the freelist. Things can get really slow. */
    opal_free_list_t rdma_comp_list;
    opal_free_list_t frag_comp_list;
    opal_free_list_t frag_list;

    /* for thread locking */
    opal_atomic_int32_t lock;
};
typedef struct mca_btl_ofi_context_t mca_btl_ofi_context_t;

/**
 * @brief OFI BTL module
 */
struct mca_btl_ofi_module_t {
    /** base BTL interface */
    mca_btl_base_module_t super;

    /* libfabric components */
    struct fi_info *fabric_info;
    struct fid_fabric *fabric;
    struct fid_domain *domain;
    struct fid_ep *ofi_endpoint;
    struct fid_av *av;

    int num_contexts;
    mca_btl_ofi_context_t *contexts;

    char *linux_device_name;

    /** whether the module has been fully initialized or not */
    bool initialized;
    bool use_virt_addr;
    bool is_scalable_ep;
    bool use_fi_mr_bind;

    opal_atomic_int64_t outstanding_rdma;
    opal_atomic_int64_t outstanding_send;

    /** linked list of BTL endpoints. this list is never searched so
     * there is no need for a complicated structure here at this time*/
    opal_list_t endpoints;

    opal_mutex_t module_lock;
    opal_hash_table_t id_to_endpoint;

    /** registration cache */
    mca_rcache_base_module_t *rcache;
    /* If the underlying OFI provider has its own cache, we want to bypass
     * rcache registration */
    bool bypass_cache;
};
typedef struct mca_btl_ofi_module_t mca_btl_ofi_module_t;

extern mca_btl_ofi_module_t mca_btl_ofi_module_template;

/**
 * @brief OFI BTL component
 */
struct mca_btl_ofi_component_t {
    mca_btl_base_component_3_0_0_t super; /**< base BTL component */

    /** number of TL modules */
    int module_count;
    int num_contexts_per_module;
    int num_cqe_read;
    int progress_threshold;
    int mode;
    int rd_num;
    bool two_sided_enabled;

    size_t namelen;

    /** Maximum inject size */
    size_t max_inject_size;
    bool disable_inject;

    bool disable_hmem;

    /** All BTL OFI modules (1 per tl) */
    mca_btl_ofi_module_t *modules[MCA_BTL_OFI_MAX_MODULES];
};
typedef struct mca_btl_ofi_component_t mca_btl_ofi_component_t;

OPAL_DECLSPEC extern mca_btl_ofi_component_t mca_btl_ofi_component;

struct mca_btl_base_registration_handle_t {
    uint64_t rkey;
    void *desc;
    void *base_addr;
};

struct mca_btl_ofi_reg_t {
    mca_rcache_base_registration_t base;
    struct fid_mr *ur_mr;

    /* remote handle */
    mca_btl_base_registration_handle_t handle;
};
typedef struct mca_btl_ofi_reg_t mca_btl_ofi_reg_t;

OBJ_CLASS_DECLARATION(mca_btl_ofi_reg_t);

struct mca_btl_ofi_header_t {
    mca_btl_base_tag_t tag;
    size_t len;
};
typedef struct mca_btl_ofi_header_t mca_btl_ofi_header_t;

struct mca_btl_ofi_base_frag_t {
    mca_btl_base_descriptor_t base;
    mca_btl_base_segment_t segments[2];

    int context_id;
    struct mca_btl_ofi_module_t *btl;
    struct mca_btl_base_endpoint_t *endpoint;
    opal_free_list_t *free_list;
    mca_btl_ofi_header_t hdr;
};

typedef struct mca_btl_ofi_base_frag_t mca_btl_ofi_base_frag_t;

OBJ_CLASS_DECLARATION(mca_btl_ofi_base_frag_t);

struct mca_btl_ofi_completion_context_t {
    struct fi_context2 ctx;
    void *comp;
};

typedef struct mca_btl_ofi_completion_context_t mca_btl_ofi_completion_context_t;

/* completion structure store information needed
 * for RDMA callbacks */
struct mca_btl_ofi_base_completion_t {
    opal_free_list_item_t comp_list;

    opal_free_list_t *my_list;

    struct mca_btl_base_module_t *btl;
    struct mca_btl_base_endpoint_t *endpoint;
    struct mca_btl_ofi_context_t *my_context;
    int type;
};
typedef struct mca_btl_ofi_base_completion_t mca_btl_ofi_base_completion_t;

struct mca_btl_ofi_rdma_completion_t {
    mca_btl_ofi_base_completion_t base;
    mca_btl_ofi_completion_context_t comp_ctx;
    void *local_address;
    mca_btl_base_registration_handle_t *local_handle;

    uint64_t operand;
    uint64_t compare;

    mca_btl_base_rdma_completion_fn_t cbfunc;
    void *cbcontext;
    void *cbdata;
};
typedef struct mca_btl_ofi_rdma_completion_t mca_btl_ofi_rdma_completion_t;

struct mca_btl_ofi_frag_completion_t {
    mca_btl_ofi_base_completion_t base;
    mca_btl_ofi_completion_context_t comp_ctx;
    mca_btl_ofi_base_frag_t *frag;
};
typedef struct mca_btl_ofi_frag_completion_t mca_btl_ofi_frag_completion_t;

OBJ_CLASS_DECLARATION(mca_btl_ofi_rdma_completion_t);
OBJ_CLASS_DECLARATION(mca_btl_ofi_frag_completion_t);

/**
 * Initiate an asynchronous put.
 * Completion Semantics: if this function returns a 1 then the operation
 *                       is complete. a return of OPAL_SUCCESS indicates
 *                       the put operation has been queued with the
 *                       network. the local_handle can not be deregistered
 *                       until all outstanding operations on that handle
 *                       have been completed.
 *
 * @param btl (IN)            BTL module
 * @param endpoint (IN)       BTL addressing information
 * @param local_address (IN)  Local address to put from (registered)
 * @param remote_address (IN) Remote address to put to (registered remotely)
 * @param local_handle (IN)   Registration handle for region containing
 *                            (local_address, local_address + size)
 * @param remote_handle (IN)  Remote registration handle for region containing
 *                            (remote_address, remote_address + size)
 * @param size (IN)           Number of bytes to put
 * @param flags (IN)          Flags for this put operation
 * @param order (IN)          Ordering
 * @param cbfunc (IN)         Function to call on completion (if queued)
 * @param cbcontext (IN)      Context for the callback
 * @param cbdata (IN)         Data for callback
 *
 * @retval OPAL_SUCCESS    The descriptor was successfully queued for a put
 * @retval OPAL_ERROR      The descriptor was NOT successfully queued for a put
 * @retval OPAL_ERR_OUT_OF_RESOURCE  Insufficient resources to queue the put
 *                         operation. Try again later
 * @retval OPAL_ERR_NOT_AVAILABLE  Put can not be performed due to size or
 *                         alignment restrictions.
 */
int mca_btl_ofi_put(struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                    void *local_address, uint64_t remote_address,
                    struct mca_btl_base_registration_handle_t *local_handle,
                    struct mca_btl_base_registration_handle_t *remote_handle, size_t size,
                    int flags, int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext,
                    void *cbdata);

/**
 * Initiate an asynchronous get.
 * Completion Semantics: if this function returns a 1 then the operation
 *                       is complete. a return of OPAL_SUCCESS indicates
 *                       the get operation has been queued with the
 *                       network. the local_handle can not be deregistered
 *                       until all outstanding operations on that handle
 *                       have been completed.
 *
 * @param btl (IN)            BTL module
 * @param endpoint (IN)       BTL addressing information
 * @param local_address (IN)  Local address to put from (registered)
 * @param remote_address (IN) Remote address to put to (registered remotely)
 * @param local_handle (IN)   Registration handle for region containing
 *                            (local_address, local_address + size)
 * @param remote_handle (IN)  Remote registration handle for region containing
 *                            (remote_address, remote_address + size)
 * @param size (IN)           Number of bytes to put
 * @param flags (IN)          Flags for this put operation
 * @param order (IN)          Ordering
 * @param cbfunc (IN)         Function to call on completion (if queued)
 * @param cbcontext (IN)      Context for the callback
 * @param cbdata (IN)         Data for callback
 *
 * @retval OPAL_SUCCESS    The descriptor was successfully queued for a put
 * @retval OPAL_ERROR      The descriptor was NOT successfully queued for a put
 * @retval OPAL_ERR_OUT_OF_RESOURCE  Insufficient resources to queue the put
 *                         operation. Try again later
 * @retval OPAL_ERR_NOT_AVAILABLE  Put can not be performed due to size or
 *                         alignment restrictions.
 */
int mca_btl_ofi_get(struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                    void *local_address, uint64_t remote_address,
                    struct mca_btl_base_registration_handle_t *local_handle,
                    struct mca_btl_base_registration_handle_t *remote_handle, size_t size,
                    int flags, int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext,
                    void *cbdata);

int mca_btl_ofi_aop(struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                    uint64_t remote_address, mca_btl_base_registration_handle_t *remote_handle,
                    mca_btl_base_atomic_op_t op, uint64_t operand, int flags, int order,
                    mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata);

int mca_btl_ofi_afop(struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                     void *local_address, uint64_t remote_address,
                     mca_btl_base_registration_handle_t *local_handle,
                     mca_btl_base_registration_handle_t *remote_handle, mca_btl_base_atomic_op_t op,
                     uint64_t operand, int flags, int order,
                     mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata);

int mca_btl_ofi_acswap(struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                       void *local_address, uint64_t remote_address,
                       mca_btl_base_registration_handle_t *local_handle,
                       mca_btl_base_registration_handle_t *remote_handle, uint64_t compare,
                       uint64_t value, int flags, int order,
                       mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata);

int mca_btl_ofi_flush(struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint);

int mca_btl_ofi_finalize(mca_btl_base_module_t *btl);

void mca_btl_ofi_rcache_init(mca_btl_ofi_module_t *module);
int mca_btl_ofi_reg_mem(void *reg_data, void *base, size_t size,
                        mca_rcache_base_registration_t *reg);
int mca_btl_ofi_dereg_mem(void *reg_data, mca_rcache_base_registration_t *reg);

int mca_btl_ofi_context_progress(mca_btl_ofi_context_t *context);

mca_btl_ofi_module_t *mca_btl_ofi_module_alloc(int mode);

int mca_btl_ofi_post_recvs(mca_btl_base_module_t *module, mca_btl_ofi_context_t *context,
                           int count);
void mca_btl_ofi_exit(void);

/* thread atomics */
static inline bool mca_btl_ofi_context_trylock(mca_btl_ofi_context_t *context)
{
    return (context->lock || OPAL_ATOMIC_SWAP_32(&context->lock, 1));
}

static inline void mca_btl_ofi_context_lock(mca_btl_ofi_context_t *context)
{
    while (mca_btl_ofi_context_trylock(context))
        ;
}

static inline void mca_btl_ofi_context_unlock(mca_btl_ofi_context_t *context)
{
    opal_atomic_mb();
    context->lock = 0;
}

END_C_DECLS
#endif
