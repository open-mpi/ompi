/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2015-2018 Los Alamos National Security, LLC. All rights
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
#ifndef MCA_BTL_UCT_H
#define MCA_BTL_UCT_H

#include "opal_config.h"
#include <sys/types.h>
#include <string.h>

/* Open MPI includes */
#include "opal/mca/event/event.h"
#include "opal/mca/btl/base/base.h"
#include "opal/mca/mpool/mpool.h"
#include "opal/mca/btl/base/btl_base_error.h"
#include "opal/mca/rcache/base/base.h"
#include "opal/class/opal_fifo.h"
#include "opal/class/opal_hash_table.h"
#include "opal/mca/pmix/pmix.h"
#include "opal/threads/tsd.h"
#include <uct/api/uct.h>

#include "btl_uct_types.h"

BEGIN_C_DECLS

/* detection for old vs new atomic flags */
#if defined(UCT_IFACE_FLAG_ATOMIC_ADD32)
#define OPAL_HAVE_UCT_EP_ATOMIC64_POST 0
#else
#define OPAL_HAVE_UCT_EP_ATOMIC64_POST 1
#endif

/**
 * @brief UCT BTL module
 */
struct mca_btl_uct_module_t {
    /** base BTL interface */
    mca_btl_base_module_t super;

    /** whether the module has been fully initialized or not */
    bool initialized;

    /** lock for the hash table */
    opal_mutex_t endpoint_lock;

    /** endpoint hash table */
    opal_hash_table_t id_to_endpoint;

    /** mutex to protect the module */
    opal_recursive_mutex_t lock;

    /** async context */
    ucs_async_context_t *ucs_async;

    /** transport for active messaging */
    mca_btl_uct_tl_t *am_tl;

    /** transport for RDMA/AMOs */
    mca_btl_uct_tl_t *rdma_tl;

    /** transport for forming connections (if needed) */
    mca_btl_uct_tl_t *conn_tl;

    /** array containing the am_tl and rdma_tl */
    mca_btl_uct_tl_t *comm_tls[2];

    /** registration cache */
    mca_rcache_base_module_t *rcache;

    /** name of the memory domain backing this module */
    char *md_name;

    /** am and rdma share endpoints */
    bool shared_endpoints;

    /** memory domain */
    mca_btl_uct_md_t *md;

    /** un-registered frags that will be used with uct_ep_am_short() */
    opal_free_list_t short_frags;

    /** registered frags that will be used with uct_ep_am_zcopy() */
    opal_free_list_t eager_frags;

    /** large registered frags for packing non-contiguous data */
    opal_free_list_t max_frags;

    /** frags that were waiting on connections that are now ready to send */
    opal_list_t pending_frags;

    /** pending connection requests */
    opal_fifo_t pending_connection_reqs;
};
typedef struct mca_btl_uct_module_t mca_btl_uct_module_t;

extern mca_btl_uct_module_t mca_btl_uct_module_template;

/**
 * @brief UCT BTL component
 */
struct mca_btl_uct_component_t {
    /** base BTL component */
    mca_btl_base_component_3_0_0_t super;

    /** number of TL modules */
    int module_count;

    /** All BTL UCT modules (1 per memory domain) */
    mca_btl_uct_module_t *modules[MCA_BTL_UCT_MAX_MODULES];

    /** allowed UCT memory domains */
    char *memory_domains;

    /** allowed transports */
    char *allowed_transports;

    /** number of worker contexts to create */
    int num_contexts_per_module;

#if OPAL_C_HAVE__THREAD_LOCAL
    /** bind threads to contexts */
    bool bind_threads_to_contexts;
#endif

    /** disable UCX memory hooks */
    bool disable_ucx_memory_hooks;
};
typedef struct mca_btl_uct_component_t mca_btl_uct_component_t;

OPAL_MODULE_DECLSPEC extern mca_btl_uct_component_t mca_btl_uct_component;

struct mca_btl_base_registration_handle_t {
    /** The packed memory handle. The size of this field is defined by UCT. */
    uint8_t packed_handle[1];
};

struct mca_btl_uct_reg_t {
    mca_rcache_base_registration_t base;

    /** UCT memory handle */
    uct_mem_h uct_memh;

    /** remote handle */
    mca_btl_base_registration_handle_t handle;
};
typedef struct mca_btl_uct_reg_t mca_btl_uct_reg_t;

OBJ_CLASS_DECLARATION(mca_btl_uct_reg_t);

#define MCA_BTL_UCT_REG_REMOTE_TO_LOCAL(reg) ((mca_btl_uct_reg_t *)((intptr_t) (reg) - offsetof (mca_btl_uct_reg_t, handle)))

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
int mca_btl_uct_put (struct mca_btl_base_module_t *btl,
    struct mca_btl_base_endpoint_t *endpoint, void *local_address,
    uint64_t remote_address, struct mca_btl_base_registration_handle_t *local_handle,
    struct mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
    int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata);

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
int mca_btl_uct_get (struct mca_btl_base_module_t *btl,
    struct mca_btl_base_endpoint_t *endpoint, void *local_address,
    uint64_t remote_address, struct mca_btl_base_registration_handle_t *local_handle,
    struct mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
    int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata);

 /**
  * Fault Tolerance Event Notification Function
  * @param state Checkpoint Stae
  * @return OPAL_SUCCESS or failure status
  */
int mca_btl_uct_ft_event(int state);

int mca_btl_uct_aop (struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                     uint64_t remote_address, mca_btl_base_registration_handle_t *remote_handle,
                     mca_btl_base_atomic_op_t op, uint64_t operand, int flags, int order,
                     mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata);

int mca_btl_uct_afop (struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                      void *local_address, uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
                      mca_btl_base_registration_handle_t *remote_handle, mca_btl_base_atomic_op_t op,
                      uint64_t operand, int flags, int order, mca_btl_base_rdma_completion_fn_t cbfunc,
                      void *cbcontext, void *cbdata);

int mca_btl_uct_acswap (struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                        void *local_address, uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
                        mca_btl_base_registration_handle_t *remote_handle, uint64_t compare, uint64_t value, int flags,
                        int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata);


int mca_btl_uct_flush (struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint);
int mca_btl_uct_flush_thread (mca_btl_base_module_t *btl);

int mca_btl_uct_finalize (mca_btl_base_module_t *btl);

int mca_btl_uct_reg_mem (void *reg_data, void *base, size_t size, mca_rcache_base_registration_t *reg);
int mca_btl_uct_dereg_mem (void *reg_data, mca_rcache_base_registration_t *reg);

ucs_status_t mca_btl_uct_am_handler (void *arg, void *data, size_t length, unsigned flags);

struct mca_btl_base_endpoint_t *mca_btl_uct_get_ep (struct mca_btl_base_module_t *module, opal_proc_t *proc);

int mca_btl_uct_query_tls (mca_btl_uct_module_t *module, mca_btl_uct_md_t *md, uct_tl_resource_desc_t *tl_descs, unsigned tl_count);
int mca_btl_uct_process_connection_request (mca_btl_uct_module_t *module, mca_btl_uct_conn_req_t *req);

/**
 * @brief Checks if a tl is suitable for using for RDMA
 *
 * @param[in] tl  btl/uct tl pointer
 */
static inline bool mca_btl_uct_tl_supports_rdma (mca_btl_uct_tl_t *tl)
{
    return (MCA_BTL_UCT_TL_ATTR(tl, 0).cap.flags & (UCT_IFACE_FLAG_PUT_ZCOPY | UCT_IFACE_FLAG_GET_ZCOPY)) ==
        (UCT_IFACE_FLAG_PUT_ZCOPY | UCT_IFACE_FLAG_GET_ZCOPY);
}

/**
 * @brief Checks if a tl is suitable for using for active messaging
 */
static inline bool mca_btl_uct_tl_support_am (mca_btl_uct_tl_t *tl)
{
    return (MCA_BTL_UCT_TL_ATTR(tl, 0).cap.flags & (UCT_IFACE_FLAG_AM_SHORT | UCT_IFACE_FLAG_AM_BCOPY | UCT_IFACE_FLAG_AM_ZCOPY));
}

/**
 * @brief Checks if a tl can be used for passing data to connect endpoints
 *
 * @param[in] tl  btl/uct tl pointer
 */
static inline bool mca_btl_uct_tl_supports_conn (mca_btl_uct_tl_t *tl)
{
    return (MCA_BTL_UCT_TL_ATTR(tl, 0).cap.flags & (UCT_IFACE_FLAG_AM_SHORT | UCT_IFACE_FLAG_CONNECT_TO_IFACE)) ==
        (UCT_IFACE_FLAG_AM_SHORT | UCT_IFACE_FLAG_CONNECT_TO_IFACE);
}

/**
 * @brief Check if tl endpoints need to be connected via a connection tl
 *
 * @param[in] tl  btl/uct tl pointer
 */
static inline bool mca_btl_uct_tl_requires_connection_tl (mca_btl_uct_tl_t *tl)
{
    return !(MCA_BTL_UCT_TL_ATTR(tl, 0).cap.flags & UCT_IFACE_FLAG_CONNECT_TO_IFACE);
}

END_C_DECLS
#endif
