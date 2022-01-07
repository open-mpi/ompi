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
 * Copyright (c) 2006-2007 Voltaire. All rights reserved.
 * Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015      Mellanox Technologies. All rights reserved.
 * Copyright (c) 2018      Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2020      Google, LLC. All rights reserved.
 *
 * Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_BTL_SM_H
#define MCA_BTL_SM_H

#include "opal_config.h"

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include <stdint.h>
#ifdef HAVE_SCHED_H
#    include <sched.h>
#endif /* HAVE_SCHED_H */
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif /* HAVE_UNISTD_H */

#include "opal/mca/shmem/base/base.h"

#include "opal/mca/btl/base/base.h"
#include "opal/mca/btl/base/btl_base_error.h"
#include "opal/mca/btl/btl.h"
#include "opal/mca/btl/sm/btl_sm_types.h"
#include "opal/mca/mpool/base/base.h"
#include "opal/mca/rcache/base/base.h"
#include "opal/mca/rcache/base/rcache_base_vma.h"
#include "opal/mca/rcache/rcache.h"
#include "opal/sys/atomic.h"
#include "opal/util/proc.h"

#include "opal/mca/pmix/pmix-internal.h"

BEGIN_C_DECLS

#define min(a, b) ((a) < (b) ? (a) : (b))

/*
 * Shared Memory resource managment
 */

struct sm_fifo_t;

OPAL_MODULE_DECLSPEC extern mca_btl_sm_component_t mca_btl_sm_component;
OPAL_MODULE_DECLSPEC extern mca_btl_sm_t mca_btl_sm;

/* number of peers on the node (not including self) */
#define MCA_BTL_SM_NUM_LOCAL_PEERS opal_process_info.num_local_peers

/* local rank in the group */
#define MCA_BTL_SM_LOCAL_RANK opal_process_info.my_local_rank

/* memcpy is faster at larger sizes but is undefined if the
   pointers are aliased (TODO -- readd alias check) */
static inline void sm_memmove(void *dst, void *src, size_t size)
{
    if (size >= (size_t) mca_btl_sm_component.memcpy_limit) {
        memcpy(dst, src, size);
    } else {
        memmove(dst, src, size);
    }
}

/**
 * Initiate a send to the peer.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */
int mca_btl_sm_send(struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                    struct mca_btl_base_descriptor_t *descriptor, mca_btl_base_tag_t tag);

/**
 * Initiate an inline send to the peer.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */
int mca_btl_sm_sendi(struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                     struct opal_convertor_t *convertor, void *header, size_t header_size,
                     size_t payload_size, uint8_t order, uint32_t flags, mca_btl_base_tag_t tag,
                     mca_btl_base_descriptor_t **descriptor);

/**
 * Initiate an synchronous put.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */
int mca_btl_sm_put(mca_btl_base_module_t *btl, mca_btl_base_endpoint_t *endpoint,
                   void *local_address, uint64_t remote_address,
                   mca_btl_base_registration_handle_t *local_handle,
                   mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
                   int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext,
                   void *cbdata);

/**
 * Initiate an synchronous get.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */
int mca_btl_sm_get(mca_btl_base_module_t *btl, mca_btl_base_endpoint_t *endpoint,
                   void *local_address, uint64_t remote_address,
                   mca_btl_base_registration_handle_t *local_handle,
                   mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
                   int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext,
                   void *cbdata);

/**
 * Allocate a segment.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 */
mca_btl_base_descriptor_t *mca_btl_sm_alloc(struct mca_btl_base_module_t *btl,
                                            struct mca_btl_base_endpoint_t *endpoint, uint8_t order,
                                            size_t size, uint32_t flags);

/**
 * Return a segment allocated by this BTL.
 *
 * @param btl (IN)      BTL module
 * @param segment (IN)  Allocated segment.
 */
int mca_btl_sm_free(struct mca_btl_base_module_t *btl, mca_btl_base_descriptor_t *des);

static inline bool mca_btl_is_self_endpoint(mca_btl_base_endpoint_t *endpoint) {
    return endpoint->peer_smp_rank == MCA_BTL_SM_LOCAL_RANK;
}

END_C_DECLS

#endif
