/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2013-2014 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2016      ARM, Inc. All rights reserved.
 * Copyright (c) 2020      Google, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BTL_SM_XPMEM_H
#define MCA_BTL_SM_XPMEM_H

#include "opal_config.h"
#include "opal/mca/btl/sm/btl_sm_types.h"
#include "opal/mca/rcache/base/rcache_base_vma.h"
#include "opal/mca/rcache/rcache.h"

#if OPAL_BTL_SM_HAVE_XPMEM

/* look up the remote pointer in the peer rcache and attach if
 * necessary */

struct mca_btl_base_endpoint_t;

int mca_btl_sm_xpmem_init(void);

mca_rcache_base_registration_t *sm_get_registation(struct mca_btl_base_endpoint_t *endpoint,
                                                   void *rem_ptr, size_t size, int flags,
                                                   void **local_ptr);

void sm_return_registration(mca_rcache_base_registration_t *reg,
                            struct mca_btl_base_endpoint_t *endpoint);
void mca_btl_sm_xpmem_cleanup_endpoint(struct mca_btl_base_endpoint_t *ep);

#endif /* OPAL_BTL_SM_HAVE_XPMEM */

#endif /* MCA_BTL_SM_XPMEM_H */
