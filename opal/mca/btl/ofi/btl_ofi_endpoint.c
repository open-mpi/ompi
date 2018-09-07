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

#include "btl_ofi.h"
#include "btl_ofi_endpoint.h"
#include "opal/util/proc.h"

static void mca_btl_ofi_endpoint_construct (mca_btl_ofi_endpoint_t *endpoint)
{
    endpoint->peer_addr = 0;
    OBJ_CONSTRUCT(&endpoint->ep_lock, opal_mutex_t);
}

static void mca_btl_ofi_endpoint_destruct (mca_btl_ofi_endpoint_t *endpoint)
{
    endpoint->peer_addr = 0;

    /* set to null, we will free ofi endpoint in module */
    endpoint->ofi_endpoint = NULL;

    OBJ_DESTRUCT(&endpoint->ep_lock);
}

OBJ_CLASS_INSTANCE(mca_btl_ofi_endpoint_t, opal_list_item_t,
                   mca_btl_ofi_endpoint_construct,
                   mca_btl_ofi_endpoint_destruct);

mca_btl_base_endpoint_t *mca_btl_ofi_endpoint_create (opal_proc_t *proc, struct fid_ep *ep)
{
    mca_btl_ofi_endpoint_t *endpoint = OBJ_NEW(mca_btl_ofi_endpoint_t);

    if (OPAL_UNLIKELY(NULL == endpoint)) {
        return NULL;
    }

    endpoint->ep_proc = proc;
    endpoint->ofi_endpoint = ep;

    return (mca_btl_base_endpoint_t *) endpoint;
}
