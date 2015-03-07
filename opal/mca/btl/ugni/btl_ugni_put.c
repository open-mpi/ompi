/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "opal_config.h"
#include "opal/include/opal_stdint.h"

#include "btl_ugni_rdma.h"

int mca_btl_ugni_put (mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint, void *local_address,
                      uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
                      mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
                      int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata)
{
    BTL_VERBOSE(("Using RDMA/FMA Put from local address %p to remote address %" PRIx64,
                 local_address, remote_address));

    /* cause endpoint to bind if it isn't already (bind is sufficient for rdma) */
    (void) mca_btl_ugni_check_endpoint_state_rdma (endpoint);

    return mca_btl_ugni_post (endpoint, false, size, local_address, remote_address, local_handle,
                              remote_handle, order, cbfunc, cbcontext, cbdata);
}
