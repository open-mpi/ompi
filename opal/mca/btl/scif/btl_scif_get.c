/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2013-2014 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include "btl_scif_frag.h"

#include <sys/time.h>

#define lmin(a,b) ((a) < (b) ? (a) : (b))

/**
 * Initiate a get operation.
 */
int mca_btl_scif_get (mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint, void *local_address,
                      uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
                      mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
                      int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata)
{
    int rc, mark, scif_flags = 0;
    off_t roffset, loffset;
#if defined(SCIF_TIMING)
    struct timespec ts;

    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts);

    mca_btl_scif_component.get_count++;
#endif

    BTL_VERBOSE(("Using DMA Get from remote address %" PRIx64 " to local address %p",
                 remote_address, local_address));

    roffset = remote_handle->scif_offset + (off_t)(remote_address - remote_handle->scif_base);
    loffset = local_handle->scif_offset + (off_t)((intptr_t)local_address - local_handle->scif_base);
        
    if (mca_btl_scif_component.rma_use_cpu) {
        scif_flags = SCIF_RMA_USECPU;
    }

    if (mca_btl_scif_component.rma_sync) {
        scif_flags |= SCIF_RMA_SYNC;
    }

    /* start the read */
    rc = scif_readfrom (endpoint->scif_epd, loffset, size, roffset, scif_flags);
    if (OPAL_UNLIKELY(-1 == rc)) {
        return OPAL_ERROR;
    }

    if (!(scif_flags & SCIF_RMA_SYNC)) {
        /* according to the scif documentation is is better to use a fence rather
         * than using the SCIF_RMA_SYNC flag with scif_readfrom */
        scif_fence_mark (endpoint->scif_epd, SCIF_FENCE_INIT_SELF, &mark);
        scif_fence_wait (endpoint->scif_epd, mark);
    }

#if defined(SCIF_TIMING)
    SCIF_UPDATE_TIMER(mca_btl_scif_component.get_time,
                      mca_btl_scif_component.get_time_max, ts);
#endif

    /* always call the callback function */
    cbfunc (btl, endpoint, local_address, local_handle, cbcontext, cbdata, OPAL_SUCCESS);

    return OPAL_SUCCESS;
}
