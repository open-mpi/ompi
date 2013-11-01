/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "btl_scif_frag.h"

#define lmin(a,b) ((a) < (b) ? (a) : (b))

/**
 * Initiate a put operation.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */
int mca_btl_scif_put (struct mca_btl_base_module_t *btl,
                      struct mca_btl_base_endpoint_t *endpoint,
                      struct mca_btl_base_descriptor_t *des) {
    mca_btl_scif_segment_t *src = (mca_btl_scif_segment_t *) des->des_src;
    mca_btl_scif_segment_t *dst = (mca_btl_scif_segment_t *) des->des_dst;
    size_t len = lmin (src->base.seg_len, dst->base.seg_len);
    int rc, mark, flags = 0;
    off_t roffset, loffset;
#if defined(SCIF_TIMING)
    struct timespec ts;

    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts);

    mca_btl_scif_component.put_count++;
#endif

    BTL_VERBOSE(("Using DMA Put for frag %p", (void *) des));

    roffset = dst->scif_offset + (off_t)(dst->orig_ptr - dst->base.seg_addr.lval);
    loffset = src->scif_offset + (off_t)(src->orig_ptr - src->base.seg_addr.lval);

    if (mca_btl_scif_component.rma_use_cpu) {
        flags = SCIF_RMA_USECPU;
    }

    if (mca_btl_scif_component.rma_sync) {
        flags |= SCIF_RMA_SYNC;
    }

    /* start the write */
    rc = scif_writeto (endpoint->scif_epd, loffset, len, roffset, flags);
    if (OPAL_UNLIKELY(-1 == rc)) {
        return OMPI_ERROR;
    }

    /* always call the callback function */
    des->des_flags |= MCA_BTL_DES_SEND_ALWAYS_CALLBACK;

    /* according to the scif documentation is is better to use a fence rather
     * than using the SCIF_RMA_SYNC flag with scif_writeto */
    if (!(flags & SCIF_RMA_SYNC)) {
        scif_fence_mark (endpoint->scif_epd, SCIF_FENCE_INIT_SELF, &mark);
        scif_fence_wait (endpoint->scif_epd, mark);
    }

#if defined(SCIF_TIMING)
    SCIF_UPDATE_TIMER(mca_btl_scif_component.put_time,
                      mca_btl_scif_component.put_time_max, ts);
#endif

    /* since we completed the fence the RMA operation is complete */
    mca_btl_scif_frag_complete ((mca_btl_scif_base_frag_t *) des, OMPI_SUCCESS);

    return OMPI_SUCCESS;
}
