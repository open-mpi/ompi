/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2010-2014 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "btl_vader.h"
#include "btl_vader_frag.h"
#include "btl_vader_endpoint.h"
#include "btl_vader_xpmem.h"

#if OMPI_BTL_VADER_HAVE_CMA
#include <sys/uio.h>

#if OMPI_CMA_NEED_SYSCALL_DEFS
#include "opal/sys/cma.h"
#endif /* OMPI_CMA_NEED_SYSCALL_DEFS */

#endif

/**
 * Initiate an synchronous put.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */
#if OMPI_BTL_VADER_HAVE_XPMEM
int mca_btl_vader_put_xpmem (struct mca_btl_base_module_t *btl,
                             struct mca_btl_base_endpoint_t *endpoint,
                             struct mca_btl_base_descriptor_t *des)
{
    mca_btl_vader_frag_t *frag = (mca_btl_vader_frag_t *) des;
    mca_btl_base_segment_t *src = des->des_src;
    mca_btl_base_segment_t *dst = des->des_dst;
    const size_t size = min(dst->seg_len, src->seg_len);
    mca_mpool_base_registration_t *reg;
    void *rem_ptr;

    reg = vader_get_registation (endpoint, dst->seg_addr.pval, dst->seg_len, 0, &rem_ptr);
    if (OPAL_UNLIKELY(NULL == reg)) {
        return OMPI_ERROR;
    }

    vader_memmove (rem_ptr, src->seg_addr.pval, size);

    vader_return_registration (reg, endpoint);

    /* always call the callback function */
    frag->base.des_flags |= MCA_BTL_DES_SEND_ALWAYS_CALLBACK;

    frag->endpoint = endpoint;
    mca_btl_vader_frag_complete (frag);

    return OMPI_SUCCESS;
}
#endif

#if OMPI_BTL_VADER_HAVE_CMA
int mca_btl_vader_put_cma (struct mca_btl_base_module_t *btl,
                           struct mca_btl_base_endpoint_t *endpoint,
                           struct mca_btl_base_descriptor_t *des)
{
    mca_btl_vader_frag_t *frag = (mca_btl_vader_frag_t *) des;
    mca_btl_base_segment_t *src = des->des_src;
    mca_btl_base_segment_t *dst = des->des_dst;
    const size_t size = min(dst->seg_len, src->seg_len);
    struct iovec src_iov = {.iov_base = src->seg_addr.pval, .iov_len = size};
    struct iovec dst_iov = {.iov_base = dst->seg_addr.pval, .iov_len = size};
    ssize_t ret;

    ret = process_vm_writev (endpoint->segment_data.other.seg_ds->seg_cpid, &src_iov, 1, &dst_iov, 1, 0);
    if (ret != (ssize_t)size) {
        opal_output(0, "Wrote %ld, expected %lu, errno = %d\n", (long)ret, (unsigned long)size, errno);
        return OMPI_ERROR;
    }

    /* always call the callback function */
    frag->base.des_flags |= MCA_BTL_DES_SEND_ALWAYS_CALLBACK;

    frag->endpoint = endpoint;
    mca_btl_vader_frag_complete (frag);

    return OMPI_SUCCESS;
}
#endif

#if OMPI_BTL_VADER_HAVE_KNEM
int mca_btl_vader_put_knem (struct mca_btl_base_module_t *btl,
                            struct mca_btl_base_endpoint_t *endpoint,
                            struct mca_btl_base_descriptor_t *des)
{
    mca_btl_vader_frag_t *frag = (mca_btl_vader_frag_t *) des;
    mca_btl_vader_segment_t *src = (mca_btl_vader_segment_t *) des->des_src;
    mca_btl_vader_segment_t *dst = (mca_btl_vader_segment_t *) des->des_dst;
    const size_t size = min(dst->base.seg_len, src->base.seg_len);
    struct knem_cmd_param_iovec send_iovec;
    struct knem_cmd_inline_copy icopy;

    /* Fill in the ioctl data fields.  There's no async completion, so
       we don't need to worry about getting a slot, etc. */
    send_iovec.base = (uintptr_t) dst->base.seg_addr.lval;
    send_iovec.len = size;
    icopy.local_iovec_array = (uintptr_t) &send_iovec;
    icopy.local_iovec_nr    = 1;
    icopy.remote_cookie     = dst->cookie;
    icopy.remote_offset     = 0;
    icopy.write             = 1;
    icopy.flags             = 0;

    /* Use the DMA flag if knem supports it *and* the segment length
     * is greater than the cutoff. Not that if DMA is not supported
     * or the user specified 0 for knem_dma_min the knem_dma_min was
     * set to UINT_MAX in mca_btl_vader_knem_init. */
    if (mca_btl_vader_component.knem_dma_min <= dst->base.seg_len) {
        icopy.flags = KNEM_FLAG_DMA;
    }
    /* synchronous flags only, no need to specify icopy.async_status_index */

    /* When the ioctl returns, the transfer is done and we can invoke
       the btl callback and return the frag */
    if (OPAL_UNLIKELY(0 != ioctl (mca_btl_vader.knem_fd, KNEM_CMD_INLINE_COPY, &icopy))) {
        return OMPI_ERROR;
    }

    if (KNEM_STATUS_FAILED == icopy.current_status) {
        return OMPI_ERROR;
    }

    /* always call the callback function */
    frag->base.des_flags |= MCA_BTL_DES_SEND_ALWAYS_CALLBACK;

    frag->endpoint = endpoint;
    mca_btl_vader_frag_complete (frag);

    return OMPI_SUCCESS;
}
#endif
