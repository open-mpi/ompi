/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2010-2013 Los Alamos National Security, LLC.  
 *                         All rights reserved. 
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
 * Initiate an synchronous get.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */
#if OMPI_BTL_VADER_HAVE_XPMEM
int mca_btl_vader_get (struct mca_btl_base_module_t *btl,
                       struct mca_btl_base_endpoint_t *endpoint,
                       struct mca_btl_base_descriptor_t *des)
{
    mca_btl_vader_frag_t *frag = (mca_btl_vader_frag_t *) des;
    mca_btl_base_segment_t *src = des->des_src;
    mca_btl_base_segment_t *dst = des->des_dst;
    const size_t size = min(dst->seg_len, src->seg_len);
    mca_mpool_base_registration_t *reg;
    void *rem_ptr;

    reg = vader_get_registation (endpoint, src->seg_addr.pval, src->seg_len, 0, &rem_ptr);
    if (OPAL_UNLIKELY(NULL == rem_ptr)) {
        return OMPI_ERROR;
    }

    vader_memmove (dst->seg_addr.pval, rem_ptr, size);

    vader_return_registration (reg, endpoint);

    mca_btl_vader_frag_complete (frag);

    return OMPI_SUCCESS;
}
#elif OMPI_BTL_VADER_HAVE_CMA
int mca_btl_vader_get (struct mca_btl_base_module_t *btl,
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

    ret = process_vm_readv (endpoint->seg_ds.seg_cpid, &dst_iov, 1, &src_iov, 1, 0);
    if (ret != size) {
        fprintf (stderr, "Read %d, expected %u, errno = %d\n", ret, size, errno);
        return OMPI_ERROR;
    }

    mca_btl_vader_frag_complete (frag);

    return OMPI_SUCCESS;
}
#endif
