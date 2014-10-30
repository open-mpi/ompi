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

#include "opal_config.h"

#include "btl_vader.h"
#include "btl_vader_frag.h"
#include "btl_vader_endpoint.h"
#include "btl_vader_xpmem.h"

#if OPAL_BTL_VADER_HAVE_CMA
#include <sys/uio.h>

#if OPAL_CMA_NEED_SYSCALL_DEFS
#include "opal/sys/cma.h"
#endif /* OPAL_CMA_NEED_SYSCALL_DEFS */

#endif

/**
 * Initiate an synchronous get.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */
#if OPAL_BTL_VADER_HAVE_XPMEM
int mca_btl_vader_get_xpmem (mca_btl_base_module_t *btl, mca_btl_base_endpoint_t *endpoint, void *local_address,
                             uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
                             mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
                             int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata)
{
    mca_mpool_base_registration_t *reg;
    void *rem_ptr;

    /* silence warning about unused arguments */
    (void) local_handle;
    (void) remote_handle;

    reg = vader_get_registation (endpoint, (void *)(intptr_t) remote_address, size, 0, &rem_ptr);
    if (OPAL_UNLIKELY(NULL == rem_ptr)) {
        return OPAL_ERROR;
    }

    vader_memmove (local_address, rem_ptr, size);

    vader_return_registration (reg, endpoint);

    /* always call the callback function */
    cbfunc (btl, endpoint, local_address, local_handle, cbcontext, cbdata, OPAL_SUCCESS);

    return OPAL_SUCCESS;
}
#endif

#if OPAL_BTL_VADER_HAVE_CMA
int mca_btl_vader_get_cma (mca_btl_base_module_t *btl, mca_btl_base_endpoint_t *endpoint, void *local_address,
                           uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
                           mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
                           int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata)
{
    struct iovec src_iov = {.iov_base = (void *)(intptr_t) remote_address, .iov_len = size};
    struct iovec dst_iov = {.iov_base = local_address, .iov_len = size};
    ssize_t ret;

    ret = process_vm_readv (endpoint->segment_data.other.seg_ds->seg_cpid, &dst_iov, 1, &src_iov, 1, 0);
    if (ret != (ssize_t)size) {
        opal_output(0, "Read %ld, expected %lu, errno = %d\n", (long)ret, (unsigned long)size, errno);
        return OPAL_ERROR;
    }

    /* always call the callback function */
    cbfunc (btl, endpoint, local_address, local_handle, cbcontext, cbdata, OPAL_SUCCESS);

    return OPAL_SUCCESS;
}
#endif

#if OPAL_BTL_VADER_HAVE_KNEM
int mca_btl_vader_get_knem (mca_btl_base_module_t *btl, mca_btl_base_endpoint_t *endpoint, void *local_address,
                            uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
                            mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
                            int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata)
{
    struct knem_cmd_param_iovec recv_iovec;
    struct knem_cmd_inline_copy icopy;

    /* Fill in the ioctl data fields.  There's no async completion, so
       we don't need to worry about getting a slot, etc. */
    recv_iovec.base = (uintptr_t) local_address;
    recv_iovec.len = size;
    icopy.local_iovec_array = (uintptr_t) &recv_iovec;
    icopy.local_iovec_nr    = 1;
    icopy.remote_cookie     = remote_handle->cookie;
    icopy.remote_offset     = remote_address - remote_handle->base_addr;
    icopy.write             = 0;
    icopy.flags             = 0;

    /* Use the DMA flag if knem supports it *and* the segment length
     * is greater than the cutoff. Not that if DMA is not supported
     * or the user specified 0 for knem_dma_min the knem_dma_min was
     * set to UINT_MAX in mca_btl_vader_knem_init. */
    if (mca_btl_vader_component.knem_dma_min <= size) {
        icopy.flags = KNEM_FLAG_DMA;
    }
    /* synchronous flags only, no need to specify icopy.async_status_index */

    /* When the ioctl returns, the transfer is done and we can invoke
       the btl callback and return the frag */
    if (OPAL_UNLIKELY(0 != ioctl (mca_btl_vader.knem_fd, KNEM_CMD_INLINE_COPY, &icopy))) {
        return OPAL_ERROR;
    }

    if (KNEM_STATUS_FAILED == icopy.current_status) {
        return OPAL_ERROR;
    }

    /* always call the callback function */
    cbfunc (btl, endpoint, local_address, local_handle, cbcontext, cbdata, OPAL_SUCCESS);

    return OPAL_SUCCESS;
}
#endif
