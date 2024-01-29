/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2020-2021 Google, LLC. All rights reserved.
 * Copyright (c) 2021      Nanook Consulting.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/include/opal/align.h"
#include "opal/mca/memchecker/base/base.h"
#include "opal/mca/pmix/pmix-internal.h"
#include "opal/mca/rcache/rcache.h"
#include "opal/mca/smsc/base/base.h"
#include "opal/mca/smsc/knem/smsc_knem_internal.h"
#include "opal/util/minmax.h"

OBJ_CLASS_INSTANCE(mca_smsc_knem_endpoint_t, opal_object_t, NULL, NULL);

mca_smsc_endpoint_t *mca_smsc_knem_get_endpoint(opal_proc_t *peer_proc)
{
    mca_smsc_knem_endpoint_t *endpoint = OBJ_NEW(mca_smsc_knem_endpoint_t);
    if (OPAL_UNLIKELY(NULL == endpoint)) {
        return NULL;
    }

    endpoint->super.proc = peer_proc;
    return &endpoint->super;
}

void mca_smsc_knem_return_endpoint(mca_smsc_endpoint_t *endpoint)
{
    OBJ_RELEASE(endpoint);
}

void *mca_smsc_knem_register_region(void *local_address, size_t size)
{
    mca_smsc_knem_module_t *knem_module = &mca_smsc_knem_module;
    mca_smsc_knem_registration_handle_t *reg = NULL;
    int rc;

    rc = knem_module->rcache->rcache_register(knem_module->rcache, local_address, size,
                                              /*flags=*/0, MCA_RCACHE_ACCESS_ANY,
                                              (mca_rcache_base_registration_t **) &reg);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        opal_output_verbose(
            MCA_BASE_VERBOSE_WARN, opal_smsc_base_framework.framework_output,
            "mca_smsc_knem_register_mem: failed to register memory for single-copy");
        return NULL;
    }

    return MCA_SMSC_KNEM_REG_HANDLE_TO_DATA(reg);
}

void mca_smsc_knem_deregister_region(void *reg_data)
{
    mca_smsc_knem_module_t *knem_module = &mca_smsc_knem_module;
    mca_smsc_knem_registration_handle_t *reg = MCA_SMSC_KNEM_REG_DATA_TO_HANDLE(reg_data);

    knem_module->rcache->rcache_deregister(knem_module->rcache, &reg->base);
}

static int mca_smsc_knem_module_copy(mca_smsc_endpoint_t *endpoint, void *local_address,
                                     void *remote_address, size_t size, void *reg_data,
                                     bool is_write)
{
    if (OPAL_UNLIKELY(NULL == reg_data)) {
        return OPAL_ERR_BAD_PARAM;
    }

    struct knem_cmd_param_iovec send_iovec = {
        .base = (uintptr_t) local_address,
        .len = size,
    };
    mca_smsc_knem_registration_data_t *reg = (mca_smsc_knem_registration_data_t *) reg_data;
    /* Fill in the ioctl data fields.  There's no async completion, so
       we don't need to worry about getting a slot, etc. */
    struct knem_cmd_inline_copy icopy = {
        .local_iovec_array = (uintptr_t) &send_iovec,
        .local_iovec_nr = 1,
        .remote_cookie = reg->cookie,
        .remote_offset = (uintptr_t) remote_address - reg->base_addr,
        .write = is_write,
        .flags = 0,
    };

    /* Use the DMA flag if knem supports it *and* the segment length
     * is greater than the cutoff. Not that if DMA is not supported
     * or the user specified 0 for knem_dma_min the knem_dma_min was
     * set to UINT_MAX in mca_smsc_knem_query. */
    if (mca_smsc_knem_component.dma_min <= size) {
        icopy.flags = KNEM_FLAG_DMA;
    }
    /* synchronous flags only, no need to specify icopy.async_status_index */

    /* When the ioctl returns, the transfer is done and we can invoke
       the btl callback and return the frag */
    if (OPAL_UNLIKELY(0 != ioctl(mca_smsc_knem_component.knem_fd, KNEM_CMD_INLINE_COPY, &icopy))) {
        opal_output_verbose(MCA_BASE_VERBOSE_WARN, opal_smsc_base_framework.framework_output,
                            "mca_smsc_knem_module_copy: failed to intiate transfer");
        return OPAL_ERROR;
    }

    if (KNEM_STATUS_FAILED == icopy.current_status) {
        opal_output_verbose(MCA_BASE_VERBOSE_WARN, opal_smsc_base_framework.framework_output,
                            "mca_smsc_knem_module_copy: transfter failed");
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

int mca_smsc_knem_copy_to(mca_smsc_endpoint_t *endpoint, void *local_address, void *remote_address,
                          size_t size, void *reg_data)
{
    return mca_smsc_knem_module_copy(endpoint, local_address, remote_address, size, reg_data,
                                     /*is_write=*/true);
}

int mca_smsc_knem_copy_from(mca_smsc_endpoint_t *endpoint, void *local_address,
                            void *remote_address, size_t size, void *reg_data)
{
    return mca_smsc_knem_module_copy(endpoint, local_address, remote_address, size, reg_data,
                                     /*is_write=*/false);
}

/* unsupported interfaces (for MCA direct) */
void *mca_smsc_knem_map_peer_region(mca_smsc_endpoint_t *endpoint, uint64_t flags,
                                    void *remote_address, size_t size, void **local_mapping)
{
    return NULL;
}

void mca_smsc_knem_unmap_peer_region(void *ctx)
{
}

mca_smsc_knem_module_t mca_smsc_knem_module = {
    .super = {
        .features = MCA_SMSC_FEATURE_REQUIRE_REGISTRATION,
        .registration_data_size = sizeof(mca_smsc_knem_registration_data_t),
        .get_endpoint = mca_smsc_knem_get_endpoint,
        .return_endpoint = mca_smsc_knem_return_endpoint,
        .copy_to = mca_smsc_knem_copy_to,
        .copy_from = mca_smsc_knem_copy_from,
        .register_region = mca_smsc_knem_register_region,
        .deregister_region = mca_smsc_knem_deregister_region,
    }, 
};
