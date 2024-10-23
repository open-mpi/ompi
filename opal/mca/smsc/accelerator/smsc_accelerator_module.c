/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2024      Advanced Micro Devices, Inc. All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/include/opal/align.h"
#include "opal/mca/rcache/rcache.h"
#include "opal/mca/rcache/base/base.h"
#include "opal/mca/accelerator/accelerator.h"
#include "opal/mca/accelerator/base/base.h"
#include "opal/mca/smsc/base/base.h"
#include "opal/mca/smsc/accelerator/smsc_accelerator.h"
#include "opal/mca/smsc/accelerator/smsc_accelerator_internal.h"
#include "opal/include/opal/opal_gpu.h"

OBJ_CLASS_INSTANCE(mca_smsc_accelerator_endpoint_t, opal_object_t, NULL, NULL);

mca_smsc_endpoint_t *mca_smsc_accelerator_get_endpoint(opal_proc_t *peer_proc)
{
    mca_smsc_accelerator_endpoint_t *endpoint = OBJ_NEW(mca_smsc_accelerator_endpoint_t);
    if (OPAL_UNLIKELY(NULL == endpoint)) {
        return NULL;
    }

    if (NULL != mca_smsc_accelerator_module.prev_smsc) {
        endpoint->prev_endpoint = mca_smsc_accelerator_module.prev_smsc->get_endpoint(peer_proc);
    }

    endpoint->rcache = mca_rcache_base_module_create("rgpusm", NULL, NULL);

    return &endpoint->super;
}

void mca_smsc_accelerator_return_endpoint(mca_smsc_endpoint_t *endpoint)
{
    mca_smsc_accelerator_endpoint_t *ep = (mca_smsc_accelerator_endpoint_t *)endpoint;

    if ((NULL != mca_smsc_accelerator_module.prev_smsc) &&
        (NULL != ep->prev_endpoint)) {
        mca_smsc_accelerator_module.prev_smsc->return_endpoint(ep->prev_endpoint);
    }

    mca_rcache_base_module_destroy(ep->rcache);
    OBJ_RELEASE(endpoint);
}

void *mca_smsc_accelerator_map_peer_region(mca_smsc_endpoint_t *endpoint, uint64_t flags,
                                     void *remote_ptr, size_t size, void **local_ptr)
{
    void* result = NULL;
    mca_smsc_accelerator_endpoint_t *ep = (mca_smsc_accelerator_endpoint_t *)endpoint;

    if (NULL != mca_smsc_accelerator_module.prev_smsc &&
        (mca_smsc_accelerator_module.prev_smsc->features & MCA_SMSC_FEATURE_CAN_MAP)) {
        result = mca_smsc_accelerator_module.prev_smsc->map_peer_region(ep->prev_endpoint,
                                                                        flags, remote_ptr, size, local_ptr);
    }
    return result;
}

void mca_smsc_accelerator_unmap_peer_region(void *ctx)
{
    if (NULL != mca_smsc_accelerator_module.prev_smsc &&
        (mca_smsc_accelerator_module.prev_smsc->features & MCA_SMSC_FEATURE_CAN_MAP)) {
        mca_smsc_accelerator_module.prev_smsc->unmap_peer_region(ctx);
    }
}

static int mca_smsc_accelerator_copy (mca_smsc_endpoint_t *endpoint, void *local_address, void *remote_address,
                                      size_t size, void *reg_handle, bool is_remote_write)
{
    mca_smsc_accelerator_endpoint_t *ep = (mca_smsc_accelerator_endpoint_t *)endpoint;
    mca_smsc_accelerator_registration_data_t *reg = (mca_smsc_accelerator_registration_data_t *)reg_handle;
    int ret = OPAL_SUCCESS;
    void *remote_memory_address;
    mca_opal_gpu_reg_t rget_reg;
    mca_opal_gpu_reg_t *reg_ptr = &rget_reg;
    size_t offset;

    memset(&rget_reg, 0, sizeof(rget_reg));
    memcpy(&rget_reg.data.ipcHandle.handle, reg->handle.accelerator,
           SMSC_ACCELERATOR_HANDLE_SIZE);
    ret = ep->rcache->rcache_register(ep->rcache, remote_address, size,
                                      /*ep->peer_smp_rank for debugging only */ 0,
                                      MCA_RCACHE_ACCESS_LOCAL_WRITE,
                                      (mca_rcache_base_registration_t **) &reg_ptr);
    if (OPAL_SUCCESS != ret) {
        opal_output(0, "mca_smsc_accelerator_copy: Failed to register remote memory, ret=%d", ret);
        return ret;
    }

    offset = (size_t)((intptr_t) remote_address - (intptr_t) reg->base_addr);
    remote_memory_address = (unsigned char *) reg_ptr->base.alloc_base + offset;

    if (is_remote_write) {
        ret = opal_accelerator.mem_copy(MCA_ACCELERATOR_NO_DEVICE_ID, mca_smsc_accelerator_module.device_id,
                                        remote_memory_address, local_address, size,
                                        MCA_ACCELERATOR_TRANSFER_DTOD);
    } else {
        ret = opal_accelerator.mem_copy(mca_smsc_accelerator_module.device_id, MCA_ACCELERATOR_NO_DEVICE_ID,
                                        local_address, remote_memory_address, size,
                                        MCA_ACCELERATOR_TRANSFER_DTOD);
    }
    if (OPAL_SUCCESS != ret) {
        opal_output(0, "mca_smsc_accelerator_copy: accelerator mem_copy failed, ret=%d", ret);
    }

    return ret;
}

int mca_smsc_accelerator_copy_to(mca_smsc_endpoint_t *endpoint, void *local_address, void *remote_address,
                                 size_t size, void *reg_handle)
{
    mca_smsc_accelerator_endpoint_t *ep = (mca_smsc_accelerator_endpoint_t *)endpoint;
    mca_smsc_accelerator_registration_data_t *reg = (mca_smsc_accelerator_registration_data_t *)reg_handle;
    int ret = OPAL_SUCCESS;

    if ((NULL != reg) && (0 != reg->base_addr)) {
        ret = mca_smsc_accelerator_copy (endpoint, local_address, remote_address, size, reg_handle, true);
    }
    else if (NULL != mca_smsc_accelerator_module.prev_smsc) {
        ret = mca_smsc_accelerator_module.prev_smsc->copy_to(ep->prev_endpoint, local_address,
                                                             remote_address, size, reg_handle);
    }

    return ret;
}

int mca_smsc_accelerator_copy_from(mca_smsc_endpoint_t *endpoint, void *local_address,
                                   void *remote_address, size_t size, void *reg_handle)
{
    mca_smsc_accelerator_endpoint_t *ep = (mca_smsc_accelerator_endpoint_t *)endpoint;
    mca_smsc_accelerator_registration_data_t *reg = (mca_smsc_accelerator_registration_data_t *)reg_handle;
    int ret = OPAL_SUCCESS;

    if ((NULL != reg) && (0 != reg->base_addr)) {
        ret = mca_smsc_accelerator_copy (endpoint, local_address, remote_address, size, reg_handle, false);
    }
    else if (NULL != mca_smsc_accelerator_module.prev_smsc) {
        ret = mca_smsc_accelerator_module.prev_smsc->copy_from(ep->prev_endpoint, local_address,
                                                               remote_address, size, reg_handle);
    }

    return ret;
}

void *mca_smsc_accelerator_register_region(void *local_address, size_t size)
{
    int dev_id, ret;
    mca_smsc_accelerator_registration_handle_t *reg;
    uint64_t flags;

    if (opal_accelerator.check_addr(local_address, &dev_id, &flags) > 0) {
        ret = mca_smsc_accelerator_module.rcache->rcache_register(mca_smsc_accelerator_module.rcache,
                                                                  local_address, size,
                                                                  MCA_RCACHE_FLAGS_ACCELERATOR_MEM,  MCA_RCACHE_ACCESS_ANY,
                                                                  (mca_rcache_base_registration_t **) &reg);
        if (OPAL_UNLIKELY(OPAL_SUCCESS != ret)) {
            opal_output_verbose(MCA_BASE_VERBOSE_WARN, opal_smsc_base_framework.framework_output,
                                "mca_smsc_accelerator_register_mem: failed to register memory for single-copy");
            return NULL;
        }
        reg->data.base_addr = (uint64_t) reg->base.base;
    }
    else {
        reg = (mca_smsc_accelerator_registration_handle_t *)malloc (sizeof(mca_smsc_accelerator_registration_handle_t));
        if (NULL == reg) {
            opal_output(0, "mca_smsc_accelerator_register_mem: failed to allocate memory");
            return NULL;
        }

        reg->data.handle.host = NULL;
        if ( (NULL != mca_smsc_accelerator_module.prev_smsc) &&
             (mca_smsc_accelerator_module.prev_smsc->features & MCA_SMSC_FEATURE_REQUIRE_REGISTRATION)) {
            reg->data.handle.host = mca_smsc_accelerator_module.prev_smsc->register_region(local_address, size);
        }
        reg->data.base_addr = 0;
    }

    return &(reg->data);
}

void mca_smsc_accelerator_deregister_region(void *reg_data)
{
    if (NULL == reg_data) {
        return;
    }

    mca_smsc_accelerator_registration_data_t *reg = (mca_smsc_accelerator_registration_data_t *)reg_data;
    mca_smsc_accelerator_registration_handle_t *reg_handle = MCA_SMSC_ACCELERATOR_REG_DATA_TO_HANDLE(reg);
    if (0 != reg->base_addr) {
        mca_smsc_accelerator_module.rcache->rcache_deregister(mca_smsc_accelerator_module.rcache,
                                                              &reg_handle->base);
    }
    else {
        if ((NULL != mca_smsc_accelerator_module.prev_smsc) &&
            (mca_smsc_accelerator_module.prev_smsc->features & MCA_SMSC_FEATURE_REQUIRE_REGISTRATION) &&
            (NULL != reg->handle.host)) {
            mca_smsc_accelerator_module.prev_smsc->deregister_region(reg_data);
        }
        free(reg_handle);
    }
}

mca_smsc_accelerator_module_t mca_smsc_accelerator_module = {
    .super = {
        .registration_data_size = sizeof(mca_smsc_accelerator_registration_data_t),
        .features = MCA_SMSC_FEATURE_REQUIRE_REGISTRATION | MCA_SMSC_FEATURE_ACCELERATOR, // also modified in component_open
        .get_endpoint = mca_smsc_accelerator_get_endpoint,
        .return_endpoint = mca_smsc_accelerator_return_endpoint,
        .copy_to = mca_smsc_accelerator_copy_to,
        .copy_from = mca_smsc_accelerator_copy_from,
        .map_peer_region = mca_smsc_accelerator_map_peer_region,
        .unmap_peer_region = mca_smsc_accelerator_unmap_peer_region,
        .register_region = mca_smsc_accelerator_register_region,
        .deregister_region = mca_smsc_accelerator_deregister_region,
    },
};
