/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2024 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2009 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006      Voltaire. All rights reserved.
 * Copyright (c) 2007      Mellanox Technologies. All rights reserved.
 * Copyright (c) 2010      IBM Corporation.  All rights reserved.
 * Copyright (c) 2012-2015 NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 *
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * Copyright (c) 2023      Advanced Micro Devices, Inc. All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file:
 *
 * This file implements a simple memory pool that is used by the GPU
 * buffer on the sending side.  It just gets a memory handle and event
 * handle that can be sent to the remote side which can then use the
 * handles to get access to the memory and the event to determine when
 * it can start accessing the memory.  There is no caching of the
 * memory handles as getting new ones is fast.  The event handles are
 * cached by the cuda_common code.
 */

#include "opal_config.h"
#include "opal/mca/rcache/base/base.h"
#include "opal/mca/rcache/gpusm/rcache_gpusm.h"
#include "opal/include/opal/opal_gpu.h"
#include "opal/mca/accelerator/base/base.h"

/**
 * Called when the registration free list is created.  An event is created
 * for each entry.
 */
static void mca_rcache_gpusm_registration_constructor(mca_rcache_gpusm_registration_t *item)
{
    int result;

    result = opal_accelerator.create_event(MCA_ACCELERATOR_NO_DEVICE_ID, &item->event, true);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != result)) {
        opal_output(0, "create_ipc_event failed\n");
    }

    result = opal_accelerator.get_ipc_event_handle(item->event, &item->evtHandle);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != result)) {
        opal_output(0, "get_ipc_event_handle failed\n");
    }
}

/**
 * Called when the program is exiting.  This destroys the events.
 */
static void mca_rcache_gpusm_registration_destructor(mca_rcache_gpusm_registration_t *item)
{
    OBJ_RELEASE(item->event);
}

OBJ_CLASS_INSTANCE(mca_rcache_gpusm_registration_t, mca_rcache_base_registration_t,
                   mca_rcache_gpusm_registration_constructor,
                   mca_rcache_gpusm_registration_destructor);

/*
 *  Initializes the rcache module.
 */
void mca_rcache_gpusm_module_init(mca_rcache_gpusm_module_t *rcache)
{
    mca_rcache_base_module_init(&rcache->super);

    rcache->super.rcache_component = &mca_rcache_gpusm_component.super;
    rcache->super.rcache_register = mca_rcache_gpusm_register;
    rcache->super.rcache_find = mca_rcache_gpusm_find;
    rcache->super.rcache_deregister = mca_rcache_gpusm_deregister;
    rcache->super.rcache_finalize = mca_rcache_gpusm_finalize;

    OBJ_CONSTRUCT(&rcache->reg_list, opal_free_list_t);

    /* Start with 0 entries in the free list since CUDA may not have
     * been initialized when this free list is created and there is
     * some CUDA specific activities that need to be done. */
    opal_free_list_init(&rcache->reg_list, sizeof(struct mca_opal_gpu_reg_t),
                        opal_cache_line_size, OBJ_CLASS(mca_rcache_gpusm_registration_t), 0,
                        opal_cache_line_size, 0, -1, 64, NULL, 0, NULL, NULL, NULL);
}

/**
 * Just go ahead and get a new registration.  The find and register
 * functions are the same thing for this memory pool.
 */
int mca_rcache_gpusm_find(mca_rcache_base_module_t *rcache, void *addr, size_t size,
                          mca_rcache_base_registration_t **reg)
{
    return mca_rcache_gpusm_register(rcache, addr, size, 0, 0, reg);
}

/*
 * Get the memory handle of a local section of memory that can be sent
 * to the remote size so it can access the memory.  This is the
 * registration function for the sending side of a message transfer.
 */
static int mca_rcache_gpusm_get_mem_handle(void *base, size_t size, mca_rcache_base_registration_t *newreg)
{
    mca_opal_gpu_reg_t *gpu_reg = (mca_opal_gpu_reg_t *) newreg;
    opal_accelerator_buffer_id_t buffer_id;
    uint64_t flags;
    int      dev_id;
    int      result;
    void*    pbase;
    size_t   psize;

    result = opal_accelerator.check_addr(base, &dev_id, &flags);
    if (0 >= result) {
        // This is either an error or host memory. In either case we do not continue
        return OPAL_ERROR;
    }

    result = opal_accelerator.get_ipc_handle(dev_id, base, &gpu_reg->data.ipcHandle);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != result)) {
        return OPAL_ERROR;
    }
    result = opal_accelerator.get_address_range (dev_id, base, &pbase, &psize);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != result)) {
        return OPAL_ERROR;
    }

    /* Store all the information in the registration */
    gpu_reg->base.base  = pbase;
    gpu_reg->base.bound = (unsigned char *) pbase + psize - 1;
    gpu_reg->data.memh_seg_addr.pval = (void *) pbase;
    gpu_reg->data.memh_seg_len = psize;

    // converting the ifdef into a mca runtime parameter
    if (opal_accelerator_use_sync_memops) {
        // need to revisit. This function also sets sync_memops
        // we might want to separate that out into a separate function
        result = opal_accelerator.get_buffer_id(dev_id, base, &buffer_id);
        if (OPAL_UNLIKELY(OPAL_SUCCESS != result)) {
            return OPAL_ERROR;
        }
    }
    else {
        result = opal_accelerator.record_event(dev_id, gpu_reg->data.event, 0);
        if (OPAL_UNLIKELY(OPAL_SUCCESS != result)) {
            return OPAL_ERROR;
        }
    }

    return OPAL_SUCCESS;
}

/*
 * This is the one function that does all the work.  It will call into
 * the register function to get the memory handle for the sending
 * buffer.  There is no need to deregister the memory handle so the
 * deregister function is a no-op.
 */
int mca_rcache_gpusm_register(mca_rcache_base_module_t *rcache, void *addr, size_t size,
                              uint32_t flags, int32_t access_flags,
                              mca_rcache_base_registration_t **reg)
{
    mca_rcache_gpusm_module_t *rcache_gpusm = (mca_rcache_gpusm_module_t *) rcache;
    mca_rcache_base_registration_t *gpusm_reg;
    opal_free_list_item_t *item;
    unsigned char *base, *bound;
    int rc;

    /* In spite of the fact we return an error code, the existing code
     * checks the registration for a NULL value rather than looking at
     * the return code.  So, initialize the registration to NULL in
     * case we run into a failure. */
    *reg = NULL;

    base = addr;
    bound = (unsigned char *) addr + size - 1;

    item = opal_free_list_get(&rcache_gpusm->reg_list);
    if (NULL == item) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    gpusm_reg = (mca_rcache_base_registration_t *) item;

    gpusm_reg->rcache = rcache;
    gpusm_reg->base = base;
    gpusm_reg->bound = bound;
    gpusm_reg->flags = flags;
    gpusm_reg->access_flags = access_flags;

    rc = mca_rcache_gpusm_get_mem_handle(base, size, gpusm_reg);

    if (OPAL_SUCCESS != rc) {
        opal_free_list_return(&rcache_gpusm->reg_list, item);
        return rc;
    }

    *reg = gpusm_reg;
    (*reg)->ref_count++;
    return OPAL_SUCCESS;
}

/*
 * Return the registration to the free list.
 */
int mca_rcache_gpusm_deregister(struct mca_rcache_base_module_t *rcache,
                                mca_rcache_base_registration_t *reg)
{
    mca_rcache_gpusm_module_t *rcache_gpusm = (mca_rcache_gpusm_module_t *) rcache;

    opal_free_list_return(&rcache_gpusm->reg_list, (opal_free_list_item_t *) reg);
    return OPAL_SUCCESS;
}

/**
 * Free up the resources.
 */
void mca_rcache_gpusm_finalize(struct mca_rcache_base_module_t *rcache)
{
    opal_free_list_item_t *item;
    mca_rcache_gpusm_module_t *rcache_gpusm = (mca_rcache_gpusm_module_t *) rcache;

    /* Need to run the destructor on each item in the free list explicitly.
     * The destruction of the free list only runs the destructor on the
     * main free list, not each item. */
    while (NULL
           != (item = (opal_free_list_item_t *) opal_lifo_pop(&(rcache_gpusm->reg_list.super)))) {
        OBJ_DESTRUCT(item);
    }

    OBJ_DESTRUCT(&rcache_gpusm->reg_list);

    mca_rcache_base_module_fini(rcache);

    return;
}
