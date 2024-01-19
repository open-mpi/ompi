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
#include "opal/include/opal/opal_cuda.h"
#include <cuda.h>

/**
 * Called when the registration free list is created.  An event is created
 * for each entry.
 */
static void mca_rcache_gpusm_registration_constructor(mca_rcache_gpusm_registration_t *item)
{
    uintptr_t *event = &item->event;
    void *handle = (void *) &item->evtHandle;
    CUresult result;

    result = cuEventCreate((CUevent *) event,
                                  CU_EVENT_INTERPROCESS | CU_EVENT_DISABLE_TIMING);
    if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
        opal_output(0, "cuEventCreate failed\n");
    }

    result = cuIpcGetEventHandle((CUipcEventHandle *) handle, (CUevent) *event);
    if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
        opal_output(0, "cuIpcGetEventHandle failed\n");
    }
}

/**
 * Called when the program is exiting.  This destroys the events.
 */
static void mca_rcache_gpusm_registration_destructor(mca_rcache_gpusm_registration_t *item)
{
    uintptr_t event = item->event;
    CUresult result;

    result = cuEventDestroy((CUevent) event);
    if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
        opal_output(0, "cuEventDestroy failed");
    }
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
    opal_free_list_init(&rcache->reg_list, sizeof(struct mca_opal_cuda_reg_t),
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
    CUmemorytype memType;
    CUresult result;
    CUipcMemHandle *memHandle;
    CUdeviceptr pbase;
    size_t psize;

    mca_opal_cuda_reg_t *cuda_reg = (mca_opal_cuda_reg_t *) newreg;
    memHandle = (CUipcMemHandle *) cuda_reg->data.memHandle;

    /* We should only be there if this is a CUDA device pointer */
    result = cuPointerGetAttribute(&memType, CU_POINTER_ATTRIBUTE_MEMORY_TYPE,
                                          (CUdeviceptr) base);
    assert(CUDA_SUCCESS == result);
    assert(CU_MEMORYTYPE_DEVICE == memType);

    /* Get the memory handle so we can send it to the remote process. */
    result = cuIpcGetMemHandle(memHandle, (CUdeviceptr) base);

    if (CUDA_SUCCESS != result) {
        return OPAL_ERROR;
    }

    /* Need to get the real base and size of the memory handle.  This is
     * how the remote side saves the handles in a cache. */
    result = cuMemGetAddressRange(&pbase, &psize, (CUdeviceptr) base);
    if (CUDA_SUCCESS != result) {
        return OPAL_ERROR;
    }

    /* Store all the information in the registration */
    cuda_reg->base.base = (void *) pbase;
    cuda_reg->base.bound = (unsigned char *) pbase + psize - 1;
    cuda_reg->data.memh_seg_addr.pval = (void *) pbase;
    cuda_reg->data.memh_seg_len = psize;

#if OPAL_CUDA_SYNC_MEMOPS
    /* With CUDA 6.0, we can set an attribute on the memory pointer that will
     * ensure any synchronous copies are completed prior to any other access
     * of the memory region.  This means we do not need to record an event
     * and send to the remote side.
     */
    memType = 1; /* Just use this variable since we already have it */
    result = cuPointerSetAttribute(&memType, CU_POINTER_ATTRIBUTE_SYNC_MEMOPS,
                                          (CUdeviceptr) base);
    if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
        return OPAL_ERROR;
    }
#else
    /* Need to record the event to ensure that any memcopies into the
     * device memory have completed.  The event handle associated with
     * this event is sent to the remote process so that it will wait
     * on this event prior to copying data out of the device memory.
     * Note that this needs to be the NULL stream to make since it is
     * unknown what stream any copies into the device memory were done
     * with. */
    result = cuEventRecord((CUevent) cuda_reg->data.event, 0);
    if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
        return OPAL_ERROR;
    }
#endif /* OPAL_CUDA_SYNC_MEMOPS */

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

    if (rc != OPAL_SUCCESS) {
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
    int rc;
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
