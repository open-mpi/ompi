/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
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
 * Copyright (c) 2012      NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 *
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
#include "opal/mca/mpool/base/base.h"
#include "opal/mca/mpool/gpusm/mpool_gpusm.h"
#include "opal/mca/common/cuda/common_cuda.h"

/**
 * Called when the registration free list is created.  An event is created
 * for each entry. 
 */
static void mca_mpool_gpusm_registration_constructor( mca_mpool_gpusm_registration_t *item )
{
    mca_common_cuda_construct_event_and_handle((uint64_t **)&item->event,
                                               (void **)&item->evtHandle);
}

/**
 * Called when the program is exiting.  This destroys the events.
 */
static void mca_mpool_gpusm_registration_destructor( mca_mpool_gpusm_registration_t *item )
{
    mca_common_cuda_destruct_event((uint64_t *)item->event);
}

OBJ_CLASS_INSTANCE(mca_mpool_gpusm_registration_t, mca_mpool_base_registration_t, 
                   mca_mpool_gpusm_registration_constructor,
                   mca_mpool_gpusm_registration_destructor);

/*
 *  Initializes the mpool module.
 */
void mca_mpool_gpusm_module_init(mca_mpool_gpusm_module_t* mpool)
{
    mpool->super.mpool_component = &mca_mpool_gpusm_component.super;
    mpool->super.mpool_base = NULL;
    mpool->super.mpool_alloc = NULL;
    mpool->super.mpool_realloc = NULL;
    mpool->super.mpool_free = NULL;
    mpool->super.mpool_register = mca_mpool_gpusm_register;
    mpool->super.mpool_find = mca_mpool_gpusm_find;
    mpool->super.mpool_deregister = mca_mpool_gpusm_deregister;
    mpool->super.mpool_release_memory = NULL;
    mpool->super.mpool_finalize = mca_mpool_gpusm_finalize;
    mpool->super.mpool_ft_event = mca_mpool_gpusm_ft_event;
    mpool->super.rcache = NULL;
    mpool->super.flags = 0;

    mpool->resources.reg_data = NULL;
    mpool->resources.sizeof_reg = sizeof(struct mca_mpool_common_cuda_reg_t);
    mpool->resources.register_mem = cuda_getmemhandle;
    mpool->resources.deregister_mem = cuda_ungetmemhandle;

    OBJ_CONSTRUCT(&mpool->reg_list, opal_free_list_t);

    /* Start with 0 entries in the free list since CUDA may not have
     * been initialized when this free list is created and there is
     * some CUDA specific activities that need to be done. */
    opal_free_list_init (&mpool->reg_list, mpool->resources.sizeof_reg,
            opal_cache_line_size,
            OBJ_CLASS(mca_mpool_gpusm_registration_t), 
            0,opal_cache_line_size,
            0, -1, 64, NULL, 0, NULL, NULL, NULL);

}

/**
 * Just go ahead and get a new registration.  The find and register
 * functions are the same thing for this memory pool.
 */
int mca_mpool_gpusm_find(mca_mpool_base_module_t *mpool, void *addr,
                         size_t size,
                         mca_mpool_base_registration_t **reg)
{
    return mca_mpool_gpusm_register(mpool, addr, size, 0, reg);
}

/*
 * This is the one function that does all the work.  It will call into
 * the register function to get the memory handle for the sending
 * buffer.  There is no need to deregister the memory handle so the
 * deregister function is a no-op.
 */
int mca_mpool_gpusm_register(mca_mpool_base_module_t *mpool, void *addr,
                             size_t size, uint32_t flags,
                             mca_mpool_base_registration_t **reg)
{
    mca_mpool_gpusm_module_t *mpool_gpusm = (mca_mpool_gpusm_module_t*)mpool;
    mca_mpool_base_registration_t *gpusm_reg;
    opal_free_list_item_t *item;
    unsigned char *base, *bound;
    int rc;

    /* In spite of the fact we return an error code, the existing code
     * checks the registration for a NULL value rather than looking at
     * the return code.  So, initialize the registration to NULL in
     * case we run into a failure. */
    *reg = NULL;

    base = addr;
    bound = (unsigned char *)addr + size - 1;

    item = opal_free_list_get (&mpool_gpusm->reg_list);
    if(NULL == item) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    gpusm_reg = (mca_mpool_base_registration_t*)item;

    gpusm_reg->mpool = mpool;
    gpusm_reg->base = base;
    gpusm_reg->bound = bound;
    gpusm_reg->flags = flags;

    rc = mpool_gpusm->resources.register_mem(base, size, gpusm_reg, NULL);

    if(rc != OPAL_SUCCESS) {
        opal_free_list_return (&mpool_gpusm->reg_list, item);
        return rc;
    }

    *reg = gpusm_reg;
    (*reg)->ref_count++;
    return OPAL_SUCCESS;

}

/*
 * Return the registration to the free list.
 */
int mca_mpool_gpusm_deregister(struct mca_mpool_base_module_t *mpool,
                               mca_mpool_base_registration_t *reg)
{
    int rc;
    mca_mpool_gpusm_module_t *mpool_gpusm = (mca_mpool_gpusm_module_t *)mpool;

    rc = mpool_gpusm->resources.deregister_mem(mpool, reg);
    opal_free_list_return (&mpool_gpusm->reg_list, (opal_free_list_item_t *) reg);
    return OPAL_SUCCESS;
}

/**
 * Free up the resources.  
 */
void mca_mpool_gpusm_finalize(struct mca_mpool_base_module_t *mpool)
{
    opal_free_list_item_t *item;
    mca_mpool_gpusm_module_t *mpool_gpusm = (mca_mpool_gpusm_module_t *)mpool;

    /* Need to run the destructor on each item in the free list explicitly.
     * The destruction of the free list only runs the destructor on the
     * main free list, not each item. */
    while (NULL != (item = (opal_free_list_item_t *)opal_lifo_pop(&(mpool_gpusm->reg_list.super)))) {
        OBJ_DESTRUCT(item);
    }

    OBJ_DESTRUCT(&mpool_gpusm->reg_list);
    return;
}

int mca_mpool_gpusm_ft_event(int state) {
    return OPAL_SUCCESS;
}
