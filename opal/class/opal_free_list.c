/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2010-2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2012-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/class/opal_free_list.h"
#include "opal/align.h"
#include "opal/util/output.h"
#include "opal/mca/mpool/mpool.h"

typedef struct opal_free_list_item_t opal_free_list_memory_t;

OBJ_CLASS_INSTANCE(opal_free_list_item_t,
                   opal_list_item_t,
                   NULL, NULL);

static void opal_free_list_construct(opal_free_list_t* fl)
{
    OBJ_CONSTRUCT(&fl->fl_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&fl->fl_condition, opal_condition_t);
    fl->fl_max_to_alloc = 0;
    fl->fl_num_allocated = 0;
    fl->fl_num_per_alloc = 0;
    fl->fl_num_waiting = 0;
    fl->fl_frag_size = sizeof(opal_free_list_item_t);
    fl->fl_frag_alignment = 0;
    fl->fl_payload_buffer_size = 0;
    fl->fl_payload_buffer_alignment = 0;
    fl->fl_frag_class = OBJ_CLASS(opal_free_list_item_t);
    fl->fl_mpool = NULL;
    /* default flags */
    fl->fl_mpool_reg_flags = MCA_MPOOL_FLAGS_CACHE_BYPASS |
        MCA_MPOOL_FLAGS_CUDA_REGISTER_MEM;
    fl->ctx = NULL;
    OBJ_CONSTRUCT(&(fl->fl_allocations), opal_list_t);
}

static void opal_free_list_allocation_release (opal_free_list_t *fl, opal_free_list_memory_t *fl_mem)
{
    if (NULL != fl->fl_mpool) {
        fl->fl_mpool->mpool_free (fl->fl_mpool, fl_mem->ptr, fl_mem->registration);
    } else if (fl_mem->ptr) {
        free (fl_mem->ptr);
    }

    /* destruct the item (we constructed it), then free the memory chunk */
    OBJ_DESTRUCT(fl_mem);
    free(fl_mem);
}

static void opal_free_list_destruct(opal_free_list_t *fl)
{
    opal_list_item_t *item;
    opal_free_list_item_t *fl_item;

#if 0 && OPAL_ENABLE_DEBUG
    if(opal_list_get_size(&fl->super) != fl->fl_num_allocated) {
        opal_output(0, "opal_free_list: %d allocated %d returned: %s:%d\n",
            fl->fl_num_allocated, opal_list_get_size(&fl->super),
            fl->super.super.cls_init_file_name, fl->super.super.cls_init_lineno);
    }
#endif

    while(NULL != (item = opal_lifo_pop(&(fl->super)))) {
        fl_item = (opal_free_list_item_t*)item;

        /* destruct the item (we constructed it), the underlying memory will be
         * reclaimed when we free the slab (opal_free_list_memory_t ptr)
         * containing it */
        OBJ_DESTRUCT(fl_item);
    }

    while(NULL != (item = opal_list_remove_first(&fl->fl_allocations))) {
        opal_free_list_allocation_release (fl, (opal_free_list_memory_t *) item);
    }

    OBJ_DESTRUCT(&fl->fl_allocations);
    OBJ_DESTRUCT(&fl->fl_condition);
    OBJ_DESTRUCT(&fl->fl_lock);
}

OBJ_CLASS_INSTANCE(opal_free_list_t, opal_lifo_t, opal_free_list_construct,
                   opal_free_list_destruct);


int opal_free_list_init (opal_free_list_t *flist, size_t frag_size, size_t frag_alignment,
                         opal_class_t *frag_class, size_t payload_buffer_size,
                         size_t payload_buffer_alignment, int num_elements_to_alloc,
                         int max_elements_to_alloc, int num_elements_per_alloc,
                         mca_mpool_base_module_t* mpool, int mpool_reg_flags,
                         void *unused0, opal_free_list_item_init_fn_t item_init, void *ctx)
{
    /* alignment must be more than zero and power of two */
    if (frag_alignment <= 1 || (frag_alignment & (frag_alignment - 1))) {
        return OPAL_ERROR;
    }

    if (0 < payload_buffer_size) {
        if (payload_buffer_alignment <= 1 || (payload_buffer_alignment & (payload_buffer_alignment - 1)))
            return OPAL_ERROR;
    }

    if (frag_size > flist->fl_frag_size) {
        flist->fl_frag_size = frag_size;
    }

    if (frag_class) {
        flist->fl_frag_class = frag_class;
    }

    flist->fl_payload_buffer_size = payload_buffer_size;
    flist->fl_max_to_alloc = max_elements_to_alloc;
    flist->fl_num_allocated = 0;
    flist->fl_num_per_alloc = num_elements_per_alloc;
    flist->fl_mpool = mpool;
    flist->fl_frag_alignment = frag_alignment;
    flist->fl_payload_buffer_alignment = payload_buffer_alignment;
    flist->item_init = item_init;
    flist->fl_mpool_reg_flags |= mpool_reg_flags;
    flist->ctx = ctx;

    if (num_elements_to_alloc) {
        return opal_free_list_grow_st (flist, num_elements_to_alloc);
    }

    return OPAL_SUCCESS;
}

int opal_free_list_grow_st (opal_free_list_t* flist, size_t num_elements)
{
    unsigned char *ptr, *mpool_alloc_ptr = NULL, *payload_ptr = NULL;
    opal_free_list_memory_t *alloc_ptr;
    size_t alloc_size, head_size, elem_size = 0;
    mca_mpool_base_registration_t *reg = NULL;
    int rc = OPAL_SUCCESS;

    if (flist->fl_max_to_alloc && (flist->fl_num_allocated + num_elements) >
        flist->fl_max_to_alloc) {
        num_elements = flist->fl_max_to_alloc - flist->fl_num_allocated;
    }

    if (num_elements == 0) {
        return OPAL_ERR_TEMP_OUT_OF_RESOURCE;
    }

    head_size = (NULL == flist->fl_mpool) ? flist->fl_frag_size:
        flist->fl_frag_class->cls_sizeof;
    head_size = OPAL_ALIGN(head_size, flist->fl_frag_alignment, size_t);

    /* calculate head allocation size */
    alloc_size = num_elements * head_size + sizeof(opal_free_list_memory_t) +
        flist->fl_frag_alignment;

    alloc_ptr = (opal_free_list_memory_t *) malloc(alloc_size);
    if (OPAL_UNLIKELY(NULL == alloc_ptr)) {
        return OPAL_ERR_TEMP_OUT_OF_RESOURCE;
    }

    if (0 != flist->fl_payload_buffer_size) {
        elem_size = OPAL_ALIGN(flist->fl_payload_buffer_size,
                               flist->fl_payload_buffer_alignment, size_t);

        /* elem_size should not be 0 here */
        assert (elem_size > 0);

        /* allocate the rest from the mpool (or use memalign/malloc) */
        if(flist->fl_mpool != NULL) {
            payload_ptr = mpool_alloc_ptr =
                (unsigned char *) flist->fl_mpool->mpool_alloc(flist->fl_mpool,
                                                               num_elements * elem_size,
                                                               flist->fl_payload_buffer_alignment,
                                                               flist->fl_mpool_reg_flags, &reg);
        } else {
#ifdef HAVE_POSIX_MEMALIGN
            posix_memalign ((void **) &mpool_alloc_ptr, flist->fl_payload_buffer_alignment,
                            num_elements * elem_size);
            payload_ptr = mpool_alloc_ptr;
#else
            mpool_alloc_ptr = (unsigned char *) malloc (num_elements * elem_size +
                                      flist->fl_payload_buffer_alignment);
            payload_ptr = (unsigned char *) OPAL_ALIGN((uintptr_t)mpool_alloc_ptr,
                                            flist->fl_payload_buffer_alignment,
                                            uintptr_t);
#endif
        }

        if(NULL == mpool_alloc_ptr) {
            free(alloc_ptr);
            return OPAL_ERR_TEMP_OUT_OF_RESOURCE;
        }
    }

    /* make the alloc_ptr a list item, save the chunk in the allocations list,
     * and have ptr point to memory right after the list item structure */
    OBJ_CONSTRUCT(alloc_ptr, opal_free_list_item_t);
    opal_list_append(&(flist->fl_allocations), (opal_list_item_t*)alloc_ptr);

    alloc_ptr->registration = reg;
    alloc_ptr->ptr = mpool_alloc_ptr;

    ptr = (unsigned char*)alloc_ptr + sizeof(opal_free_list_memory_t);
    ptr = OPAL_ALIGN_PTR(ptr, flist->fl_frag_alignment, unsigned char*);

    for(size_t i = 0; i < num_elements ; ++i) {
        opal_free_list_item_t* item = (opal_free_list_item_t*)ptr;
        item->registration = reg;
        item->ptr = payload_ptr;

        OBJ_CONSTRUCT_INTERNAL(item, flist->fl_frag_class);
        item->super.item_free = 0;

        /* run the initialize function if present */
        if (flist->item_init) {
            if (OPAL_SUCCESS != (rc = flist->item_init(item, flist->ctx))) {
                num_elements = i;
                OBJ_DESTRUCT (item);
                break;
            }
        }

        /* NTH: in case the free list may be accessed from multiple threads
         * use the atomic lifo push. The overhead is small compared to the
         * overall overhead of opal_free_list_grow(). */
        opal_lifo_push_atomic (&flist->super, &item->super);
        ptr += head_size;
        payload_ptr += elem_size;

    }

    if (OPAL_SUCCESS != rc && 0 == num_elements) {
        /* couldn't initialize any items */
        opal_list_remove_item (&flist->fl_allocations, (opal_list_item_t *) alloc_ptr);
        opal_free_list_allocation_release (flist, alloc_ptr);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    flist->fl_num_allocated += num_elements;
    return OPAL_SUCCESS;
}

/**
 * This function resize the free_list to contain at least the specified
 * number of elements. We do not create all of them in the same memory
 * segment. Instead we will several time the fl_num_per_alloc elements
 * until we reach the required number of the maximum allowed by the
 * initialization.
 */
int opal_free_list_resize_mt(opal_free_list_t *flist, size_t size)
{
    ssize_t inc_num;
    int ret = OPAL_SUCCESS;

    if (flist->fl_num_allocated > size) {
        return OPAL_SUCCESS;
    }

    opal_mutex_lock (&flist->fl_lock);
    do {
        ret = opal_free_list_grow_st (flist, flist->fl_num_per_alloc);
        if (OPAL_SUCCESS != ret) {
            break;
        }

        inc_num = (ssize_t)size - (ssize_t)flist->fl_num_allocated;
    } while (inc_num > 0);
    opal_mutex_unlock (&flist->fl_lock);

    return ret;
}
