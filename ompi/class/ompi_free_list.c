/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/class/ompi_free_list.h"
#include "opal/sys/cache.h"
#include "opal/util/output.h"
#include "ompi/mca/mpool/mpool.h"

static inline size_t align_to(size_t val, size_t alignment);
static inline size_t align_to(size_t val, size_t alignment)
{
    size_t mod;
   
    if(0 == alignment)
        return val;

    mod = val % alignment;
    
    if(mod)
        val += (alignment - mod);

    return val;
}

static void ompi_free_list_construct(ompi_free_list_t* fl);
static void ompi_free_list_destruct(ompi_free_list_t* fl);

OBJ_CLASS_INSTANCE(ompi_free_list_t, opal_atomic_lifo_t,
        ompi_free_list_construct, ompi_free_list_destruct);

struct ompi_free_list_memory_t {
    opal_list_item_t super;
    mca_mpool_base_registration_t *registration;
};
typedef struct ompi_free_list_memory_t ompi_free_list_memory_t;
static OBJ_CLASS_INSTANCE(ompi_free_list_memory_t,
                          opal_list_item_t,
                          NULL, NULL);

OBJ_CLASS_INSTANCE(ompi_free_list_item_t, 
                   opal_list_item_t,
                   NULL, NULL); 

static void ompi_free_list_construct(ompi_free_list_t* fl)
{
    OBJ_CONSTRUCT(&fl->fl_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&fl->fl_condition, opal_condition_t);
    fl->fl_max_to_alloc = 0;
    fl->fl_num_allocated = 0;
    fl->fl_num_per_alloc = 0;
    fl->fl_num_waiting = 0;
    fl->fl_elem_size = sizeof(ompi_free_list_item_t);
    fl->fl_elem_class = OBJ_CLASS(ompi_free_list_item_t);
    fl->fl_header_space = 0;
    fl->fl_alignment = 0;
    fl->fl_mpool = 0;
    OBJ_CONSTRUCT(&(fl->fl_allocations), opal_list_t);
}

static void ompi_free_list_destruct(ompi_free_list_t* fl)
{
    opal_list_item_t *item;

#if 0 && OMPI_ENABLE_DEBUG
    if(opal_list_get_size(&fl->super) != fl->fl_num_allocated) {
        opal_output(0, "ompi_free_list: %d allocated %d returned: %s:%d\n",
            fl->fl_num_allocated, opal_list_get_size(&fl->super),
            fl->super.super.cls_init_file_name, fl->super.super.cls_init_lineno);
    }
#endif

    if (NULL != fl->fl_mpool) {
        ompi_free_list_memory_t *fl_mem;

        while (NULL != (item = opal_list_remove_first(&(fl->fl_allocations)))) {
            /* destruct the item (we constructed it), then free the memory chunk */
            OBJ_DESTRUCT(item);
            fl_mem = (ompi_free_list_memory_t*) item;
            fl->fl_mpool->mpool_free(fl->fl_mpool, item, fl_mem->registration);
        }
    } else {
        while (NULL != (item = opal_list_remove_first(&(fl->fl_allocations)))) {
            /* destruct the item (we constructed it), then free the memory chunk */
            OBJ_DESTRUCT(item);
            free(item);
        }
    }

    OBJ_DESTRUCT(&fl->fl_allocations);
    OBJ_DESTRUCT(&fl->fl_condition);
    OBJ_DESTRUCT(&fl->fl_lock);
}

int ompi_free_list_init_ex(
    ompi_free_list_t *flist,
    size_t elem_size,
    size_t header_space,
    size_t alignment,
    opal_class_t* elem_class,
    int num_elements_to_alloc,
    int max_elements_to_alloc,
    int num_elements_per_alloc,
    mca_mpool_base_module_t* mpool)
{
    if(elem_size > flist->fl_elem_size)
        flist->fl_elem_size = elem_size;
    if(elem_class)
        flist->fl_elem_class = elem_class;
    flist->fl_max_to_alloc = max_elements_to_alloc;
    flist->fl_num_allocated = 0;
    flist->fl_num_per_alloc = num_elements_per_alloc;
    flist->fl_mpool = mpool;
    flist->fl_header_space = header_space;
    flist->fl_alignment = alignment;
    flist->fl_elem_size = align_to(flist->fl_elem_size, flist->fl_alignment);
    if(num_elements_to_alloc)
        return ompi_free_list_grow(flist, num_elements_to_alloc);
    return OMPI_SUCCESS;
}

int ompi_free_list_grow(ompi_free_list_t* flist, size_t num_elements)
{
    unsigned char* ptr;
    ompi_free_list_memory_t *alloc_ptr;
    size_t i, alloc_size;
    mca_mpool_base_registration_t* user_out = NULL; 

    if (flist->fl_max_to_alloc > 0)
        if (flist->fl_num_allocated + num_elements > flist->fl_max_to_alloc)
            num_elements = flist->fl_max_to_alloc - flist->fl_num_allocated;

    if (num_elements == 0)
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;

    alloc_size = num_elements * flist->fl_elem_size +
        sizeof(ompi_free_list_memory_t) + flist->fl_header_space +
        flist->fl_alignment;

    if (NULL != flist->fl_mpool)
        alloc_ptr = (ompi_free_list_memory_t*)flist->fl_mpool->mpool_alloc(flist->fl_mpool, 
                                                                           alloc_size, 0, MCA_MPOOL_FLAGS_CACHE_BYPASS, &user_out);
    else
        alloc_ptr = (ompi_free_list_memory_t*)malloc(alloc_size);

    if(NULL == alloc_ptr)
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;

    /* make the alloc_ptr a list item, save the chunk in the allocations list, and
       have ptr point to memory right after the list item structure */
    OBJ_CONSTRUCT(alloc_ptr, ompi_free_list_memory_t);
    opal_list_append(&(flist->fl_allocations), (opal_list_item_t*) alloc_ptr);

    alloc_ptr->registration = user_out;

    ptr = (unsigned char*) alloc_ptr + sizeof(ompi_free_list_memory_t);

    ptr = (unsigned char*)(align_to((size_t)ptr + flist->fl_header_space,
                flist->fl_alignment) - flist->fl_header_space);

    for(i=0; i<num_elements; i++) {
        ompi_free_list_item_t* item = (ompi_free_list_item_t*)ptr;
        item->user_data = user_out; 

        OBJ_CONSTRUCT_INTERNAL(item, flist->fl_elem_class);

        opal_atomic_lifo_push(&(flist->super), &(item->super));
        ptr += flist->fl_elem_size;
    }

    flist->fl_num_allocated += num_elements;
    return OMPI_SUCCESS;
}

/* This function is not protected. It should be never used when the
 * process s still active. It was designed for debugger, in order
 * to provide them with a fast mechanism to look into the queues
 * (mostly the MPI request queue).
 */
int ompi_free_list_parse( ompi_free_list_t* list,
                          struct ompi_free_list_pos_t* position,
                          opal_list_item_t** return_item )
{
    /* Make sure we are in one of the free list allocations */
    if( NULL == position->last_memory ) {
        position->last_memory = (unsigned char*)opal_list_get_first( &(list->fl_allocations) );
        position->last_item = NULL;
    }

 dig_for_the_requests:
    /* If the request will be the first on this memory region, it's easy. */
    if( NULL == position->last_item ) {
        unsigned long ptr = (unsigned long)position->last_memory;

        ptr += sizeof(ompi_free_list_memory_t);

        ptr = align_to(ptr + list->fl_header_space, list->fl_alignment) -
            list->fl_header_space;
        *return_item = (opal_list_item_t*)ptr;
        return 0;
    }
    /* else go to the next request */
    position->last_item += list->fl_elem_size;

    {
        /* otherwise go to the next one. Once there make sure we're still on the
         * memory fragment, otherwise go to the next fragment.
         */
        size_t frag_length = (list->fl_elem_size * list->fl_num_per_alloc + 
                              + sizeof(ompi_free_list_memory_t) +
                              list->fl_header_space + list->fl_alignment);

        if( position->last_item < (position->last_memory + frag_length) ) {
            *return_item = (opal_list_item_t*)position->last_item;
            return 0;
        }
    }

    /* we're outside the fragment. Try to go to the next one ... */
    if( opal_list_get_end(&(list->fl_allocations)) ==
        ((opal_list_item_t*)position->last_memory)->opal_list_next ) {
        *return_item = NULL;
        return 0;  /* nothing anymore */
    }

    position->last_memory = (unsigned char*)((opal_list_item_t*)position->last_memory)->opal_list_next;
    goto dig_for_the_requests;
}

/**
 * This function resize the free_list to contain at least the specified
 * number of elements. We do not create all of them in the same memory
 * segment. Instead we will several time the fl_num_per_alloc elements
 * until we reach the required number of the maximum allowed by the 
 * initialization.
 */
int
ompi_free_list_resize(ompi_free_list_t* flist, size_t size)
{
    ssize_t inc_num;
    int ret = OMPI_SUCCESS;

    if (flist->fl_num_allocated > size) {
        return OMPI_SUCCESS;
    }
    OPAL_THREAD_LOCK(&((flist)->fl_lock));
    inc_num = size - flist->fl_num_allocated;
    while( inc_num > 0 ) {
        ret = ompi_free_list_grow(flist, flist->fl_num_per_alloc);
        if( OMPI_SUCCESS != ret ) break;
        inc_num = size - flist->fl_num_allocated;
    }
    OPAL_THREAD_UNLOCK(&((flist)->fl_lock));

    return ret;
}
