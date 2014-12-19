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
 * Copyright (c) 2010      IBM Corporation.  All rights reserved.
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_FREE_LIST_H
#define OMPI_FREE_LIST_H

#include "opal_config.h"
#include "opal/class/opal_lifo.h"
#include "opal/prefetch.h"
#include "opal/threads/condition.h"
#include "opal/constants.h"
#include "opal/runtime/opal.h"

BEGIN_C_DECLS

struct mca_mem_pool_t;
struct ompi_free_list_item_t;
    
typedef void (*ompi_free_list_item_init_fn_t) (
        struct ompi_free_list_item_t*, void* ctx);

struct ompi_free_list_t
{
    opal_lifo_t super;
    size_t fl_max_to_alloc;
    size_t fl_num_allocated;
    size_t fl_num_per_alloc;
    size_t fl_num_waiting;
    size_t fl_frag_size;                /* size of the fragment descriptor */
    size_t fl_frag_alignment;           /* fragment descriptor alignment */
    size_t fl_payload_buffer_size;      /* size of payload buffer */
    size_t fl_payload_buffer_alignment; /* payload buffer alignment */
    opal_class_t* fl_frag_class;
    struct mca_mpool_base_module_t* fl_mpool;
    opal_mutex_t fl_lock;
    opal_condition_t fl_condition; 
    opal_list_t fl_allocations;
    ompi_free_list_item_init_fn_t item_init;
    void* ctx;
};
typedef struct ompi_free_list_t ompi_free_list_t;
OPAL_DECLSPEC OBJ_CLASS_DECLARATION(ompi_free_list_t);

struct mca_mpool_base_registration_t;
struct ompi_free_list_item_t
{ 
    opal_list_item_t super; 
    struct mca_mpool_base_registration_t *registration;
    void *ptr;
}; 
typedef struct ompi_free_list_item_t ompi_free_list_item_t; 
OPAL_DECLSPEC OBJ_CLASS_DECLARATION(ompi_free_list_item_t);

/**
 * Initialize a free list.
 *
 * @param free_list (IN)           Free list.
 * @param element_size (IN)        Size of each element.
 * @param element_class (IN)       opal_class_t of element - used to initialize allocated elements.
 * @param num_elements_to_alloc    Initial number of elements to allocate.
 * @param max_elements_to_alloc    Maximum number of elements to allocate.
 * @param num_elements_per_alloc   Number of elements to grow by per allocation.
 * @param mpool                    Optional memory pool for allocation.s
 */
 
OPAL_DECLSPEC int ompi_free_list_init_ex(
    ompi_free_list_t *free_list, 
    size_t element_size,
    size_t alignment,
    opal_class_t* element_class,
    int num_elements_to_alloc,
    int max_elements_to_alloc,
    int num_elements_per_alloc,
    struct mca_mpool_base_module_t*,
    ompi_free_list_item_init_fn_t item_init,
    void *ctx
    );

static inline int ompi_free_list_init(
    ompi_free_list_t *free_list, 
    size_t element_size,
    opal_class_t* element_class,
    int num_elements_to_alloc,
    int max_elements_to_alloc,
    int num_elements_per_alloc,
    struct mca_mpool_base_module_t* mpool)
{
    return ompi_free_list_init_ex(free_list, element_size, opal_cache_line_size,
            element_class, num_elements_to_alloc, max_elements_to_alloc,
                                  num_elements_per_alloc, mpool, NULL, NULL);
}

/**
 * Initialize a free list.  - this will replace ompi_free_list_init_ex
 *
 * @param free_list                (IN)  Free list.
 * @param frag_size                (IN)  Size of each element - allocated by malloc.
 * @param frag_alignment           (IN)  Fragment alignment.
 * @param frag_class               (IN)  opal_class_t of element - used to initialize allocated elements.
 * @param payload_buffer_size      (IN)  Size of payload buffer - allocated from mpool.
 * @param payload_buffer_alignment (IN)  Payload buffer alignment.
 * @param num_elements_to_alloc    (IN)  Initial number of elements to allocate.
 * @param max_elements_to_alloc    (IN)  Maximum number of elements to allocate.
 * @param num_elements_per_alloc   (IN)  Number of elements to grow by per allocation.
 * @param mpool                    (IN)  Optional memory pool for allocation.s
 * @param item_init                (IN)
 * @param ctx                      (IN)
 */
 
OPAL_DECLSPEC int ompi_free_list_init_ex_new(
    ompi_free_list_t *free_list, 
    size_t frag_size,
    size_t frag_alignment,
    opal_class_t* frag_class,
    size_t payload_buffer_size,
    size_t payload_buffer_alignment,
    int num_elements_to_alloc,
    int max_elements_to_alloc,
    int num_elements_per_alloc,
    struct mca_mpool_base_module_t*,
    ompi_free_list_item_init_fn_t item_init,
    void *ctx
    );

/**
 * Initialize a free list. - this will replace ompi_free_list_init
 *
 * @param free_list                (IN)  Free list.
 * @param frag_size                (IN)  Size of each element - allocated by malloc.
 * @param frag_alignment           (IN)  Fragment alignment.
 * @param frag_class               (IN)  opal_class_t of element - used to initialize allocated elements.
 * @param payload_buffer_size      (IN)  Size of payload buffer - allocated from mpool.
 * @param payload_buffer_alignment (IN)  Payload buffer alignment.
 * @param num_elements_to_alloc    (IN)  Initial number of elements to allocate.
 * @param max_elements_to_alloc    (IN)  Maximum number of elements to allocate.
 * @param num_elements_per_alloc   (IN)  Number of elements to grow by per allocation.
 * @param mpool                    (IN)  Optional memory pool for allocation.s
 */
static inline int ompi_free_list_init_new(
    ompi_free_list_t *free_list, 
    size_t frag_size,
    size_t frag_alignment,
    opal_class_t* frag_class,
    size_t payload_buffer_size,
    size_t payload_buffer_alignment,
    int num_elements_to_alloc,
    int max_elements_to_alloc,
    int num_elements_per_alloc,
    struct mca_mpool_base_module_t* mpool)
{
    return ompi_free_list_init_ex_new(free_list,
            frag_size, frag_alignment, frag_class,
            payload_buffer_size, payload_buffer_alignment,
            num_elements_to_alloc, max_elements_to_alloc,
            num_elements_per_alloc, mpool, NULL, NULL);
}

OPAL_DECLSPEC int ompi_free_list_grow(ompi_free_list_t* flist, size_t num_elements);

/* Grow the free list to be *at least* size elements.  This function
   will not shrink the list if it is already larger than size and may
   grow it past size if necessary (it will grow in
   num_elements_per_alloc chunks) */
OPAL_DECLSPEC int ompi_free_list_resize_mt(ompi_free_list_t *flist, size_t size);

/**
 * Attemp to obtain an item from a free list. 
 *
 * @param fl (IN)        Free list.
 * @param item (OUT)     Allocated item.
 *
 * If the requested item is not available the free list is grown to 
 * accomodate the request - unless the max number of allocations has 
 * been reached.  If this is the case - a NULL pointer is returned 
 * to the caller.
 */

#define OMPI_FREE_LIST_GET_MT(fl, item)                                 \
    {                                                                   \
        item = (ompi_free_list_item_t*) opal_lifo_pop (&((fl)->super)); \
        if( OPAL_UNLIKELY(NULL == item) ) {                             \
            if(opal_using_threads()) {                                  \
                opal_mutex_lock(&((fl)->fl_lock));                      \
                ompi_free_list_grow((fl), (fl)->fl_num_per_alloc);      \
                opal_mutex_unlock(&((fl)->fl_lock));                    \
            } else {                                                    \
                ompi_free_list_grow((fl), (fl)->fl_num_per_alloc);      \
            }                                                           \
            item = (ompi_free_list_item_t*) opal_lifo_pop(&((fl)->super)); \
        }                                                               \
    } 

/**
 * Blocking call to obtain an item from a free list.
 *
 * @param fl (IN)        Free list.
 * @param item (OUT)     Allocated item.
 *
 * If the requested item is not available the free list is grown to 
 * accomodate the request - unless the max number of allocations has 
 * been reached. In this case the caller is blocked until an item
 * is returned to the list.
 */

#define OMPI_FREE_LIST_WAIT_MT(fl, item)           \
    __ompi_free_list_wait_mt( (fl), &(item) )

static inline void __ompi_free_list_wait_mt( ompi_free_list_t* fl,
                                             ompi_free_list_item_t** item )
{
    *item = (ompi_free_list_item_t*)opal_lifo_pop(&((fl)->super));
    while( NULL == *item ) {
        if( !OPAL_THREAD_TRYLOCK(&((fl)->fl_lock)) ) {
            if((fl)->fl_max_to_alloc <= (fl)->fl_num_allocated) {
                (fl)->fl_num_waiting++;
                opal_condition_wait(&((fl)->fl_condition), &((fl)->fl_lock));
                (fl)->fl_num_waiting--;
            } else {
                if(ompi_free_list_grow((fl), (fl)->fl_num_per_alloc)
                        == OPAL_SUCCESS) {
                    if( 0 < (fl)->fl_num_waiting ) {
                        if( 1 == (fl)->fl_num_waiting ) {
                            opal_condition_signal(&((fl)->fl_condition));
                        } else {
                            opal_condition_broadcast(&((fl)->fl_condition));
                        }
                    }
                } else {
                    (fl)->fl_num_waiting++;
                    opal_condition_wait(&((fl)->fl_condition), &((fl)->fl_lock));
                    (fl)->fl_num_waiting--;
                }
            }
        } else {
            /* If I wasn't able to get the lock in the begining when I finaly grab it
             * the one holding the lock in the begining already grow the list. I will
             * release the lock and try to get a new element until I succeed.
             */
            OPAL_THREAD_LOCK(&((fl)->fl_lock));
        }
        OPAL_THREAD_UNLOCK(&((fl)->fl_lock));
        *item = (ompi_free_list_item_t*)opal_lifo_pop(&((fl)->super));
    }
} 

/**
 * Return an item to a free list. 
 *
 * @param fl (IN)        Free list.
 * @param item (OUT)     Allocated item.
 *
 */
 
#define OMPI_FREE_LIST_RETURN_MT(fl, item)                              \
    do {                                                                \
        opal_list_item_t* original;                                     \
                                                                        \
        original = opal_lifo_push( &(fl)->super, &(item)->super);       \
        if( &(fl)->super.opal_lifo_ghost == original ) {                \
            OPAL_THREAD_LOCK(&(fl)->fl_lock);                           \
            if((fl)->fl_num_waiting > 0) {                              \
                if( 1 == (fl)->fl_num_waiting ) {                       \
                    opal_condition_signal(&((fl)->fl_condition));       \
                } else {                                                \
                    opal_condition_broadcast(&((fl)->fl_condition));    \
                }                                                       \
            }                                                           \
            OPAL_THREAD_UNLOCK(&(fl)->fl_lock);                         \
        }                                                               \
    } while(0)
    
END_C_DECLS
#endif 

