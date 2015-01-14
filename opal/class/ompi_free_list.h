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

#include "opal_free_list.h"

#define ompi_free_list_item_t opal_free_list_item_t

typedef void (*ompi_free_list_item_init_fn_t) (ompi_free_list_item_t*, void* ctx);

struct ompi_free_list_t {
    opal_free_list_t super;
    ompi_free_list_item_init_fn_t item_init;
    void *ctx;
};
typedef struct ompi_free_list_t ompi_free_list_t;
OPAL_DECLSPEC OBJ_CLASS_DECLARATION(ompi_free_list_t);

#define OMPI_FREE_LIST_GET_MT(fl, item)		\
    opal_free_list_get (&(fl)->super, &(item))

#define OMPI_FREE_LIST_WAIT_MT(fl, item)        \
    opal_free_list_wait (&(fl)->super, &(item))

#define OMPI_FREE_LIST_RETURN_MT(fl, item)      \
    opal_free_list_return (&(fl)->super, item)

static inline int ompi_free_list_item_init_compat (ompi_free_list_item_t *item,
                                                   void *ctx)
{
    ompi_free_list_t *free_list = (ompi_free_list_t *) ctx;

    free_list->item_init (item, free_list->ctx);
    return OPAL_SUCCESS;
}


static inline int __opal_attribute_deprecated__
ompi_free_list_init_ex_new (ompi_free_list_t *free_list,  size_t frag_size,
                            size_t frag_alignment, opal_class_t* frag_class,
                            size_t payload_buffer_size, size_t payload_buffer_alignment,
                            int num_elements_to_alloc, int max_elements_to_alloc,
                            int num_elements_per_alloc, struct mca_mpool_base_module_t *mpool,
                            ompi_free_list_item_init_fn_t item_init, void *ctx)
{
    free_list->item_init = item_init;
    free_list->ctx = ctx;

    return opal_free_list_init (&free_list->super, frag_size, frag_alignment,
                                frag_class, payload_buffer_size, payload_buffer_alignment,
                                num_elements_to_alloc, max_elements_to_alloc,
                                num_elements_per_alloc, mpool, 0, NULL,
                                ompi_free_list_item_init_compat,
                                (void *) free_list);
}

static inline int __opal_attribute_deprecated__
ompi_free_list_init_new (ompi_free_list_t *free_list,  size_t frag_size,
                         size_t frag_alignment, opal_class_t* frag_class,
                         size_t payload_buffer_size, size_t payload_buffer_alignment,
                         int num_elements_to_alloc,int max_elements_to_alloc,
                         int num_elements_per_alloc, struct mca_mpool_base_module_t* mpool)
{
    return opal_free_list_init (&free_list->super, frag_size, frag_alignment,
                                frag_class, payload_buffer_size,
                                payload_buffer_alignment, num_elements_to_alloc,
                                max_elements_to_alloc, num_elements_per_alloc,
                                mpool, 0, NULL, NULL, NULL);
}

static inline int __opal_attribute_deprecated__
ompi_free_list_init_ex (ompi_free_list_t *free_list, size_t element_size,
                        size_t alignment, opal_class_t* element_class,
                        int num_elements_to_alloc, int max_elements_to_alloc,
                        int num_elements_per_alloc, struct mca_mpool_base_module_t *mpool,
                        ompi_free_list_item_init_fn_t item_init, void *ctx)
{
    return ompi_free_list_init_ex_new (free_list, element_size, alignment,
                                       element_class, 0, 0, num_elements_to_alloc,
                                       max_elements_to_alloc, num_elements_per_alloc,
                                       mpool, item_init, ctx);
}

static inline int __opal_attribute_deprecated__
ompi_free_list_init (ompi_free_list_t *free_list,  size_t element_size,
                     opal_class_t* element_class, int num_elements_to_alloc,
                     int max_elements_to_alloc, int num_elements_per_alloc,
                     struct mca_mpool_base_module_t* mpool)
{
    return opal_free_list_init (&free_list->super, element_size, opal_cache_line_size,
                                element_class, 0, 0, num_elements_to_alloc,
                                max_elements_to_alloc, num_elements_per_alloc,
                                mpool, 0, NULL, NULL, NULL);
}

/* Grow the free list to be *at least* size elements.  This function
   will not shrink the list if it is already larger than size and may
   grow it past size if necessary (it will grow in
   num_elements_per_alloc chunks) */
static inline int __opal_attribute_deprecated__
ompi_free_list_resize_mt(ompi_free_list_t *flist, size_t size)
{
    return opal_free_list_resize_mt (&flist->super, size);
}

#endif
