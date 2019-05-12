/*
 * Copyright (c) 2019      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "oshmem_config.h"

#include "oshmem/mca/sshmem/sshmem.h"
#include "oshmem/include/shmemx.h"
#include "oshmem/mca/sshmem/base/base.h"

#include "sshmem_ucx.h"

#define SSHMEM_UCX_SHADOW_ELEM_FLAG_FREE     0x1

typedef struct sshmem_ucx_shadow_alloc_elem {
    unsigned                       flags;
    unsigned                       block_size;
} sshmem_ucx_shadow_alloc_elem_t;

struct sshmem_ucx_shadow_allocator {
    size_t                         num_elems;
    sshmem_ucx_shadow_alloc_elem_t elems[];
};

static int sshmem_ucx_shadow_is_free(sshmem_ucx_shadow_alloc_elem_t *elem)
{
    return elem->flags & SSHMEM_UCX_SHADOW_ELEM_FLAG_FREE;
}

static void sshmem_ucx_shadow_set_elem(sshmem_ucx_shadow_alloc_elem_t *elem,
                                       unsigned flags, unsigned block_size)
{
    elem->flags      = flags;
    elem->block_size = block_size;
}

sshmem_ucx_shadow_allocator_t *sshmem_ucx_shadow_create(unsigned count)
{
    sshmem_ucx_shadow_allocator_t *allocator;

    allocator = calloc(1, sizeof(*allocator) +
                       count * sizeof(*allocator->elems));
    if (allocator) {
        /* initialization: set initial element to the whole buffer */
        sshmem_ucx_shadow_set_elem(&allocator->elems[0],
                                   SSHMEM_UCX_SHADOW_ELEM_FLAG_FREE, count);
        allocator->num_elems = count;
    }

    return allocator;
}

void sshmem_ucx_shadow_destroy(sshmem_ucx_shadow_allocator_t *allocator)
{
    free(allocator); /* no leak check. TODO add leak warnings/debug */
}

int sshmem_ucx_shadow_alloc(sshmem_ucx_shadow_allocator_t *allocator,
                            unsigned count, unsigned *index)
{
    sshmem_ucx_shadow_alloc_elem_t *end = &allocator->elems[allocator->num_elems];
    sshmem_ucx_shadow_alloc_elem_t *elem;

    assert(count > 0);

    for (elem = &allocator->elems[0]; elem < end; elem += elem->block_size) {
        if (sshmem_ucx_shadow_is_free(elem) && (elem->block_size >= count)) {
            /* found suitable free element */
            if (elem->block_size > count) {
                /* create new 'free' element for tail of current buffer */
                sshmem_ucx_shadow_set_elem(elem + count,
                                           SSHMEM_UCX_SHADOW_ELEM_FLAG_FREE,
                                           elem->block_size - count);
            }

            /* set the size and flags of the allocated element */
            sshmem_ucx_shadow_set_elem(elem, 0, count);
            *index = elem - &allocator->elems[0];
            return OSHMEM_SUCCESS;
        }
    }

    return OSHMEM_ERR_OUT_OF_RESOURCE;
}

static void sshmem_ucx_shadow_merge_blocks(sshmem_ucx_shadow_allocator_t *allocator)
{
    sshmem_ucx_shadow_alloc_elem_t *elem = &allocator->elems[0];
    sshmem_ucx_shadow_alloc_elem_t *end  = &allocator->elems[allocator->num_elems];
    sshmem_ucx_shadow_alloc_elem_t *next_elem;

    while ( (next_elem = (elem + elem->block_size)) < end) {
        if (sshmem_ucx_shadow_is_free(elem) && sshmem_ucx_shadow_is_free(next_elem)) {
            /* current & next elements are free, should be merged */
            elem->block_size += next_elem->block_size;
            /* clean element which is merged */
            sshmem_ucx_shadow_set_elem(next_elem, 0, 0);
        } else {
            elem = next_elem;
        }
    }
}

int sshmem_ucx_shadow_free(sshmem_ucx_shadow_allocator_t *allocator,
                           unsigned index)
{
    sshmem_ucx_shadow_alloc_elem_t *elem = &allocator->elems[index];

    elem->flags |= SSHMEM_UCX_SHADOW_ELEM_FLAG_FREE;
    sshmem_ucx_shadow_merge_blocks(allocator);
    return OSHMEM_SUCCESS;
}

size_t sshmem_ucx_shadow_size(sshmem_ucx_shadow_allocator_t *allocator,
                              unsigned index)
{
    sshmem_ucx_shadow_alloc_elem_t *elem = &allocator->elems[index];

    assert(!sshmem_ucx_shadow_is_free(elem));
    return elem->block_size;
}
