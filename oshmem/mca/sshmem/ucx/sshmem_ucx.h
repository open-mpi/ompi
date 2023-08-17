/*
 * Copyright (c) 2017      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_SSHMEM_UCX_EXPORT_H
#define MCA_SSHMEM_UCX_EXPORT_H

#include "oshmem_config.h"

#include "oshmem/mca/sshmem/sshmem.h"

#include <ucp/api/ucp.h>

BEGIN_C_DECLS

typedef struct sshmem_ucx_shadow_allocator sshmem_ucx_shadow_allocator_t;

/**
 * globally exported variable to hold the ucx component.
 */
typedef struct mca_sshmem_ucx_component_t {
    /* base component struct */
    mca_sshmem_base_component_t super;
    /* priority for ucx component */
    int priority;
} mca_sshmem_ucx_component_t;

OSHMEM_DECLSPEC extern mca_sshmem_ucx_component_t
mca_sshmem_ucx_component;

typedef struct mca_sshmem_ucx_segment_context {
    sshmem_ucx_shadow_allocator_t  *shadow_allocator;
    ucp_mem_h                       ucp_memh;
} mca_sshmem_ucx_segment_context_t;

typedef struct mca_sshmem_ucx_module_t {
    mca_sshmem_base_module_t super;
} mca_sshmem_ucx_module_t;
extern mca_sshmem_ucx_module_t mca_sshmem_ucx_module;

sshmem_ucx_shadow_allocator_t *sshmem_ucx_shadow_create(unsigned count);
void sshmem_ucx_shadow_destroy(sshmem_ucx_shadow_allocator_t *allocator);
int sshmem_ucx_shadow_alloc(sshmem_ucx_shadow_allocator_t *allocator,
                            unsigned count, unsigned *index);

/* Reallocate existing allocated buffer. If possible - used inplace
 * reallocation.
 * Parameter 'inplace' - out, in case if zero - new buffer was allocated
 * (inplace is not possible), user should remove original buffer after data
 * is copied, else (if inplace == 0) - no additional action required */
int sshmem_ucx_shadow_realloc(sshmem_ucx_shadow_allocator_t *allocator,
                              unsigned count, unsigned old_index, unsigned *index,
                              int *inplace);
int sshmem_ucx_shadow_free(sshmem_ucx_shadow_allocator_t *allocator,
                           unsigned index);
unsigned sshmem_ucx_shadow_size(sshmem_ucx_shadow_allocator_t *allocator,
                                unsigned index);

END_C_DECLS

#endif /* MCA_SHMEM_UCX_EXPORT_H */
