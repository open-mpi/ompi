/*
 * $HEADER$
 */

#include "lam/mem/allocator.h"
#include "lam/mem/sharedmem_util.h"

void *lam_allocator_malloc(lam_allocator_t *allocator, size_t chunk_size);
void lam_allocator_default_free(lam_allocator_t *allocator, void *base_ptr);

lam_class_info_t      allocator_cls = {"lam_allocator_t", &lam_object_cls, 
                                (class_init_t)lam_allocator_init, (class_destroy_t)lam_obj_destroy};

void lam_allocator_init(lam_allocator_t *allocator)
{
    SUPER_INIT(allocator, &lam_object_cls);
    allocator->alc_alloc_fn = lam_allocator_malloc;
    allocator->alc_free_fn = lam_allocator_free;
    allocator->alc_is_shared = 0;
    allocator->alc_mem_prot = 0;
    allocator->alc_should_pin = 0;
    allocator->alc_pinned_offset = 0;
    allocator->alc_pinned_sz = 0;
}

void *lam_alg_get_chunk(size_t chunk_size, int is_shared,
                    int mem_protect)
{
    if ( !is_shared )
        return malloc(chunk_size);
    else
    {
        return lam_zero_alloc(chunk_size, mem_protect, MMAP_SHARED_FLAGS);
    }
}


void *lam_allocator_alloc(lam_allocator_t *allocator, size_t chunk_size)
{
    return allocator->alc_alloc_fn(allocator, chunk_size);
}

void lam_allocator_free(lam_allocator_t *allocator, void *chunk_ptr)
{
    if ( chunk_ptr )
        allocator->alc_free_fn(allocator, chunk_ptr);
}

void *lam_allocator_malloc(lam_allocator_t *allocator, size_t chunk_size)
{
    return malloc(chunk_size);
}

void lam_allocator_default_free(lam_allocator_t *allocator, void *chunk_ptr)
{
    if ( chunk_ptr )
        free(chunk_ptr);
}

