/*
 * $HEADER$
 */

#include "mem/allocator.h"
#include "mem/sharedmem_util.h"

void *ompi_allocator_malloc(ompi_allocator_t *allocator, size_t chunk_size);
void ompi_allocator_default_free(ompi_allocator_t *allocator, void *base_ptr);

static void ompi_allocator_construct(ompi_allocator_t *allocator)
{
    allocator->alc_alloc_fn = ompi_allocator_malloc;
    allocator->alc_free_fn = ompi_allocator_free;
    allocator->alc_is_shared = 0;
    allocator->alc_mem_prot = 0;
    allocator->alc_should_pin = 0;
    allocator->alc_pinned_offset = 0;
    allocator->alc_pinned_sz = 0;
}

static void ompi_allocator_destruct(ompi_allocator_t *allocator)
{
}

ompi_class_t ompi_allocator_t_class = {
    "ompi_allocator_t",
    OBJ_CLASS(ompi_object_t), 
    (ompi_construct_t) ompi_allocator_construct,
    (ompi_destruct_t) ompi_allocator_destruct
};


void *ompi_alg_get_chunk(size_t chunk_size, int is_shared,
                    int mem_protect)
{
    if ( !is_shared )
        return malloc(chunk_size);
    else
    {
        return ompi_zero_alloc(chunk_size, mem_protect, MMAP_SHARED_FLAGS);
    }
}


void *ompi_allocator_alloc(ompi_allocator_t *allocator, size_t chunk_size)
{
    return allocator->alc_alloc_fn(allocator, chunk_size);
}

void ompi_allocator_free(ompi_allocator_t *allocator, void *chunk_ptr)
{
    if ( chunk_ptr )
        allocator->alc_free_fn(allocator, chunk_ptr);
}

void *ompi_allocator_malloc(ompi_allocator_t *allocator, size_t chunk_size)
{
    return malloc(chunk_size);
}

void ompi_allocator_default_free(ompi_allocator_t *allocator, void *chunk_ptr)
{
    if ( chunk_ptr )
        free(chunk_ptr);
}

