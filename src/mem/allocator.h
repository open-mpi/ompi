/*
 * $HEADER$
 */

#ifndef OMPI_ALLOCATOR_H
#define OMPI_ALLOCATOR_H

#include "class/ompi_object.h"

/*
 * This class is used to provide a generic and flexible way for the
 * mem pool to allocate memory.  It's meant to be derived for
 * device-dependent logic, e.g. GM.
 *
 * You should be able to share allocators, but then you will need to
 * protect with a lock.
 */

/*
 * Base allocator is a wrapper for malloc
 */

typedef struct ompi_allocator {
    ompi_object_t super;
    int alc_is_shared;          /* indicates whether to get shared memory */
    int alc_mem_prot;           /* memory protection for shared mem */
    int alc_should_pin;         /* should pin memory when allocating */
    uint64_t alc_pinned_offset; /* pinned memory offset */
    uint64_t alc_pinned_sz;     /* pinned mem size (may be different from alloc size. */
    void *(*alc_alloc_fn) (struct ompi_allocator *, size_t);
    void (*alc_free_fn) (struct ompi_allocator *, void *);
} ompi_allocator_t;

extern ompi_class_t ompi_allocator_t_class;

void *ompi_alg_get_chunk(size_t chunk_size, int is_shared, int mem_protect);

void *ompi_allocator_alloc(ompi_allocator_t *allocator, size_t chunk_size);
void ompi_allocator_free(ompi_allocator_t *allocator, void *chunk_ptr);

static inline int ompi_allocator_get_is_shared(ompi_allocator_t *allocator)
{
    return allocator->alc_is_shared;
}

static inline void ompi_allocator_set_is_shared(ompi_allocator_t *allocator,
                                               int is_shared)
{
    allocator->alc_is_shared = is_shared;
}

static inline int ompi_allocator_get_mem_prot(ompi_allocator_t *allocator)
{
    return allocator->alc_mem_prot;
}

static inline void ompi_allocator_set_mem_prot(ompi_allocator_t *allocator,
                                              int mem_prot)
{
    allocator->alc_mem_prot = mem_prot;
}

static inline int ompi_allocator_get_should_pin(ompi_allocator_t *allocator)
{
    return allocator->alc_should_pin;
}

static inline void ompi_allocator_set_should_pin(ompi_allocator_t *allocator,
                                                int pin)
{
    allocator->alc_should_pin = pin;
}

static inline uint64_t ompi_allocator_get_pin_offset(ompi_allocator_t
                                                    *allocator)
{
    return allocator->alc_pinned_offset;
}

static inline void ompi_allocator_set_pin_offset(ompi_allocator_t *allocator,
                                                uint64_t pin_offset)
{
    allocator->alc_pinned_offset = pin_offset;
}

static inline uint64_t ompi_allocator_get_pin_size(ompi_allocator_t
                                                  *allocator)
{
    return allocator->alc_pinned_sz;
}

static inline void ompi_allocator_set_pin_size(ompi_allocator_t *allocator,
                                              uint64_t pin_sz)
{
    allocator->alc_pinned_sz = pin_sz;
}

#endif                          /* OMPI_ALLOCATOR_H */
