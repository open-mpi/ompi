/*
 * $HEADER$
 */

#ifndef LAM_ALLOCATOR_H
#define LAM_ALLOCATOR_H

#include "lam/lfc/object.h"

/* This class is used to provide a generic and flexible
    way for the mem pool to allocate memory.  It's meant
    to be derived for device-dependent logic, e.g. GM.

    You should be able to share allocators, but then
    you will need to protect with a lock.
*/

/*
 *  Base allocator is a wrapper for malloc
 */

typedef struct lam_allocator
{
    lam_object_t    super;
    int             alc_is_shared;      /* indicates whether to get shared memory */
    int             alc_mem_prot;       /* memory protection for shared mem */
    int             alc_should_pin;     /* should pin memory when allocating */
    uint64_t        alc_pinned_offset;  /* pinned memory offset */
    uint64_t        alc_pinned_sz;      /* pinned mem size (may be different from alloc size. */
    void            *(*alc_alloc_fn)(struct lam_allocator *, size_t);
    void            (*alc_free_fn)(struct lam_allocator *, void *);
} lam_allocator_t;

extern lam_class_info_t lam_allocator_t_class_info;

void lam_allocator_construct(lam_allocator_t *allocator);

void *lam_alg_get_chunk(size_t chunk_size, int is_shared,
                           int mem_protect);

void *lam_allocator_alloc(lam_allocator_t *allocator, size_t chunk_size);
void lam_allocator_free(lam_allocator_t *allocator, void *chunk_ptr);

static inline int lam_allocator_get_is_shared(lam_allocator_t *allocator) {return allocator->alc_is_shared;}
static inline void lam_allocator_set_is_shared(lam_allocator_t *allocator, int is_shared) {allocator->alc_is_shared = is_shared;}

static inline int lam_allocator_get_mem_prot(lam_allocator_t *allocator) {return allocator->alc_mem_prot;}
static inline void lam_allocator_set_mem_prot(lam_allocator_t *allocator, int mem_prot) {allocator->alc_mem_prot = mem_prot;}

static inline int lam_allocator_get_should_pin(lam_allocator_t *allocator) {return allocator->alc_should_pin;}
static inline void lam_allocator_set_should_pin(lam_allocator_t *allocator, int pin) {allocator->alc_should_pin = pin;}

static inline uint64_t lam_allocator_get_pin_offset(lam_allocator_t *allocator) {return allocator->alc_pinned_offset;}
static inline void lam_allocator_set_pin_offset(lam_allocator_t *allocator, uint64_t pin_offset)
                    {allocator->alc_pinned_offset = pin_offset;}

static inline uint64_t lam_allocator_get_pin_size(lam_allocator_t *allocator) {return allocator->alc_pinned_sz;}
static inline void lam_allocator_set_pin_size(lam_allocator_t *allocator, uint64_t pin_sz)
                    {allocator->alc_pinned_sz = pin_sz;}

#endif  /* LAM_ALLOCATOR_H */

