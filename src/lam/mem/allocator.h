/*
 * $HEADER$
 *
 * Copyright 2002-2003. The Regents of the University of California. This material
 * was produced under U.S. Government contract W-7405-ENG-36 for Los Alamos
 * National Laboratory, which is operated by the University of California for
 * the U.S. Department of Energy. The Government is granted for itself and
 * others acting on its behalf a paid-up, nonexclusive, irrevocable worldwide
 * license in this material to reproduce, prepare derivative works, and
 * perform publicly and display publicly. Beginning five (5) years after
 * October 10,2002 subject to additional five-year worldwide renewals, the
 * Government is granted for itself and others acting on its behalf a paid-up,
 * nonexclusive, irrevocable worldwide license in this material to reproduce,
 * prepare derivative works, distribute copies to the public, perform publicly
 * and display publicly, and to permit others to do so. NEITHER THE UNITED
 * STATES NOR THE UNITED STATES DEPARTMENT OF ENERGY, NOR THE UNIVERSITY OF
 * CALIFORNIA, NOR ANY OF THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR
 * IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY,
 * COMPLETENESS, OR USEFULNESS OF ANY INFORMATION, APPARATUS, PRODUCT, OR
 * PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD NOT INFRINGE PRIVATELY
 * OWNED RIGHTS.
                                                                                                     
 * Additionally, this program is free software; you can distribute it and/or
 * modify it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2 of the License,
 * or any later version.  Accordingly, this program is distributed in the hope
 * that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

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

extern lam_class_info_t      allocator_cls;

void lam_alc_init(lam_allocator_t *allocator);

void *lam_alg_get_chunk(size_t chunk_size, int is_shared,
                           int mem_protect);

void *lam_alc_alloc(lam_allocator_t *allocator, size_t chunk_size);
void lam_alc_free(lam_allocator_t *allocator, void *chunk_ptr);

static inline int lam_alc_get_is_shared(lam_allocator_t *allocator) {return allocator->alc_is_shared;}
static inline void lam_alc_set_is_shared(lam_allocator_t *allocator, int is_shared) {allocator->alc_is_shared = is_shared;}

static inline int lam_alc_get_mem_prot(lam_allocator_t *allocator) {return allocator->alc_mem_prot;}
static inline void lam_alc_set_mem_prot(lam_allocator_t *allocator, int mem_prot) {allocator->alc_mem_prot = mem_prot;}

static inline int lam_alc_get_should_pin(lam_allocator_t *allocator) {return allocator->alc_should_pin;}
static inline void lam_alc_set_should_pin(lam_allocator_t *allocator, int pin) {allocator->alc_should_pin = pin;}

static inline uint64_t lam_alc_get_pin_offset(lam_allocator_t *allocator) {return allocator->alc_pinned_offset;}
static inline void lam_alc_set_pin_offset(lam_allocator_t *allocator, uint64_t pin_offset)
                    {allocator->alc_pinned_offset = pin_offset;}

static inline uint64_t lam_alc_get_pin_size(lam_allocator_t *allocator) {return allocator->alc_pinned_sz;}
static inline void lam_alc_set_pin_size(lam_allocator_t *allocator, uint64_t pin_sz)
                    {allocator->alc_pinned_sz = pin_sz;}

#endif  /* LAM_ALLOCATOR_H */

