/*
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

#include "lam/mem/allocator.h"
#include "lam/mem/sharedmem_util.h"

void *lam_alc_malloc(lam_allocator_t *allocator, size_t chunk_size);
void lam_alc_default_free(lam_allocator_t *allocator, void *base_ptr);

lam_class_info_t      allocator_cls = {"lam_allocator_t", &object_cls, 
                                (class_init_t)lam_alc_init, (class_destroy_t)lam_obj_destroy};

void lam_alc_init(lam_allocator_t *allocator)
{
    SUPER_INIT(allocator, &object_cls);
    allocator->alc_alloc_fn = lam_alc_malloc;
    allocator->alc_free_fn = lam_alc_free;
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


void *lam_alc_alloc(lam_allocator_t *allocator, size_t chunk_size)
{
    return allocator->alc_alloc_fn(allocator, chunk_size);
}

void lam_alc_free(lam_allocator_t *allocator, void *chunk_ptr)
{
    if ( chunk_ptr )
        allocator->alc_free_fn(allocator, chunk_ptr);
}

void *lam_alc_malloc(lam_allocator_t *allocator, size_t chunk_size)
{
    return malloc(chunk_size);
}

void lam_alc_default_free(lam_allocator_t *allocator, void *chunk_ptr)
{
    if ( chunk_ptr )
        free(chunk_ptr);
}

