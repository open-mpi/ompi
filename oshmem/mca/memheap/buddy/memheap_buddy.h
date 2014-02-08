/**
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 * Description of the Registration Cache framework
 */
#ifndef MCA_MEMHEAP_BUDDY_H
#define MCA_MEMHEAP_BUDDY_H

#include "oshmem_config.h"
#include "opal/mca/mca.h"
#include "opal/class/opal_list.h" 
#include "opal/threads/mutex.h"
#include "oshmem/mca/memheap/memheap.h"
#include "oshmem/mca/memheap/base/base.h"
#include "oshmem/mca/spml/spml.h"
#include "oshmem/util/oshmem_util.h"
#include "opal/class/opal_hash_table.h"
#include "ompi/mca/btl/btl.h"
#include <string.h>
#include <sys/types.h>
#include <math.h>

#define BITS_PER_BYTE                    8
#define __BITOPS_WORDSIZE                64
#define DEFAULT_HASHTABLE_SIZE           100

#define BITOP_WORD(nr)                   ((nr) / bits_per_long())
#define DIV_ROUND_UP(n,d)                (((n) + (d) - 1) / (d))
#define BITS_TO_LONGS(nr)                DIV_ROUND_UP(nr, BITS_PER_BYTE * sizeof(unsigned long))
#define __BITOPS_WORDS(bits)             (((bits)+__BITOPS_WORDSIZE-1)/__BITOPS_WORDSIZE)
#define clear_bit(x,y)                   __clear_bit((x), (y))
#define set_bit(x,y)                     __set_bit((x), (y))
#define find_first_bit(addr, size)      find_next_bit((addr), (size), 0)

BEGIN_C_DECLS

struct mca_memheap_buddy_heap_t {
    unsigned long **bits; /** Part of the buddy allocator */
    unsigned *num_free; /** Part of the buddy allocator */
    unsigned max_order; /** Log2 of Maximal heap size, part of the allocator */
    unsigned min_order; /** min alloc order */
    void* symmetric_heap; /** Symmetric Heap */
    opal_hash_table_t* symmetric_heap_hashtable; /** Pointer to the Symmetric heap used for moving on it */
};
typedef struct mca_memheap_buddy_heap_t mca_memheap_buddy_heap_t;

/* Structure for managing shmem symmetric heap */
struct mca_memheap_buddy_module_t {
    mca_memheap_base_module_t super;

    int priority; /** Module's Priority */
    mca_memheap_buddy_heap_t heap;
    mca_memheap_buddy_heap_t private_heap;
    opal_mutex_t lock; /** Part of the buddy allocator */
};
typedef struct mca_memheap_buddy_module_t mca_memheap_buddy_module_t;
OSHMEM_DECLSPEC extern mca_memheap_buddy_module_t memheap_buddy;

/*
 * Buddy interface. 
 * Please pay attention to the new differences in the interface. 
 */
OSHMEM_DECLSPEC extern int mca_memheap_buddy_module_init(memheap_context_t *);
OSHMEM_DECLSPEC extern int mca_memheap_buddy_alloc(size_t, void**);
OSHMEM_DECLSPEC extern int mca_memheap_buddy_realloc(size_t, void*, void **);
OSHMEM_DECLSPEC extern int mca_memheap_buddy_align(size_t, size_t, void**);
OSHMEM_DECLSPEC extern int mca_memheap_buddy_free(void*);
OSHMEM_DECLSPEC extern int mca_memheap_buddy_finalize(void);

/* private alloc/free functions */
OSHMEM_DECLSPEC extern int mca_memheap_buddy_private_alloc(size_t, void**);
OSHMEM_DECLSPEC extern int mca_memheap_buddy_private_free(void*);

/**
 * static/global variables support. Consider making it a separate component 
 */

END_C_DECLS

#endif /* MCA_MEMHEAP_BUDDY_H */
