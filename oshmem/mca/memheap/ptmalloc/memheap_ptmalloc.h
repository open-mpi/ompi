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
#ifndef MCA_MEMHEAP_PTMALLOC_H
#define MCA_MEMHEAP_PTMALLOC_H

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

BEGIN_C_DECLS

#include "malloc_defs.h"
/*
 * At the moment we use only dlmalloc part of the ptmalloc3. Thread safety is implemented by using locks on 
 * alloc operations. Since all shmem alloc ops are collectives, malloc performance is not a problem. So it makes 
 * sense to use simpler algorithm. 
 *
 * Heap is allocate in one chunk, and we implement our on sbrk like function that serves portions of the memory
 * to malloc. 
 *
 * At the moment we do not support growing/returning heap based memory to OS.  
 */

/* Structure for managing shmem symmetric heap */
struct mca_memheap_ptmalloc_module_t {
    mca_memheap_base_module_t super;
    int priority; /** Module's Priority */
    void *base;
    size_t cur_size;
    size_t max_size;
    size_t max_alloc_size;
    opal_mutex_t lock; /** Part of the allocator */
};

typedef struct mca_memheap_ptmalloc_module_t mca_memheap_ptmalloc_module_t;
OSHMEM_DECLSPEC extern mca_memheap_ptmalloc_module_t memheap_ptmalloc;

/*
 * Buddy interface. 
 * Please pay attention to the new differences in the interface. 
 */
OSHMEM_DECLSPEC extern int mca_memheap_ptmalloc_module_init(memheap_context_t *);
OSHMEM_DECLSPEC extern int mca_memheap_ptmalloc_alloc(size_t, void**);
OSHMEM_DECLSPEC extern int mca_memheap_ptmalloc_realloc(size_t, void*, void **);
OSHMEM_DECLSPEC extern int mca_memheap_ptmalloc_align(size_t, size_t, void**);
OSHMEM_DECLSPEC extern int mca_memheap_ptmalloc_free(void*);
OSHMEM_DECLSPEC extern int mca_memheap_ptmalloc_finalize(void);

END_C_DECLS

#endif /* MCA_MEMHEAP_BUDDY_H */
