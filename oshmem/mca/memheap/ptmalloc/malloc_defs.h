#ifndef _MALLOC_DEFS_H
#define _MALLOC_DEFS_H

#include "oshmem/runtime/runtime.h"

/* See malloc.c for detailed parameter description */
#define USE_SPIN_LOCKS  0
#define USE_DL_PREFIX   
#define ABORT   oshmem_shmem_abort(-2)
//#define ABORT   abort()
#define MORECORE   mca_memheap_ptmalloc_sbrk
#define MORECORE_CANNOT_TRIM
#define DL_HAVE_MMAP   0
#define DL_HAVE_MREMAP 0
#define malloc_getpagesize mca_memheap_ptmalloc_getpagesize()
#define REALLOC_ZERO_BYTES_FREES
#define ABORT_ON_ASSERT_FAILURE 1
/* next two are useful for debugging */
#define DL_DEBUG 0
#define FOOTERS 0
/* print error if *alloc() is called with incorrect params */
#define USAGE_ERROR_ACTION(m, p) do { printf("PTMALLOC: USAGE ERROR DETECTED: m=%p ptr=%p\n", (void*)m, (void*)p); } while (0)

int mca_memheap_ptmalloc_getpagesize(void);
void *mca_memheap_ptmalloc_sbrk(size_t size);

void* dlmalloc(size_t);
void dlfree(void*);
void* dlrealloc(void*, size_t);
void* dlmemalign(size_t, size_t);

#endif
