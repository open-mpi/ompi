/*
 * $HEADER$
 */

#ifndef SHAREDMEM_UTIL_H
#define SHAREDMEM_UTIL_H

#include <sys/types.h>
#include <sys/mman.h>

#define MMAP_SHARED_PROT PROT_READ|PROT_WRITE
#define MMAP_PRIVATE_PROT PROT_READ|PROT_WRITE
#define MMAP_PRIVATE_FLAGS MAP_PRIVATE

#ifndef __osf__
# define MMAP_SHARED_FLAGS MAP_SHARED
#else
# define MMAP_SHARED_FLAGS MAP_SHARED|MAP_ANONYMOUS
#endif

void *lam_zero_alloc(size_t len, int mem_prot, int mem_flags);

#endif  /* SHAREDMEM_UTIL_H */

