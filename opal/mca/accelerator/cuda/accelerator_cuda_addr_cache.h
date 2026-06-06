/*
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Address-classification cache for accelerator_cuda_check_addr.
 *
 * The cache stores the result of a previous classification keyed by
 * the allocation's base/size range. A successful lookup returns the
 * cached (mem_type, dev_id, flags) and avoids the up-to-three CUDA
 * driver calls that the cold path issues.
 *
 * Two correctness mechanisms guard against stale entries:
 *
 *   1. For CUDA-tracked pointers, each entry stores the
 *      CU_POINTER_ATTRIBUTE_BUFFER_ID at insert time. On lookup we
 *      re-query BUFFER_ID and evict if it has changed (allocation was
 *      freed and the address was reused).
 *
 *   2. For host pointers (caching a negative result), invalidation is
 *      driven by opal_mem_hooks_register_release: any munmap that
 *      overlaps a cached range evicts the affected entries.
 */

#ifndef OPAL_ACCELERATOR_CUDA_ADDR_CACHE_H
#define OPAL_ACCELERATOR_CUDA_ADDR_CACHE_H

#include "opal_config.h"

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

BEGIN_C_DECLS

typedef struct {
    int      mem_type;     /* check_addr return: 0 host, 1 device */
    int      dev_id;       /* MCA_ACCELERATOR_NO_DEVICE_ID for host */
    uint64_t flags;        /* MCA_ACCELERATOR_FLAGS_* (e.g. UNIFIED_MEMORY) */
    uint64_t buffer_id;    /* CU_POINTER_ATTRIBUTE_BUFFER_ID at insert; 0 for host */
    bool     has_buffer_id;/* false for host entries (no CUDA buffer id) */
} opal_accelerator_cuda_addr_class_t;

/* Lifecycle. Init is safe to call multiple times. */
int  opal_accelerator_cuda_addr_cache_init(void);
void opal_accelerator_cuda_addr_cache_finalize(void);

/*
 * Look up addr in the cache.
 *
 * Returns 1 on a verified hit (out is filled). Returns 0 on miss or
 * when the cache is disabled. Stale entries (BUFFER_ID mismatch) are
 * evicted as a side effect of a miss.
 */
int  opal_accelerator_cuda_addr_cache_lookup(const void *addr,
                                             opal_accelerator_cuda_addr_class_t *out);

/*
 * Insert a classification for the [base, base+size) range.
 *
 * base/size should typically come from cuMemGetAddressRange() so the
 * entry covers the whole allocation. For host (negative) caching,
 * pass the page-aligned region returned by opal_mem_hooks if known,
 * or a conservative single-page range.
 *
 * Idempotent: a duplicate or overlapping insert evicts any existing
 * overlapping entries first.
 */
void opal_accelerator_cuda_addr_cache_insert(const void *base, size_t size,
                                             const opal_accelerator_cuda_addr_class_t *cls);

/*
 * Evict any cached entries overlapping [addr, addr+size). Called from
 * the opal_mem_hooks release callback and may also be called directly
 * if the caller knows an allocation has been freed.
 */
void opal_accelerator_cuda_addr_cache_invalidate(const void *addr, size_t size);

/* Tunable, registered as accelerator_cuda_addr_cache_enable. Default true. */
extern bool opal_accelerator_cuda_addr_cache_enabled;

/* Tunable, registered as accelerator_cuda_addr_cache_size. Maximum number of
 * ranges retained before LRU eviction. Read once at init; values < 1 select
 * the built-in default. */
extern int opal_accelerator_cuda_addr_cache_max_entries;

/* Statistics, useful for debugging and tuning. Updated under the cache lock. */
extern uint64_t opal_accelerator_cuda_addr_cache_hits;
extern uint64_t opal_accelerator_cuda_addr_cache_misses;
extern uint64_t opal_accelerator_cuda_addr_cache_stale_evictions;
extern uint64_t opal_accelerator_cuda_addr_cache_lru_evictions;

END_C_DECLS

#endif /* OPAL_ACCELERATOR_CUDA_ADDR_CACHE_H */
