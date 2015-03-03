/*
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
 */
#ifndef MCA_MEMHEAP_BASE_H
#define MCA_MEMHEAP_BASE_H

#include "oshmem_config.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_value_array.h"
#include "opal/mca/mca.h"

#include "oshmem/mca/memheap/memheap.h"

BEGIN_C_DECLS

/*
 * Global functions for MCA: overall MEMHEAP open and close
 */
OSHMEM_DECLSPEC int mca_memheap_base_select(void);

/*
 * Globals
 */
OSHMEM_DECLSPEC extern struct mca_memheap_base_module_t* mca_memheap_base_module_initialized;

/* only used within base -- no need to DECLSPEC */
#define MEMHEAP_BASE_MIN_ORDER         3                                /* forces 64 bit alignment */
#define MEMHEAP_BASE_PAGE_ORDER        21   
#define MEMHEAP_BASE_PRIVATE_SIZE      (1ULL << MEMHEAP_BASE_PAGE_ORDER) /* should be at least the same as a huge page size */
#define MEMHEAP_BASE_MIN_SIZE          (1ULL << MEMHEAP_BASE_PAGE_ORDER)    /* must fit into at least one huge page */

extern char* mca_memheap_base_include;
extern char* mca_memheap_base_exclude;
extern int mca_memheap_base_already_opened;
extern int mca_memheap_base_key_exchange;

#define MCA_MEMHEAP_MAX_SEGMENTS    256
#define HEAP_SEG_INDEX  0
#define SYMB_SEG_INDEX  1

typedef struct mca_memheap_map {  
    map_segment_t   mem_segs[MCA_MEMHEAP_MAX_SEGMENTS]; /* TODO: change into pointer array */
    int             n_segments;
    int             num_transports;
} mca_memheap_map_t;

extern mca_memheap_map_t mca_memheap_base_map;

int mca_memheap_base_alloc_init(mca_memheap_map_t *, size_t);
void mca_memheap_base_alloc_exit(mca_memheap_map_t *);
int mca_memheap_base_static_init(mca_memheap_map_t *);
void mca_memheap_base_static_exit(mca_memheap_map_t *);
int mca_memheap_base_reg(mca_memheap_map_t *);
int mca_memheap_base_dereg(mca_memheap_map_t *);
int memheap_oob_init(mca_memheap_map_t *);
void memheap_oob_destruct(void);

OSHMEM_DECLSPEC uint64_t mca_memheap_base_find_offset(int pe,
                                                      int tr_id,
                                                      void* va,
                                                      void* rva);
OSHMEM_DECLSPEC int mca_memheap_base_is_symmetric_addr(const void* va);
OSHMEM_DECLSPEC sshmem_mkey_t *mca_memheap_base_get_mkey(void* va,
                                                           int tr_id);
OSHMEM_DECLSPEC sshmem_mkey_t * mca_memheap_base_get_cached_mkey_slow(map_segment_t *s,
                                                                      int pe,
                                                                      void* va,
                                                                      int btl_id,
                                                                      void** rva);
OSHMEM_DECLSPEC void mca_memheap_modex_recv_all(void);

/* This function is for internal usage only
 * return value:
 * 0 - addr is not symmetric address
 * 1 - addr is part of user memheap
 * 2 - addr is part of private memheap
 * 3 - addr is static variable
 */
typedef enum {
    ADDR_INVALID = 0, ADDR_USER, ADDR_PRIVATE, ADDR_STATIC,
} addr_type_t;

OSHMEM_DECLSPEC int mca_memheap_base_detect_addr_type(void* va);

static inline unsigned memheap_log2(unsigned long long val)
{
    /* add 1 if val is NOT a power of 2 (to do the ceil) */
    unsigned int count = (val & (val - 1) ? 1 : 0);

    while (val > 0) {
        val = val >> 1;
        count++;
    }

    return count > 0 ? count - 1 : 0;
}

static inline void *memheap_down_align_addr(void* addr, unsigned int shift)
{
    return (void*) (((intptr_t) addr) & (~(intptr_t) 0) << shift);
}

static inline void *memheap_up_align_addr(void*addr, unsigned int shift)
{
    return (void*) ((((intptr_t) addr) | ~((~(intptr_t) 0) << shift)));
}

static inline unsigned long long memheap_align(unsigned long top)
{
    return ((top + MEMHEAP_BASE_MIN_SIZE - 1) & ~(MEMHEAP_BASE_MIN_SIZE - 1));
}

/*
 * MCA framework
 */
OSHMEM_DECLSPEC extern mca_base_framework_t oshmem_memheap_base_framework;

/* ******************************************************************** */
#ifdef __BASE_FILE__
#define __SPML_FILE__ __BASE_FILE__
#else
#define __SPML_FILE__ __FILE__
#endif

#ifdef OPAL_ENABLE_DEBUG
#define MEMHEAP_VERBOSE(level, ...) \
    oshmem_output_verbose(level, oshmem_memheap_base_framework.framework_output, \
        "%s:%d - %s()", __SPML_FILE__, __LINE__, __FUNCTION__, __VA_ARGS__)
#else
#define MEMHEAP_VERBOSE(level, ...)
#endif

#define MEMHEAP_ERROR(...) \
    oshmem_output(oshmem_memheap_base_framework.framework_output, \
        "Error %s:%d - %s()", __SPML_FILE__, __LINE__, __FUNCTION__, __VA_ARGS__)

#define MEMHEAP_WARN(...) \
    oshmem_output_verbose(0, oshmem_memheap_base_framework.framework_output, \
        "Warning %s:%d - %s()", __SPML_FILE__, __LINE__, __FUNCTION__, __VA_ARGS__)

extern int mca_memheap_seg_cmp(const void *k, const void *v);

/* Turn ON/OFF debug output from build (default 0) */
#ifndef MEMHEAP_BASE_DEBUG
#define MEMHEAP_BASE_DEBUG    0
#endif
#define MEMHEAP_VERBOSE_FASTPATH(...)

extern mca_memheap_map_t* memheap_map;

static inline map_segment_t *memheap_find_va(const void* va)
{
    map_segment_t *s;

    if (OPAL_LIKELY((uintptr_t)va >= (uintptr_t)memheap_map->mem_segs[HEAP_SEG_INDEX].seg_base_addr &&
                    (uintptr_t)va < (uintptr_t)memheap_map->mem_segs[HEAP_SEG_INDEX].end)) {
        s = &memheap_map->mem_segs[HEAP_SEG_INDEX];
    } else {
        s = bsearch(va,
                    &memheap_map->mem_segs[SYMB_SEG_INDEX],
                    memheap_map->n_segments - 1,
                    sizeof(*s),
                    mca_memheap_seg_cmp);
    }

#if MEMHEAP_BASE_DEBUG == 1
    if (s) {
        MEMHEAP_VERBOSE(5, "match seg#%02ld: 0x%llX - 0x%llX %llu bytes va=%p",
                s - memheap_map->mem_segs,
                (long long)s->seg_base_addr,
                (long long)s->end,
                (long long)(s->end - s->seg_base_addr),
                (void *)va);
    }
#endif
    return s;
}

static inline void* memheap_va2rva(void* va, void* local_base, void* remote_base)
{
    return (void*) (remote_base > local_base ?
            (uintptr_t)va + ((uintptr_t)remote_base - (uintptr_t)local_base) :
            (uintptr_t)va - ((uintptr_t)local_base - (uintptr_t)remote_base));
}

static inline  sshmem_mkey_t *mca_memheap_base_get_cached_mkey(int pe,
                                                                void* va,
                                                                int btl_id,
                                                                void** rva)
{
    map_segment_t *s;
    sshmem_mkey_t *mkey;

    MEMHEAP_VERBOSE_FASTPATH(10, "rkey: pe=%d va=%p", pe, va);
    s = memheap_find_va(va);
    if (OPAL_UNLIKELY(NULL == s))
        return NULL ;

    if (OPAL_UNLIKELY(!MAP_SEGMENT_IS_VALID(s)))
        return NULL ;

    if (OPAL_UNLIKELY(pe == oshmem_my_proc_id())) {
        *rva = va;
        MEMHEAP_VERBOSE_FASTPATH(10, "rkey: pe=%d va=%p -> (local) %lx %p", pe, va, 
                s->mkeys[btl_id].u.key, *rva);
        return &s->mkeys[btl_id];
    }

    if (OPAL_LIKELY(s->mkeys_cache[pe])) {
        mkey = &s->mkeys_cache[pe][btl_id];
        *rva = memheap_va2rva(va, s->seg_base_addr, mkey->va_base);
        MEMHEAP_VERBOSE_FASTPATH(10, "rkey: pe=%d va=%p -> (cached) %lx %p", pe, (void *)va, mkey->u.key, (void *)*rva);
        return mkey;
    }

    return mca_memheap_base_get_cached_mkey_slow(s, pe, va, btl_id, rva);
}

END_C_DECLS

#endif /* MCA_MEMHEAP_BASE_H */
