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

extern void* mca_memheap_base_start_address;
extern char* mca_memheap_base_include;
extern char* mca_memheap_base_exclude;
extern int mca_memheap_base_already_opened;
extern int mca_memheap_base_alloc_type;
extern int mca_memheap_base_key_exchange;
extern int mca_memheap_base_mr_interleave_factor;

#define MCA_MEMHEAP_MAX_SEGMENTS    256
#define HEAP_SEG_INDEX  0
#define SYMB_SEG_INDEX  1

#define MEMHEAP_SHM_INVALID         (-1)

#define MEMHEAP_SHM_CODE( type, id )        ((((uint64_t)(type)) << 32) | ((uint32_t)(id)))
#define MEMHEAP_SHM_GET_TYPE( x )           (((uint32_t)((x) >> 32)) & 0xFFFFFFFF)
#define MEMHEAP_SHM_GET_ID( x )             ((uint32_t)((x) & 0xFFFFFFFF))

typedef enum {
    MAP_SEGMENT_STATIC = 0,
    MAP_SEGMENT_ALLOC_MMAP,
    MAP_SEGMENT_ALLOC_SHM,
    MAP_SEGMENT_ALLOC_IBV,
    MAP_SEGMENT_UNKNOWN
} segment_type_t;

#if defined(MPAGE_ENABLE) && (MPAGE_ENABLE > 0)
#include <infiniband/verbs.h>

typedef struct openib_device_t {
    struct ibv_device **ib_devs;
    struct ibv_device *ib_dev;
    struct ibv_context *ib_dev_context;
    struct ibv_device_attr ib_dev_attr;
    struct ibv_pd *ib_pd;
    opal_value_array_t ib_mr_array;
    struct ibv_mr *ib_mr_shared;
} openib_device_t;
#endif /* MPAGE_ENABLE */

typedef struct map_segment_t {
    mca_spml_mkey_t **mkeys_cache;  /* includes remote segment bases in va_base */
    mca_spml_mkey_t *mkeys;         /* includes local segment bases in va_base */
    int             is_active;      /* enable/disable flag */
    int             shmid;

    void*           start;          /* base address of the segment */
    void*           end;            /* final address of the segment */
    size_t          size;           /* length of the segment */

    segment_type_t  type;           /* type of the segment */
    void            *context;       /* additional data related the segment */
} map_segment_t;

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
OSHMEM_DECLSPEC mca_spml_mkey_t *mca_memheap_base_get_mkey(void* va,
                                                           int tr_id);
OSHMEM_DECLSPEC mca_spml_mkey_t * mca_memheap_base_get_cached_mkey(int pe,
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
    oshmem_output_verbose(0, oshmem_memheap_base_framework.framework_output, \
        "Error %s:%d - %s()", __SPML_FILE__, __LINE__, __FUNCTION__, __VA_ARGS__)

#define MEMHEAP_WARN(...) \
    oshmem_output_verbose(0, oshmem_memheap_base_framework.framework_output, \
        "Error %s:%d - %s()", __SPML_FILE__, __LINE__, __FUNCTION__, __VA_ARGS__)

END_C_DECLS

#endif /* MCA_MEMHEAP_BASE_H */
