/* Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "oshmem_config.h"
#include "oshmem/proc/proc.h"
#include "oshmem/mca/spml/spml.h"
#include "oshmem/mca/memheap/memheap.h"
#include "oshmem/mca/memheap/buddy/memheap_buddy.h"
#include "oshmem/mca/memheap/buddy/memheap_buddy_component.h"
#include "oshmem/mca/memheap/base/base.h" 
#include "orte/mca/grpcomm/grpcomm.h"
#include "opal/class/opal_hash_table.h"
#include "opal/class/opal_object.h"
#include "orte/util/name_fns.h"

static int buddy_init(mca_memheap_buddy_module_t* buddy);

mca_memheap_buddy_module_t memheap_buddy = {
    { 
        &mca_memheap_buddy_component,
        mca_memheap_buddy_finalize,
        mca_memheap_buddy_alloc,
        mca_memheap_buddy_align,
        mca_memheap_buddy_realloc,
        mca_memheap_buddy_free,

        mca_memheap_buddy_private_alloc,
        mca_memheap_buddy_private_free,

        mca_memheap_base_get_cached_mkey,
        mca_memheap_base_get_mkey,
        mca_memheap_base_find_offset,
        mca_memheap_base_is_symmetric_addr,
        mca_memheap_modex_recv_all,
        
        0
    },
    1   /* priority */ 
};

/* Memory Heap Buddy Implementation */

/* Static inline functions */
static inline unsigned int bits_per_long(void)
{
    return BITS_PER_BYTE * sizeof(unsigned long);
}

static inline void bitmap_zero(unsigned long *dst, unsigned long nbits)
{
    unsigned long len = BITS_TO_LONGS(nbits) * sizeof(unsigned long);
    memset(dst, 0, len);
}

/*
 * WARNING: Non atomic version.
 */
static inline void __clear_bit(unsigned long nr, volatile void * addr)
{
    int *m = ((int *) addr) + (nr >> 5);
    *m &= ~(1 << (nr & 31));
}

/*
 * WARNING: non atomic version.
 */
static inline void __set_bit(unsigned long nr, volatile void * addr)
{
    int *m = ((int *) addr) + (nr >> 5);
    *m |= 1 << (nr & 31);
}

static inline int test_bit(int nr, const volatile void * addr)
{
    return (1UL & (((const int *) addr)[nr >> 5] >> (nr & 31))) != 0UL;
}

/*
 * __ffs - find first bit in word.
 * @word: The word to search
 *
 * Undefined if no bit exists, so code should check against 0 first.
 */
static inline __opal_attribute_always_inline__ unsigned long __ffs(unsigned long word)
{
    int num = 0;
#if SIZEOF_LONG == 8
        if ((word & 0xffffffff) == 0) {
            num += 32;
            word >>= 32;
        }
#endif

    if ((word & 0xffff) == 0) {
        num += 16;
        word >>= 16;
    }
    if ((word & 0xff) == 0) {
        num += 8;
        word >>= 8;
    }
    if ((word & 0xf) == 0) {
        num += 4;
        word >>= 4;
    }
    if ((word & 0x3) == 0) {
        num += 2;
        word >>= 2;
    }
    if ((word & 0x1) == 0)
    num += 1;
    return num;
}

/* round up to next power of two */
static inline unsigned memheap_buddy_find_order(unsigned long size)
{
    unsigned order;

    if (size & (size - 1))
        order = 1;
    else
        order = 0;

    while (size >>= 1) {
        order++;
    }
    return order;
}

/* 
 * find the first set bit in a memory region
 * @addr: The address to base the search on
 * @offset: The bitnumber to start searching at
 * @size: The maximum size to search
 */

static inline unsigned long find_next_bit(const unsigned long *addr,
                                          unsigned long size,
                                          unsigned long offset)
{
    const unsigned long *p = addr + BITOP_WORD(offset);
    unsigned long result = offset & ~(bits_per_long() - 1);
    unsigned long tmp;

    if (offset >= size)
        return size;
    size -= result;
    offset %= bits_per_long();
    if (offset) {
        tmp = *(p++);
        tmp &= (~0UL << offset);
        if (size < bits_per_long())
            goto found_first;
        if (tmp)
            goto found_middle;
        size -= bits_per_long();
        result += bits_per_long();
    }
    while (size & ~(bits_per_long() - 1)) {
        if ((tmp = *(p++)))
            goto found_middle;
        result += bits_per_long();
        size -= bits_per_long();
    }
    if (!size)
        return result;
    tmp = *p;

    found_first: tmp &= (~0UL >> (bits_per_long() - size));
    if (tmp == 0UL) /* Are any bits set? */
        return result + size; /* Nope. */
    found_middle: return result + __ffs(tmp);
}

/**
 * Initialize the Memory Heap 
 */
int mca_memheap_buddy_module_init(memheap_context_t *context)
{
    if (!context || !context->user_size || !context->private_size) {
        return OSHMEM_ERR_BAD_PARAM;
    }

    /* Construct a mutex object */
    OBJ_CONSTRUCT(&memheap_buddy.lock, opal_mutex_t);

    memheap_buddy.heap.max_order = memheap_log2(context->user_size);
    memheap_buddy.heap.min_order = MEMHEAP_BASE_MIN_ORDER;
    memheap_buddy.private_heap.max_order = memheap_log2(context->private_size);
    memheap_buddy.private_heap.min_order = MEMHEAP_BASE_MIN_ORDER;

    if (context->user_size != (1ULL << memheap_buddy.heap.max_order)) {
        MEMHEAP_VERBOSE(1,
                        "Memheap rounded to the nearest power of two: requested %llu bytes, allocated %llu bytes",
                        (unsigned long long)context->user_size, 1ULL << memheap_buddy.heap.max_order);
    }

    assert(context->private_size == (1ULL << memheap_buddy.private_heap.max_order));

    memheap_buddy.heap.symmetric_heap = context->user_base_addr;
    memheap_buddy.private_heap.symmetric_heap = context->private_base_addr;

    memheap_buddy.super.memheap_size = (1ULL << memheap_buddy.heap.max_order);

    MEMHEAP_VERBOSE(1,
                    "symmetric heap memory (user+private): %llu bytes",
                    (unsigned long long)(context->user_size + context->private_size));

    /* Initialize buddy allocator */
    if (OSHMEM_SUCCESS != buddy_init(&memheap_buddy)) {
        MEMHEAP_ERROR("Failed to setup MEMHEAP buddy allocator");
        goto err;
    }

    return OSHMEM_SUCCESS;

    err: mca_memheap_buddy_finalize();
    return OSHMEM_ERROR;
}

static int buddy_init(mca_memheap_buddy_module_t* buddy)
{
    unsigned long long total_size;
    unsigned i;
    unsigned long long s;

    /* Allocate and init Hashtable */
    memheap_buddy.heap.symmetric_heap_hashtable = OBJ_NEW(opal_hash_table_t);
    if (NULL == memheap_buddy.heap.symmetric_heap_hashtable) {
        MEMHEAP_ERROR("Opal failed to allocate hashtable object");
        goto err;
    }
    memheap_buddy.private_heap.symmetric_heap_hashtable =
            OBJ_NEW(opal_hash_table_t);
    if (NULL == memheap_buddy.private_heap.symmetric_heap_hashtable) {
        MEMHEAP_ERROR("Opal failed to allocate hashtable object");
        goto err;
    }

    opal_hash_table_init(memheap_buddy.heap.symmetric_heap_hashtable,
                         DEFAULT_HASHTABLE_SIZE);
    opal_hash_table_init(memheap_buddy.private_heap.symmetric_heap_hashtable,
                         DEFAULT_HASHTABLE_SIZE);
    /* Init Buddy Allocator */
    buddy->heap.bits = (unsigned long**) calloc((buddy->heap.max_order + 1),
                                                sizeof(unsigned long *));
    buddy->private_heap.bits =
            (unsigned long**) calloc((buddy->private_heap.max_order + 1),
                                     sizeof(unsigned long *));
    buddy->heap.num_free = (unsigned int*) calloc((buddy->heap.max_order + 1),
                                                  sizeof(unsigned int));
    buddy->private_heap.num_free =
            (unsigned int*) calloc((buddy->private_heap.max_order + 1),
                                   sizeof(unsigned int));
    if ((NULL == buddy->heap.bits) || (NULL == buddy->heap.num_free)
            || (NULL == buddy->private_heap.bits)
            || (NULL == buddy->private_heap.num_free)) {

        MEMHEAP_ERROR("Failed to allocate buddy allocator");
        goto err;
    }

    total_size = 0;
    for (i = buddy->heap.min_order; i <= buddy->heap.max_order; ++i) {
        s = BITS_TO_LONGS(1UL << (buddy->heap.max_order - i));
        MEMHEAP_VERBOSE(20,
                        "%d: (order=%d) allocating %llu longs (sizeof long = %d)",
                        i, buddy->heap.max_order, s, (int)sizeof(unsigned long));
        total_size += s * sizeof(unsigned long);
        buddy->heap.bits[i] = (unsigned long*) malloc(s
                * sizeof(unsigned long));
        if (NULL == buddy->heap.bits[i]) {
            MEMHEAP_ERROR("Failed to allocate buddy->allocator");
            goto err;
        }
        bitmap_zero(buddy->heap.bits[i], 1UL << (buddy->heap.max_order - i));
    }
    MEMHEAP_VERBOSE(5, "MEMHEAP metadata size = %llu bytes", total_size);

    total_size = 0;
    for (i = buddy->private_heap.min_order; i <= buddy->private_heap.max_order;
            ++i) {
        s = BITS_TO_LONGS(1UL << (buddy->private_heap.max_order - i));
        MEMHEAP_VERBOSE(20,
                        "%d: (order=%d) allocating %llu longs (sizeof long = %d)",
                        i, buddy->private_heap.max_order, s, (int)sizeof(unsigned long));
        total_size += s * sizeof(unsigned long);
        buddy->private_heap.bits[i] = (unsigned long*) malloc(s
                * sizeof(unsigned long));
        if (NULL == buddy->private_heap.bits[i]) {
            MEMHEAP_ERROR("Failed to allocate buddy->allocator");
            goto err;
        }
        bitmap_zero(buddy->private_heap.bits[i],
                    1UL << (buddy->private_heap.max_order - i));
    }
    MEMHEAP_VERBOSE(5,
                    "private MEMHEAP metadata size = %llu bytes",
                    total_size);

    set_bit(0, buddy->heap.bits[buddy->heap.max_order]);
    set_bit(0, buddy->private_heap.bits[buddy->private_heap.max_order]);
    buddy->heap.num_free[buddy->heap.max_order] = 1;
    buddy->private_heap.num_free[buddy->private_heap.max_order] = 1;

    return OSHMEM_SUCCESS;

    err: return OSHMEM_ERROR;
}

static int buddy_cleanup(mca_memheap_buddy_module_t* buddy)
{
    unsigned int i;

    MEMHEAP_VERBOSE(5, "buddy cleanup");
    if (NULL == buddy) {
        return OSHMEM_SUCCESS;
    }

    for (i = 0; i <= buddy->heap.max_order; ++i) {
        if (NULL != buddy->heap.bits && NULL != buddy->heap.bits[i]) {
            free(buddy->heap.bits[i]);
        }
    }

    for (i = 0; i <= buddy->private_heap.max_order; ++i) {
        if (NULL != buddy->private_heap.bits
                && NULL != buddy->private_heap.bits[i]) {
            free(buddy->private_heap.bits[i]);
        }
    }

    if (NULL != buddy->heap.bits) {
        free(buddy->heap.bits);
    }
    if (NULL != buddy->heap.num_free) {
        free(buddy->heap.num_free);
    }

    if (NULL != buddy->private_heap.bits) {
        free(buddy->private_heap.bits);
    }
    if (NULL != buddy->private_heap.num_free) {
        free(buddy->private_heap.num_free);
    }

    OBJ_DESTRUCT(&buddy->lock);
    return OSHMEM_SUCCESS;
}

static int _buddy_alloc(unsigned order,
                        uint32_t* seg,
                        mca_memheap_buddy_heap_t *heap)
{
    uint32_t o;
    uint32_t m;

    MEMHEAP_VERBOSE(20, "order=%d size=%d", order, 1<<order);
    OPAL_THREAD_LOCK(&memheap_buddy.lock);
    for (o = order; o <= heap->max_order; ++o) {
        if (heap->num_free[o]) {
            m = 1 << (heap->max_order - o);
            *seg = find_first_bit(heap->bits[o], m);
            MEMHEAP_VERBOSE(20,
                            "found free bit: order=%d, bits=0x%lx m=%d, *seg=%d",
                            o, heap->bits[o][0], m, *seg);
            if (*seg < m)
                goto found;
        }
    }

    OPAL_THREAD_UNLOCK(&memheap_buddy.lock);
    return OSHMEM_ERROR;

    found:
    clear_bit(*seg, heap->bits[o]);
    --(heap->num_free[o]);

    while (o > order) {
        --o;
        *seg <<= 1;
        set_bit(*seg ^ 1, heap->bits[o]);
        ++(heap->num_free[o]);
    }

    OPAL_THREAD_UNLOCK(&memheap_buddy.lock);
    *seg <<= order;

    return OSHMEM_SUCCESS;
}

static int _buddy_free(mca_memheap_buddy_module_t* buddy,
                       uint32_t seg,
                       unsigned order,
                       mca_memheap_buddy_heap_t *heap)
{
    MEMHEAP_VERBOSE(20, "order=%d size=%d seg=%d", order, 1<<order, seg);
    seg >>= order;
    OPAL_THREAD_LOCK(&buddy->lock);

    while (test_bit(seg ^ 1, heap->bits[order])) {
        clear_bit(seg ^ 1, heap->bits[order]);
        --(heap->num_free[order]);
        seg >>= 1;
        ++order;
    }

    set_bit(seg, heap->bits[order]);
    ++(heap->num_free[order]);
    OPAL_THREAD_UNLOCK(&buddy->lock);
    return OSHMEM_SUCCESS;
}

static int buddy_free(mca_memheap_buddy_module_t* buddy,
                      uint32_t seg,
                      unsigned order)
{
    return _buddy_free(buddy, seg, order, &buddy->heap);
}

static int buddy_private_free(mca_memheap_buddy_module_t* buddy,
                              uint32_t seg,
                              unsigned order)
{
    return _buddy_free(buddy, seg, order, &buddy->private_heap);
}

static int _do_alloc(uint32_t order,
                     void **p_buff,
                     mca_memheap_buddy_heap_t *heap)
{
    int rc;
    unsigned long base;
    uint32_t offset;
    unsigned long addr;

    if (order < heap->min_order)
        order = heap->min_order;

    *p_buff = 0;
    if (order > heap->max_order) {
        /* Test allocated size overflow */
        MEMHEAP_VERBOSE(5, "Allocation overflow of symmetric heap size");
        return OSHMEM_ERROR;
    }

    base = (unsigned long) heap->symmetric_heap;

    if (OSHMEM_SUCCESS != _buddy_alloc(order, &offset, heap)) {
        MEMHEAP_VERBOSE(5, "Buddy Allocator failed to return a base address");
        return OSHMEM_ERROR;
    }

    /* Save the order of the allocated variable */
    addr = base + offset;

    rc = opal_hash_table_set_value_uint64(heap->symmetric_heap_hashtable,
                                          addr,
                                          (void *) (unsigned long) order);

    if (OPAL_SUCCESS != rc) {
        MEMHEAP_VERBOSE(5, "Failed to insert order to hashtable");
        goto alloc_error;
    }

    *p_buff = (void*) addr;
    /* no barrier because it is not required by spec! */
    return OSHMEM_SUCCESS;

    alloc_error: _buddy_free(&memheap_buddy, offset, order, heap);
    return OSHMEM_ERROR;
}

static int do_alloc(uint32_t order, void **p_buff)
{
    return _do_alloc(order, p_buff, &(memheap_buddy.heap));
}

static int do_private_alloc(uint32_t order, void **p_buff)
{
    return _do_alloc(order, p_buff, &(memheap_buddy.private_heap));
}

/**
 * Allocate size bytes on the symmetric heap.
 * The allocated variable is aligned to its size.
 */
int mca_memheap_buddy_alloc(size_t size, void** p_buff)
{

    uint32_t order;

    order = memheap_buddy_find_order(size);

    return do_alloc(order, p_buff);
}

int mca_memheap_buddy_private_alloc(size_t size, void** p_buff)
{
    uint32_t order;
    int status = 0;
    order = memheap_buddy_find_order(size);

    status = do_private_alloc(order, p_buff);

    MEMHEAP_VERBOSE(20, "private alloc addr: %p", *p_buff);

    return status;
}

int mca_memheap_buddy_private_free(void* ptr)
{
    int rc;
    uint32_t offset;
    unsigned long addr;
    unsigned long base;
    void *order;

    if (0 == ptr) {
        return OSHMEM_SUCCESS;
    }

    base = (unsigned long) memheap_buddy.private_heap.symmetric_heap;
    addr = (unsigned long) ptr;
    offset = addr - base;

    rc =
            opal_hash_table_get_value_uint64(memheap_buddy.private_heap.symmetric_heap_hashtable,
                                             addr,
                                             &order);
    if (OPAL_SUCCESS != rc) {
        return OSHMEM_ERROR;
    }

    buddy_private_free(&memheap_buddy,
                       offset,
                       (unsigned) (unsigned long) order);
    opal_hash_table_remove_value_uint64(memheap_buddy.private_heap.symmetric_heap_hashtable,
                                        addr);

    return OSHMEM_SUCCESS;
}

int mca_memheap_buddy_align(size_t align, size_t size, void **p_buff)
{
    uint32_t order;

    if (align == 0) {
        *p_buff = 0;
        return OSHMEM_ERROR;
    }

    /* check that align is power of 2 */
    if (align & (align - 1)) {
        *p_buff = 0;
        return OSHMEM_ERROR;
    }

    order = memheap_buddy_find_order(size);
    if ((unsigned long) align > (1UL << order))
        order = memheap_buddy_find_order(align);

    return do_alloc(order, p_buff);
}

int mca_memheap_buddy_realloc(size_t new_size, void *p_buff, void **p_new_buff)
{
    int rc;
    unsigned long addr;
    void *order;
    size_t old_size;
    char *tmp_buf;

    /* equiv to alloc if old ptr is null */
    if (NULL == p_buff)
        return mca_memheap_buddy_alloc(new_size, p_new_buff);

    addr = (unsigned long) p_buff;

    rc =
            opal_hash_table_get_value_uint64(memheap_buddy.heap.symmetric_heap_hashtable,
                                             addr,
                                             &order);
    if (OPAL_SUCCESS != rc) {
        *p_new_buff = NULL;
        return OSHMEM_ERROR;
    }

    /* equiv to free if new_size is 0 */
    if (0 == new_size) {
        *p_new_buff = NULL;
        return mca_memheap_buddy_free(p_buff);
    }

    old_size = 1UL << (unsigned long) order;

    /* do nothing if new size is less then current size */
    if (new_size <= old_size) {
        *p_new_buff = p_buff;
        return OSHMEM_SUCCESS;
    }

    if (new_size > (1UL << memheap_buddy.heap.max_order)) {
        *p_new_buff = NULL;
        return OSHMEM_ERR_OUT_OF_RESOURCE;
    }

    if (old_size + new_size >= (1UL << memheap_buddy.heap.max_order)) {
        /* copy via temporary buffer */

        tmp_buf = (char *) malloc(old_size);
        if (!tmp_buf)
            return OSHMEM_ERR_OUT_OF_RESOURCE;
        memcpy(tmp_buf, p_buff, old_size);
        mca_memheap_buddy_free(p_buff);
    } else
        tmp_buf = p_buff;

    /* alloc and copy data to new buffer, free old one */
    rc = mca_memheap_buddy_alloc(new_size, p_new_buff);
    if (OSHMEM_SUCCESS != rc) {
        *p_new_buff = NULL;
        if (old_size + new_size >= (1UL << memheap_buddy.heap.max_order)
                && tmp_buf) {
            free(tmp_buf);
        }
        return rc;
    }

    memcpy(*p_new_buff, tmp_buf, old_size);

    if (old_size + new_size < (1UL << memheap_buddy.heap.max_order))
        mca_memheap_buddy_free(p_buff);
    else if (tmp_buf)
        free(tmp_buf);

    return OSHMEM_SUCCESS;
}

/*
 * Free a variable allocated on the
 * symmetric heap.
 */
int mca_memheap_buddy_free(void* ptr)
{
    int rc;
    uint32_t offset;
    unsigned long addr;
    unsigned long base;
    void *order;

    base = (unsigned long) memheap_buddy.heap.symmetric_heap;
    addr = (unsigned long) ptr;
    offset = addr - base;

    rc =
            opal_hash_table_get_value_uint64(memheap_buddy.heap.symmetric_heap_hashtable,
                                             addr,
                                             &order);
    if (OPAL_SUCCESS != rc) {
        return OSHMEM_ERROR;
    }

    buddy_free(&memheap_buddy, offset, (unsigned) (unsigned long) order);
    opal_hash_table_remove_value_uint64(memheap_buddy.heap.symmetric_heap_hashtable,
                                        addr);

    return OSHMEM_SUCCESS;
}

int mca_memheap_buddy_finalize()
{
    MEMHEAP_VERBOSE(5, "deregistering symmetric heap");

    /* was not initialized - do nothing */
    if (memheap_buddy.heap.max_order == 0)
        return OSHMEM_SUCCESS;

    /* Destruct hashtable supporting shfree of symmetric heap variables */
    if (memheap_buddy.heap.symmetric_heap_hashtable) {
        OBJ_RELEASE(memheap_buddy.heap.symmetric_heap_hashtable);
    }
    if (memheap_buddy.private_heap.symmetric_heap_hashtable) {
        OBJ_RELEASE(memheap_buddy.private_heap.symmetric_heap_hashtable);
    }

    buddy_cleanup(&memheap_buddy);

    return OSHMEM_SUCCESS;
}

