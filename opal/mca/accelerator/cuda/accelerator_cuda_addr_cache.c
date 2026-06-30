/*
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <cuda.h>
#include <stdlib.h>
#include <string.h>

#include "accelerator_cuda_addr_cache.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_object.h"
#include "opal/class/opal_rb_tree.h"
#include "opal/mca/accelerator/accelerator.h"
#include "opal/mca/threads/mutex.h"
#include "opal/memoryhooks/memory.h"
#include "opal/util/output.h"

#define DEFAULT_MAX_ENTRIES 4096

bool     opal_accelerator_cuda_addr_cache_enabled       = true;
int      opal_accelerator_cuda_addr_cache_max_entries   = DEFAULT_MAX_ENTRIES;
uint64_t opal_accelerator_cuda_addr_cache_hits          = 0;
uint64_t opal_accelerator_cuda_addr_cache_misses        = 0;
uint64_t opal_accelerator_cuda_addr_cache_stale_evictions = 0;
uint64_t opal_accelerator_cuda_addr_cache_lru_evictions = 0;

/*
 * Implementation notes:
 *
 *  - The store is an rb_tree keyed by the range base. The comparator
 *    treats the lookup key (a probe address) as "in range" when it
 *    falls within [base, base+size). This works because we maintain
 *    the invariant that ranges in the tree never overlap — overlapping
 *    inserts evict colliding entries first.
 *
 *  - A doubly-linked LRU list runs in parallel for eviction. The list
 *    is protected by the same mutex; no separate lock.
 *
 *  - One global mutex. check_addr is on the per-message hot path, so a
 *    rwlock would be a clear win, but the OMPI thread layer does not
 *    expose one yet. Revisit if profiles show contention. Practically
 *    the critical sections are short (tree walk + 1 driver call on hit,
 *    tree update on miss/insert).
 */

typedef struct addr_cache_entry_s {
    /* tree key: const void *base. Stored here so we can re-derive on evict. */
    const void *base;
    size_t      size;
    opal_accelerator_cuda_addr_class_t cls;
    /* LRU list links */
    struct addr_cache_entry_s *lru_prev;
    struct addr_cache_entry_s *lru_next;
} addr_cache_entry_t;

static opal_rb_tree_t    s_tree;
static opal_mutex_t      s_lock;
static addr_cache_entry_t *s_lru_head = NULL;  /* most recently used */
static addr_cache_entry_t *s_lru_tail = NULL;  /* least recently used */
static size_t            s_n_entries = 0;
static size_t            s_max_entries = DEFAULT_MAX_ENTRIES;
static bool              s_initialized = false;
static bool              s_release_hook_registered = false;

/*
 * Comparator for range-membership lookup.
 *
 * key1 is the probe address (a const void *) passed to opal_rb_tree_find.
 * key2 is the entry's base pointer (since we insert with base as the key).
 * On lookup we need to know the entry's full extent, but the rb_tree only
 * gives us the key, so for lookup we use a separate compfn that knows how
 * to dereference the entry.
 *
 * The trick: we insert the entry's address (not its base) as the key, so
 * key2 in the lookup compfn is the entry pointer. That lets the comparator
 * do range membership against entry->base / entry->size.
 */
static int range_compare_for_lookup(void *probe, void *entry_ptr)
{
    const char         *p = (const char *) probe;
    addr_cache_entry_t *e = (addr_cache_entry_t *) entry_ptr;
    if (p < (const char *) e->base)             return -1;
    if (p >= (const char *) e->base + e->size)  return  1;
    return 0;
}

/*
 * Comparator for non-overlapping insertion ordering. key1 and key2 are
 * both addr_cache_entry_t * (entry pointers). We order by base address;
 * since ranges are non-overlapping after eviction, base ordering is
 * total.
 */
static int entry_compare_for_insert(void *a, void *b)
{
    addr_cache_entry_t *ea = (addr_cache_entry_t *) a;
    addr_cache_entry_t *eb = (addr_cache_entry_t *) b;
    if (ea->base < eb->base) return -1;
    if (ea->base > eb->base) return  1;
    return 0;
}

/* LRU helpers; caller must hold s_lock. */
static inline void lru_unlink(addr_cache_entry_t *e)
{
    if (e->lru_prev) e->lru_prev->lru_next = e->lru_next;
    else             s_lru_head = e->lru_next;
    if (e->lru_next) e->lru_next->lru_prev = e->lru_prev;
    else             s_lru_tail = e->lru_prev;
    e->lru_prev = e->lru_next = NULL;
}

static inline void lru_push_front(addr_cache_entry_t *e)
{
    e->lru_prev = NULL;
    e->lru_next = s_lru_head;
    if (s_lru_head) s_lru_head->lru_prev = e;
    s_lru_head = e;
    if (NULL == s_lru_tail) s_lru_tail = e;
}

static inline void lru_touch(addr_cache_entry_t *e)
{
    if (s_lru_head == e) return;
    lru_unlink(e);
    lru_push_front(e);
}

static void evict_entry_locked(addr_cache_entry_t *e)
{
    /* tree key is the entry pointer itself (see entry_compare_for_insert) */
    opal_rb_tree_delete(&s_tree, e);
    lru_unlink(e);
    s_n_entries--;
    free(e);
}

static void evict_lru_locked(void)
{
    if (NULL == s_lru_tail) return;
    addr_cache_entry_t *victim = s_lru_tail;
    evict_entry_locked(victim);
    opal_accelerator_cuda_addr_cache_lru_evictions++;
}

/*
 * On opal_mem_hooks release: any cached range overlapping [buf, buf+length)
 * is now invalid. Walk and evict.
 */
static void mem_release_callback(void *buf, size_t length, void *cbdata, bool from_alloc)
{
    (void) cbdata;
    (void) from_alloc;
    opal_accelerator_cuda_addr_cache_invalidate(buf, length);
}

int opal_accelerator_cuda_addr_cache_init(void)
{
    if (s_initialized) {
        return OPAL_SUCCESS;
    }

    OBJ_CONSTRUCT(&s_lock, opal_mutex_t);

    s_max_entries = (opal_accelerator_cuda_addr_cache_max_entries > 0)
                        ? (size_t) opal_accelerator_cuda_addr_cache_max_entries
                        : DEFAULT_MAX_ENTRIES;

    int rc = opal_rb_tree_init(&s_tree, entry_compare_for_insert);
    if (OPAL_SUCCESS != rc) {
        OBJ_DESTRUCT(&s_lock);
        return rc;
    }

    /* Best-effort munmap notifications. Without OPAL_MEMORY_FREE_SUPPORT we
     * still work — host (negative) entries become slightly stale-prone, but
     * device entries are still validated by BUFFER_ID. */
    if (opal_mem_hooks_support_level() & OPAL_MEMORY_FREE_SUPPORT) {
        if (OPAL_SUCCESS == opal_mem_hooks_register_release(mem_release_callback, NULL)) {
            s_release_hook_registered = true;
        }
    }

    s_initialized = true;
    return OPAL_SUCCESS;
}

void opal_accelerator_cuda_addr_cache_finalize(void)
{
    if (!s_initialized) return;

    if (s_release_hook_registered) {
        opal_mem_hooks_unregister_release(mem_release_callback);
        s_release_hook_registered = false;
    }

    /* Drain the LRU list, freeing each entry. The rb_tree's nodes hold no
     * separately-allocated memory beyond the value pointers, so destroying
     * the tree after this is sufficient. */
    OPAL_THREAD_LOCK(&s_lock);
    while (NULL != s_lru_head) {
        addr_cache_entry_t *e = s_lru_head;
        evict_entry_locked(e);
    }
    OPAL_THREAD_UNLOCK(&s_lock);

    opal_rb_tree_destroy(&s_tree);
    OBJ_DESTRUCT(&s_lock);
    s_initialized = false;
}

int opal_accelerator_cuda_addr_cache_lookup(const void *addr,
                                            opal_accelerator_cuda_addr_class_t *out)
{
    if (!opal_accelerator_cuda_addr_cache_enabled || !s_initialized) {
        return 0;
    }

    opal_accelerator_cuda_addr_class_t snapshot;

    OPAL_THREAD_LOCK(&s_lock);
    addr_cache_entry_t *e = (addr_cache_entry_t *)
        opal_rb_tree_find_with(&s_tree, (void *) addr, range_compare_for_lookup);
    if (NULL == e) {
        opal_accelerator_cuda_addr_cache_misses++;
        OPAL_THREAD_UNLOCK(&s_lock);
        return 0;
    }

    /* Snapshot the classification and promote it in the LRU list while the
     * entry is guaranteed live (we hold the lock). Count the hit optimistically;
     * it is undone below if device revalidation fails. After this unlock we must
     * not dereference 'e', as a concurrent insert/evict/finalize may free it. */
    snapshot = e->cls;
    lru_touch(e);
    opal_accelerator_cuda_addr_cache_hits++;
    OPAL_THREAD_UNLOCK(&s_lock);

    /* Host entries carry no BUFFER_ID and rely solely on mem_hooks
     * invalidation, so the snapshot is authoritative. */
    if (!snapshot.has_buffer_id) {
        *out = snapshot;
        return 1;
    }

    /* Device entries: revalidate the BUFFER_ID outside the lock so the driver
     * round-trip does not serialize concurrent classifications. A mismatch
     * means the allocation was freed and its virtual address reused. */
    unsigned long long cur_id = 0;
    CUresult r = cuPointerGetAttribute(&cur_id, CU_POINTER_ATTRIBUTE_BUFFER_ID,
                                       (CUdeviceptr) addr);
    if (CUDA_SUCCESS == r && (uint64_t) cur_id == snapshot.buffer_id) {
        *out = snapshot;
        return 1;
    }

    /* Stale: undo the optimistic hit and evict the entry if it is still the
     * one we validated (it may already be gone or replaced). Re-find by addr
     * rather than reusing the now-untrusted 'e' pointer. */
    OPAL_THREAD_LOCK(&s_lock);
    opal_accelerator_cuda_addr_cache_hits--;
    addr_cache_entry_t *stale = (addr_cache_entry_t *)
        opal_rb_tree_find_with(&s_tree, (void *) addr, range_compare_for_lookup);
    if (NULL != stale && stale->cls.has_buffer_id
        && stale->cls.buffer_id == snapshot.buffer_id) {
        evict_entry_locked(stale);
        opal_accelerator_cuda_addr_cache_stale_evictions++;
    }
    opal_accelerator_cuda_addr_cache_misses++;
    OPAL_THREAD_UNLOCK(&s_lock);
    return 0;
}

void opal_accelerator_cuda_addr_cache_insert(const void *base, size_t size,
                                             const opal_accelerator_cuda_addr_class_t *cls)
{
    if (!opal_accelerator_cuda_addr_cache_enabled || !s_initialized) {
        return;
    }
    if (NULL == base || 0 == size || NULL == cls) {
        return;
    }

    addr_cache_entry_t *e = (addr_cache_entry_t *) malloc(sizeof(*e));
    if (NULL == e) return;
    e->base = base;
    e->size = size;
    e->cls  = *cls;
    e->lru_prev = e->lru_next = NULL;

    OPAL_THREAD_LOCK(&s_lock);

    /* Evict any existing overlap. We can only find one overlap per probe
     * (ranges are non-overlapping by invariant), so re-probing in a loop
     * handles the (rare) case where multiple existing entries fall inside
     * the new range. */
    for (;;) {
        addr_cache_entry_t *ov = (addr_cache_entry_t *)
            opal_rb_tree_find_with(&s_tree, (void *) base, range_compare_for_lookup);
        if (NULL == ov) break;
        evict_entry_locked(ov);
    }

    if (s_n_entries >= s_max_entries) {
        evict_lru_locked();
    }

    if (OPAL_SUCCESS != opal_rb_tree_insert(&s_tree, e, e)) {
        free(e);
        OPAL_THREAD_UNLOCK(&s_lock);
        return;
    }
    lru_push_front(e);
    s_n_entries++;

    OPAL_THREAD_UNLOCK(&s_lock);
}

void opal_accelerator_cuda_addr_cache_invalidate(const void *addr, size_t size)
{
    if (!s_initialized || NULL == addr || 0 == size) return;

    const char *lo = (const char *) addr;
    const char *hi = lo + size;

    OPAL_THREAD_LOCK(&s_lock);

    /*
     * Walk the LRU list (cheap, already O(N)) and evict every entry whose
     * range overlaps [addr, addr+size). The rb_tree doesn't support range
     * iteration directly. The release callback is on the slow path, so
     * O(N) is acceptable; if it ever shows up in profiles, switch the
     * store to an explicit interval tree.
     */
    addr_cache_entry_t *e = s_lru_head;
    while (NULL != e) {
        addr_cache_entry_t *next = e->lru_next;
        const char *e_lo = (const char *) e->base;
        const char *e_hi = e_lo + e->size;
        if (e_lo < hi && e_hi > lo) {
            evict_entry_locked(e);
        }
        e = next;
    }

    OPAL_THREAD_UNLOCK(&s_lock);
}
