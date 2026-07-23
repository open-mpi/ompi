/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * The mca_base_event dispatch path (Phase 2): the instance pool, the
 * raise/dispatch algorithm, handle/callback/dropped-handler management,
 * timestamps, and the in-callback accessors.  See specs/mpi-t-events/spec.md
 * sections 5.9-5.11.
 *
 * This translation unit uses opal_free_list (instance pool), which depends on
 * the mpool base.  That dependency is absent from libopen-pal_core, so this
 * file is deliberately kept separate from mca_base_event.c (which is reachable
 * from core via mca_base_var's init/finalize): this file is pulled in only by
 * callers of raise/handle_alloc -- the OMPI MPI_T layer -- which link the full
 * library.
 */

#include "opal_config.h"

#include <stdint.h>
#include <string.h>
#include <time.h>

#include "opal/class/opal_free_list.h"
#include "opal/class/opal_hash_table.h"
#include "opal/mca/base/mca_base_event.h"
#include "opal/mca/base/mca_base_vari.h"
#include "opal/mca/threads/thread_usage.h"
#include "opal/util/clock_gettime.h"

/* OMPI-installed debug hook (sec. 5.10); NULL until/unless installed. */
void (*mca_base_event_debug_raise_check_fn)(void) = NULL;

/* Count of instance-pool acquisitions, for the white-box test to assert that an
   all-drops raise consumes no instance (sec. 12.4).  Atomic because it is bumped
   in the lock-free Step-3 delivery section, where concurrent raises would
   otherwise race on a plain counter. */
static opal_atomic_int64_t pool_acquire_count = 0;

int64_t mca_base_event_test_pool_acquires(void)
{
    return (int64_t) pool_acquire_count;
}

/* --- Instance pool (lazy) ------------------------------------------------ */

static opal_free_list_t instance_pool;
static bool             pool_created = false;

/* --- Default clock origin (lazy, captured at first listener-attach) ------ */

static struct timespec  clock_origin;
static bool             clock_origin_set = false;

/* --- Live-registration table + generation (sec. 5.10) -------------------- */

static opal_hash_table_t live_registrations;
static bool              live_table_created = false;
static int               live_registration_count = 0;

/* --- Per-thread dispatch-depth recursion cap (sec. 5.9 step 0); the cap
   constant MCA_BASE_EVENT_MAX_DISPATCH_DEPTH is in mca_base_event.h ---------- */

static opal_thread_local int dispatch_depth = 0;

/* Test-only seam: force the instance pool to appear exhausted, so the white-box
   test can exercise the drop-restore path with a production-sized pool (rather
   than a size-1 pool that would mask the recursion-depth cap). */
static bool force_pool_exhaustion = false;

void mca_base_event_test_set_force_exhaustion(bool v)
{
    force_pool_exhaustion = v;
}

/* ======================================================================== */
/* Saturating drop-count clamp (sec. 5.4)                                    */
/* ======================================================================== */

opal_count_t mca_base_event_saturate_to(int64_t value, int64_t ceiling)
{
    /* Compare at full width BEFORE narrowing, so a 32-bit ceiling on a 64-bit
       host clamps correctly and never wraps negative.  A negative `value` means
       the (lock-free) drop counter itself overflowed past INT64_MAX -- treat
       that as saturated rather than reporting a negative count. */
    if (value < 0 || value > ceiling) {
        return (opal_count_t) ceiling;
    }
    return (opal_count_t) value;
}

/* ======================================================================== */
/* Timestamps (sec. 5.11)                                                    */
/* ======================================================================== */

/* Default clock: nanoseconds since the lazily-captured origin, integer math.
   Matches the MPI_Wtime backend (opal_clock_gettime). */
static opal_count_t default_timestamp(void)
{
    struct timespec now;

    opal_clock_gettime(&now);
    return (opal_count_t) (now.tv_sec - clock_origin.tv_sec) * 1000000000
           + (opal_count_t) (now.tv_nsec - clock_origin.tv_nsec);
}

/* Read a source's timestamp, enforcing ORDERED monotonicity with an atomic
   CAS-max on last_ts (sec. 5.11) so two threads can never publish out of
   order and a backward-stepping wall clock cannot regress. */
static opal_count_t source_timestamp(mca_base_event_source_t *source)
{
    opal_count_t ts;

    if (NULL != source->get_timestamp) {
        ts = source->get_timestamp(source->ctx);
    } else {
        ts = default_timestamp();
    }

    if (MCA_BASE_EVENT_SOURCE_ORDERED == source->ordering) {
        int64_t prev = source->last_ts;
        while (ts > prev) {
            if (opal_atomic_compare_exchange_strong_64(&source->last_ts, &prev, (int64_t) ts)) {
                break;
            }
            /* prev was reloaded by the CAS; retry if still behind. */
        }
        if (ts < prev) {
            ts = (opal_count_t) prev; /* never go backwards */
        }
    }

    return ts;
}

/* Ad-hoc source timestamp read (sec. 5.8).  Implemented here (not in the core
   TU) because the default clock lives here. */
static void fold_event(mca_base_event_t *event);

int mca_base_event_source_get_timestamp(int source_index, opal_count_t *timestamp)
{
    mca_base_event_source_t *source;
    int rc;

    if (NULL == timestamp) {
        return OPAL_ERR_BAD_PARAM;
    }
    rc = mca_base_event_source_get_by_index(source_index, &source);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }
    if (!source->supports_adhoc) {
        return OPAL_ERR_NOT_SUPPORTED;
    }

    /* For a default-clock source the origin is normally captured on first
       listener attach (ensure_dispatch_state()).  A tool may read an ad-hoc
       timestamp before any handle is allocated, so capture it here too --
       otherwise default_timestamp() would subtract a zero origin and return a
       timestamp in the wrong epoch. */
    if (NULL == source->get_timestamp) {
        opal_mutex_lock(&mca_base_event_lock);
        if (!clock_origin_set) {
            opal_clock_gettime(&clock_origin);
            clock_origin_set = true;
        }
        opal_mutex_unlock(&mca_base_event_lock);
    }

    /* A read of a deferred-release source drains its accumulated releases and
       delivers them as one aggregated event (sec. 7.1); a no-op otherwise. */
    fold_event(source->fold_event);

    *timestamp = source_timestamp(source);
    return OPAL_SUCCESS;
}

/* ======================================================================== */
/* Pool lifecycle                                                            */
/* ======================================================================== */

/* Tear down the pool + dispatch state.  Registered with the core finalize via
   mca_base_event_set_dispatch_finalize() the first time the pool is created. */
static void dispatch_finalize(void)
{
    /* v1 limitation: any registration a tool failed to free before finalize
       leaks here (it remains on its event's registration list and in the live
       table).  Freeing a handle is the tool's responsibility (the standard
       treats use-after-finalize as erroneous); this is a one-time process-exit
       leak only. */
    if (pool_created) {
        OBJ_DESTRUCT(&instance_pool);
        pool_created = false;
    }
    if (live_table_created) {
        OBJ_DESTRUCT(&live_registrations);
        live_table_created = false;
    }
    /* Reset the DoS-guard counter together with the table it tracks: otherwise a
       handle leaked past finalize (acknowledged above) leaves the count nonzero,
       and across MPI_T init/finalize cycles it drifts up against a rebuilt-empty
       table until it reaches mca_base_event_max_registrations and every later
       handle_alloc fails with no handles actually live. */
    live_registration_count = 0;
    clock_origin_set = false;
}

/* Lazily create the pool, live table, and clock origin on first attach.
   MUST be called with mca_base_event_lock held. */
static int ensure_dispatch_state(void)
{
    int rc, pool_max;

    if (!clock_origin_set) {
        opal_clock_gettime(&clock_origin);
        clock_origin_set = true;
    }

    if (!live_table_created) {
        OBJ_CONSTRUCT(&live_registrations, opal_hash_table_t);
        rc = opal_hash_table_init(&live_registrations, 256);
        if (OPAL_SUCCESS != rc) {
            OBJ_DESTRUCT(&live_registrations);
            return rc;
        }
        live_table_created = true;
    }

    if (!pool_created) {
        int pool_grow;
        pool_max = mca_base_event_pool_max;
        if (pool_max < 1) {
            pool_max = 256; /* never unbounded (sec. 5.9) */
        }
        /* The grow quantum is independent of the pre-allocation count, so
           event_pool_size == 0 means "pre-allocate nothing, grow on demand"
           rather than "never grow" -- which (with a zero grow quantum) would
           leave the pool permanently empty and drop every event forever. */
        pool_grow = (mca_base_event_pool_size > 0) ? mca_base_event_pool_size : 8;
        OBJ_CONSTRUCT(&instance_pool, opal_free_list_t);
        rc = opal_free_list_init(&instance_pool, sizeof(mca_base_event_instance_t), 8,
                                 OBJ_CLASS(mca_base_event_instance_t), 0, 0,
                                 mca_base_event_pool_size, pool_max, pool_grow, NULL,
                                 0, NULL, NULL, NULL);
        if (OPAL_SUCCESS != rc) {
            OBJ_DESTRUCT(&instance_pool);
            return rc;
        }
        pool_created = true;
    }

    /* (Re-)register the teardown hook so finalize can reach us. */
    mca_base_event_set_dispatch_finalize(dispatch_finalize);

    return OPAL_SUCCESS;
}

/* ======================================================================== */
/* num_active state machine (sec. 5.10) -- caller holds the lock             */
/* ======================================================================== */

static bool reg_is_active(const mca_base_event_registration_t *reg)
{
    int l;

    if (NULL != reg->dropped_slot) {
        return true;
    }
    for (l = 0; l < MCA_BASE_EVENT_CB_REQUIRE_MAX; ++l) {
        if (NULL != reg->callbacks[l]) {
            return true;
        }
    }
    return false;
}

static void update_num_active(mca_base_event_registration_t *reg, bool was_active)
{
    bool now_active = reg_is_active(reg);
    mca_base_event_source_t *source = reg->event->source;
    int32_t n;

    if (was_active && !now_active) {
        n = opal_atomic_sub_fetch_32(&reg->event->num_active, 1);
        /* Last listener on this event detached: let a deferred-release source
           remove its OS hook (sec. 7.1).  Called under the lock. */
        if (0 == n && NULL != source && NULL != source->activate_fn) {
            source->activate_fn(source->ctx, false);
        }
    } else if (!was_active && now_active) {
        n = opal_atomic_add_fetch_32(&reg->event->num_active, 1);
        /* First listener attached: let a deferred-release source install its
           OS hook lazily (sec. 7.1).  Called under the lock. */
        if (1 == n && NULL != source && NULL != source->activate_fn) {
            source->activate_fn(source->ctx, true);
        }
    }
}

/* ======================================================================== */
/* Deferred-free helpers (sec. 5.10) -- caller holds the lock                */
/* ======================================================================== */

/* Objects whose refcount reached 0 while dead are collected here (via their
   own opal_list_item_t super, so the collection is UNBOUNDED -- a single
   reconcile can free arbitrarily many handles); their user callbacks +
   release_cbs + OBJ_RELEASE run AFTER the lock is dropped.  A dead reg has
   already been removed from event->registrations, and a dead slot is no longer
   referenced by reg->callbacks[]/dropped_slot, so reusing each object's super
   list item here is safe. */
typedef struct {
    opal_list_t cb_slots;
    opal_list_t dropped_slots;
    opal_list_t regs;
} to_free_t;

static void to_free_init(to_free_t *tf)
{
    OBJ_CONSTRUCT(&tf->cb_slots, opal_list_t);
    OBJ_CONSTRUCT(&tf->dropped_slots, opal_list_t);
    OBJ_CONSTRUCT(&tf->regs, opal_list_t);
}

static void release_cb_slot(mca_base_event_cb_t *slot, to_free_t *tf)
{
    if (NULL == slot) {
        return;
    }
    if (0 == opal_atomic_sub_fetch_32(&slot->refcount, 1) && slot->dead) {
        opal_list_append(&tf->cb_slots, &slot->super);
    }
}

static void release_dropped_slot(mca_base_event_dropped_cb_t *slot, to_free_t *tf)
{
    if (NULL == slot) {
        return;
    }
    if (0 == opal_atomic_sub_fetch_32(&slot->refcount, 1) && slot->dead) {
        opal_list_append(&tf->dropped_slots, &slot->super);
    }
}

static void release_reg(mca_base_event_registration_t *reg, to_free_t *tf)
{
    if (NULL == reg) {
        return;
    }
    if (0 == opal_atomic_sub_fetch_32(&reg->refcount, 1) && reg->dead) {
        opal_list_append(&tf->regs, &reg->super);
    }
}

/* Run the deferred frees collected in tf.  MUST be called with the lock NOT
   held (user callbacks + release_cbs run here).  Empties + destructs tf. */
static void process_to_free(to_free_t *tf)
{
    opal_list_item_t *item;

    while (NULL != (item = opal_list_remove_first(&tf->cb_slots))) {
        mca_base_event_cb_t *slot = (mca_base_event_cb_t *) item;
        if (NULL != slot->release_cb) {
            slot->release_cb(slot->user_data);
        }
        if (NULL != slot->info) {
            OBJ_RELEASE(slot->info);
        }
        OBJ_RELEASE(slot);
    }
    while (NULL != (item = opal_list_remove_first(&tf->dropped_slots))) {
        mca_base_event_dropped_cb_t *slot = (mca_base_event_dropped_cb_t *) item;
        if (NULL != slot->release_cb) {
            slot->release_cb(slot->user_data);
        }
        OBJ_RELEASE(slot);
    }
    while (NULL != (item = opal_list_remove_first(&tf->regs))) {
        mca_base_event_registration_t *reg = (mca_base_event_registration_t *) item;
        if (NULL != reg->free_cb) {
            reg->free_cb(reg, MCA_BASE_EVENT_CB_REQUIRE_NONE, reg->free_user_data);
        }
        if (NULL != reg->info) {
            OBJ_RELEASE(reg->info);
        }
        OBJ_RELEASE(reg);
    }
    OBJ_DESTRUCT(&tf->cb_slots);
    OBJ_DESTRUCT(&tf->dropped_slots);
    OBJ_DESTRUCT(&tf->regs);
}

/* ======================================================================== */
/* Deferred-release fold (sec. 7.1)                                          */
/* ======================================================================== */

/* Drain a deferred-release source's accumulated {count, bytes} and deliver them
   as ONE aggregated, data-bearing event (sec. 7.1).  The underlying releases
   occurred inside free()/munmap()/brk() where delivery is unsafe; this runs at
   safe framework operations -- and before any change to the attached-
   registration set, so a newly-attached registration is never credited with
   pre-attach releases.  Runs with NO framework lock held: mca_base_event_raise()
   takes the lock itself.  A no-op for ordinary sources. */
static void fold_event(mca_base_event_t *event)
{
    mca_base_event_source_t *source;
    int64_t count = 0, bytes = 0;
    struct {
        uint64_t count;
        uint64_t bytes;
    } payload;

    if (NULL == event || NULL == (source = event->source) || NULL == source->fold_consume_fn) {
        return;
    }

    /* Consume-and-clear the producer's pending releases (the sink is a
       never-freed, lock-free atomic pair). */
    source->fold_consume_fn(source->ctx, &count, &bytes);
    if (0 == count) {
        return;
    }

    /* Deliver the batch as one event.  raise() snapshots the registrations and
       delivers under its own lock; a registration whose callback cannot run in
       this (NORMAL) context -- or that has only a dropped handler, no callback --
       instead observes a single drop.  Saturate the aggregates into the
       payload's unsigned elements. */
    payload.count = (uint64_t) mca_base_event_saturate_to(count, OPAL_COUNT_MAX);
    payload.bytes = (uint64_t) mca_base_event_saturate_to(bytes, OPAL_COUNT_MAX);
    mca_base_event_raise(event, source, &payload);
}

/* ======================================================================== */
/* Handle alloc / free / info / validation                                   */
/* ======================================================================== */

/* v1 stores no handle-level hints, so info is ignored. */
int mca_base_event_handle_alloc(int event_index, void *obj_handle,
                                opal_info_t *info __opal_attribute_unused__,
                                mca_base_event_registration_t **reg_out)
{
    mca_base_event_t *event;
    mca_base_event_registration_t *reg;
    int rc;

    rc = mca_base_event_get_by_index(event_index, &event);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }

    /* Resolve the bound-object identity (sec. 6.1).  For a NO_OBJECT event the
       standard requires obj_handle to be IGNORED, so a conformant tool may pass
       any value and must not be rejected (MPI-5.0 sec. 15.3.8); store NULL so a
       NO_OBJECT raise (obj_handle == NULL) matches every registration.  For an
       object-bound event obj_handle references the tool's MPI handle -- the
       "address to a local variable that stores the object's handle" (MPI-5.0
       p.751) -- so dereference it to the identity a producer raises with, and
       reject a missing object.
       XXX ABI (#13280): under the Open MPI ABI the MPI handle IS the internal
       object pointer the producer raises with, so a single deref suffices; the
       MPI Standard ABI path must convert its integer handle to that internal
       pointer here. */
    void *bound_obj = NULL;
    if (MCA_BASE_VAR_BIND_NO_OBJECT != event->bind) {
        if (NULL == obj_handle) {
            return OPAL_ERR_BAD_PARAM;
        }
        bound_obj = *(void **) obj_handle;
    }

    /* Drain a deferred-release source's pending releases BEFORE this new
       registration joins event->registrations, so it never receives an
       aggregated event covering releases that predate it (sec. 7.1
       fold-before-membership-change); a no-op for ordinary sources. */
    fold_event(event);

    opal_mutex_lock(&mca_base_event_lock);

    rc = ensure_dispatch_state();
    if (OPAL_SUCCESS != rc) {
        opal_mutex_unlock(&mca_base_event_lock);
        return rc;
    }

    {
        /* DoS guard (sec. 9.1).  Clamp the (user-settable) cap to a sane range:
           it bounds the dispatch snapshot, so a pathological value must not let
           the snapshot capacity arithmetic overflow. */
        int max_regs = mca_base_event_max_registrations;
        if (max_regs < 1) {
            max_regs = 1;
        } else if (max_regs > (1 << 20)) {
            max_regs = (1 << 20);
        }
        if (live_registration_count >= max_regs) {
            opal_mutex_unlock(&mca_base_event_lock);
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
    }

    reg = OBJ_NEW(mca_base_event_registration_t);
    if (NULL == reg) {
        opal_mutex_unlock(&mca_base_event_lock);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    reg->event = event;
    reg->obj_handle = bound_obj;
    reg->refcount = 1;
    reg->dead = false;

    opal_list_append(&event->registrations, &reg->super);
    rc = opal_hash_table_set_value_uint64(&live_registrations, (uint64_t) (uintptr_t) reg, reg);
    if (OPAL_SUCCESS != rc) {
        /* The live table is what handle validity keys off (handle_is_valid_locked
           looks the registration up there); if the insert failed (e.g. OOM while
           growing the table) unwind instead of returning a handle every later
           operation would reject as MPI_T_ERR_INVALID_HANDLE. */
        opal_list_remove_item(&event->registrations, &reg->super);
        opal_mutex_unlock(&mca_base_event_lock);
        OBJ_RELEASE(reg);
        return rc;
    }
    live_registration_count++;

    opal_mutex_unlock(&mca_base_event_lock);

    *reg_out = reg;
    return OPAL_SUCCESS;
}

/* Validate a registration handle WITHOUT dereferencing it -- the live table is
   keyed by the pointer VALUE, so a lookup on a stale pointer to freed memory is
   safe (sec. 5.10).  MUST be called with mca_base_event_lock held.

   Guarantee: this prevents use-after-free (a freed handle is removed from the
   table at handle_free, so it validates as invalid and is never dereferenced).
   It does NOT distinguish a handle whose address was reused by a *new*
   registration after the original was fully freed -- but reuse can only happen
   once no reference remains (deferred free keeps the memory alive while any
   snapshot/handle references it), and using a freed handle is erroneous tool
   behaviour per the standard, so validating-as-the-new-handle (no crash) is
   conformant. */
static bool handle_is_valid_locked(mca_base_event_registration_t *reg)
{
    void *tmp;

    if (NULL == reg) {
        return false;
    }
    return live_table_created
           && OPAL_SUCCESS
                  == opal_hash_table_get_value_uint64(&live_registrations,
                                                      (uint64_t) (uintptr_t) reg, &tmp)
           && tmp == (void *) reg;
}

bool mca_base_event_handle_is_valid(mca_base_event_registration_t *reg)
{
    bool valid;

    opal_mutex_lock(&mca_base_event_lock);
    valid = handle_is_valid_locked(reg);
    opal_mutex_unlock(&mca_base_event_lock);

    return valid;
}

int mca_base_event_handle_free(mca_base_event_registration_t *reg, void *user_data,
                               mca_base_event_free_cb_fn_t free_cb)
{
    to_free_t tf;
    mca_base_event_dropped_cb_t *flush_slot = NULL;
    mca_base_event_dropped_cb_fn_t flush_cb = NULL;
    void *flush_user_data = NULL;
    int flush_source_index = -1;
    int64_t flush_n = 0;
    bool was_active;
    int l;

    /* Drain a deferred-release source's pending releases -- delivering the final
       aggregated event to this still-live registration -- BEFORE it detaches
       (sec. 7.1), so a tool that frees the handle without first polling
       source_get_timestamp still observes the last batch.  Validate first to
       safely read reg->event; fold_event is a no-op for ordinary sources.  (A
       concurrent erroneous double-free only makes the drain best-effort; the
       validate-and-mutate section below still rejects the stale free.) */
    {
        mca_base_event_t *event = NULL;
        opal_mutex_lock(&mca_base_event_lock);
        if (handle_is_valid_locked(reg)) {
            event = reg->event;
        }
        opal_mutex_unlock(&mca_base_event_lock);
        if (NULL != event) {
            fold_event(event);
        }
    }

    /* Validate AND mutate under one lock acquisition (no TOCTOU): a dead handle
       is absent from the live table, so this rejects a stale/double free. */
    opal_mutex_lock(&mca_base_event_lock);

    if (!handle_is_valid_locked(reg)) {
        opal_mutex_unlock(&mca_base_event_lock);
        return OPAL_ERR_BAD_PARAM; /* stale / double free -> INVALID_HANDLE */
    }

    to_free_init(&tf);

    was_active = reg_is_active(reg);

    /* Final delivery-independent flush for a registration with no selectable
       callback (sec. 5.8): pinned ordering -- retain, swap, THEN mark dead. */
    if (NULL != reg->dropped_slot) {
        flush_n = opal_atomic_swap_64(&reg->dropped, 0);
        if (flush_n != 0) { /* != 0 also catches an overflow-wrapped count */
            (void) opal_atomic_add_fetch_32(&reg->dropped_slot->refcount, 1);
            flush_slot = reg->dropped_slot;
            flush_cb = reg->dropped_slot->dropped_cb;
            flush_user_data = reg->dropped_slot->user_data;
            flush_source_index = (NULL != reg->event->source)
                                     ? reg->event->source->source_index
                                     : -1;
        }
    }

    /* Logical death: remove from the live table at once (a second free then
       fails immediately) but keep the object alive for in-flight dispatch. */
    reg->dead = true;
    opal_hash_table_remove_value_uint64(&live_registrations, (uint64_t) (uintptr_t) reg);
    if (live_registration_count > 0) {
        live_registration_count--;
    }
    opal_list_remove_item(&reg->event->registrations, &reg->super);

    /* Mark all slots dead so later dispatch skips them. */
    for (l = 0; l < MCA_BASE_EVENT_CB_REQUIRE_MAX; ++l) {
        if (NULL != reg->callbacks[l]) {
            reg->callbacks[l]->dead = true;
        }
    }
    if (NULL != reg->dropped_slot) {
        reg->dropped_slot->dead = true;
    }

    /* The reg is leaving; remove its contribution to the gate (the slots are
       marked dead but not NULLed, so reg_is_active can't detect the change).
       On the last-listener (1->0) transition, fire the source deactivate hook
       too -- this path bypasses update_num_active, so a deferred-release
       source (e.g. ompi.unordered) would otherwise never remove its OS hook. */
    if (was_active) {
        int32_t n = opal_atomic_sub_fetch_32(&reg->event->num_active, 1);
        mca_base_event_source_t *source = reg->event->source;
        if (0 == n && NULL != source && NULL != source->activate_fn) {
            source->activate_fn(source->ctx, false);
        }
    }

    reg->free_cb = free_cb;
    reg->free_user_data = user_data;

    opal_mutex_unlock(&mca_base_event_lock);

    /* Invoke the final dropped-handler flush OUTSIDE the lock. */
    if (NULL != flush_cb) {
        flush_cb(mca_base_event_saturate_to(flush_n, OPAL_COUNT_MAX), reg, flush_source_index,
                 MCA_BASE_EVENT_CB_REQUIRE_NONE, flush_user_data);
    }

    /* Release the owning reference (and the flush retain).  When the refcount
       reaches 0, the reg/slots are collected and freed outside the lock. */
    opal_mutex_lock(&mca_base_event_lock);
    if (NULL != flush_slot) {
        release_dropped_slot(flush_slot, &tf);
    }
    for (l = 0; l < MCA_BASE_EVENT_CB_REQUIRE_MAX; ++l) {
        release_cb_slot(reg->callbacks[l], &tf);
    }
    if (NULL != reg->dropped_slot) {
        release_dropped_slot(reg->dropped_slot, &tf);
    }
    release_reg(reg, &tf);
    opal_mutex_unlock(&mca_base_event_lock);

    process_to_free(&tf);
    return OPAL_SUCCESS;
}

/* v1 stores no handle-level hints, so info is ignored. */
int mca_base_event_handle_set_info(mca_base_event_registration_t *reg,
                                   opal_info_t *info __opal_attribute_unused__)
{
    if (!mca_base_event_handle_is_valid(reg)) {
        return OPAL_ERR_BAD_PARAM; /* stale / invalid handle */
    }
    return OPAL_SUCCESS; /* v1 acts on no handle-level hints */
}

int mca_base_event_handle_get_info(mca_base_event_registration_t *reg, opal_info_t **info_used)
{
    if (!mca_base_event_handle_is_valid(reg)) {
        return OPAL_ERR_BAD_PARAM; /* stale / invalid handle */
    }
    *info_used = OBJ_NEW(opal_info_t); /* fresh, user-owned, possibly empty */
    return (NULL != *info_used) ? OPAL_SUCCESS : OPAL_ERR_OUT_OF_RESOURCE;
}

/* ======================================================================== */
/* Callback / dropped-handler management                                     */
/* ======================================================================== */

/* v1 stores no per-callback hints, so info is ignored. */
int mca_base_event_register_callback(mca_base_event_registration_t *reg,
                                     mca_base_event_cb_safety_t cb_safety,
                                     opal_info_t *info __opal_attribute_unused__,
                                     void *user_data, mca_base_event_ctx_release_fn_t release_cb,
                                     mca_base_event_cb_fn_t cb)
{
    to_free_t tf;
    mca_base_event_cb_t *old_slot, *new_slot = NULL;
    bool was_active;

    if (cb_safety < 0 || cb_safety >= MCA_BASE_EVENT_CB_REQUIRE_MAX) {
        return OPAL_ERR_BAD_PARAM;
    }

    /* Allocate the new slot before taking the lock. */
    if (NULL != cb) {
        new_slot = OBJ_NEW(mca_base_event_cb_t);
        if (NULL == new_slot) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
        new_slot->cb_safety = cb_safety;
        new_slot->cb_function = cb;
        new_slot->user_data = user_data;
        new_slot->release_cb = release_cb;
        new_slot->refcount = 1;
        new_slot->dead = false;
    }

    /* Validate AND mutate under one lock acquisition (no TOCTOU). */
    opal_mutex_lock(&mca_base_event_lock);
    if (!handle_is_valid_locked(reg)) {
        opal_mutex_unlock(&mca_base_event_lock);
        if (NULL != new_slot) {
            OBJ_RELEASE(new_slot);
        }
        return OPAL_ERR_BAD_PARAM;
    }

    to_free_init(&tf);
    was_active = reg_is_active(reg);

    old_slot = reg->callbacks[cb_safety];
    if (NULL != old_slot) {
        old_slot->dead = true; /* detach; freed when in-flight snapshots release it */
    }
    reg->callbacks[cb_safety] = new_slot; /* NULL on removal */

    update_num_active(reg, was_active);

    release_cb_slot(old_slot, &tf);
    opal_mutex_unlock(&mca_base_event_lock);

    process_to_free(&tf);
    return OPAL_SUCCESS;
}

/* v1 stores no per-callback hints, so info is ignored. */
int mca_base_event_callback_set_info(mca_base_event_registration_t *reg,
                                     mca_base_event_cb_safety_t cb_safety,
                                     opal_info_t *info __opal_attribute_unused__)
{
    if ((int) cb_safety < 0 || (int) cb_safety >= MCA_BASE_EVENT_CB_REQUIRE_MAX) {
        return OPAL_ERR_BAD_PARAM; /* out-of-range safety level */
    }
    /* The hints apply to the callback registered at cb_safety; reject a level
       with no registered callback (and a stale handle).  Read the slot under the
       lock. */
    opal_mutex_lock(&mca_base_event_lock);
    if (!handle_is_valid_locked(reg) || NULL == reg->callbacks[cb_safety]) {
        opal_mutex_unlock(&mca_base_event_lock);
        return OPAL_ERR_BAD_PARAM;
    }
    opal_mutex_unlock(&mca_base_event_lock);
    return OPAL_SUCCESS;
}

int mca_base_event_callback_get_info(mca_base_event_registration_t *reg,
                                     mca_base_event_cb_safety_t cb_safety, opal_info_t **info_used)
{
    if ((int) cb_safety < 0 || (int) cb_safety >= MCA_BASE_EVENT_CB_REQUIRE_MAX) {
        return OPAL_ERR_BAD_PARAM; /* out-of-range safety level */
    }
    /* Return the hints of the callback registered at cb_safety; reject a level
       with no registered callback (and a stale handle). */
    opal_mutex_lock(&mca_base_event_lock);
    if (!handle_is_valid_locked(reg) || NULL == reg->callbacks[cb_safety]) {
        opal_mutex_unlock(&mca_base_event_lock);
        return OPAL_ERR_BAD_PARAM;
    }
    opal_mutex_unlock(&mca_base_event_lock);
    *info_used = OBJ_NEW(opal_info_t);
    return (NULL != *info_used) ? OPAL_SUCCESS : OPAL_ERR_OUT_OF_RESOURCE;
}

int mca_base_event_set_dropped_handler(mca_base_event_registration_t *reg,
                                       mca_base_event_dropped_cb_fn_t dropped_cb, void *user_data,
                                       mca_base_event_ctx_release_fn_t release_cb)
{
    to_free_t tf;
    mca_base_event_dropped_cb_t *old_slot, *new_slot = NULL;
    bool was_active;

    /* Allocate the new slot before taking the lock. */
    if (NULL != dropped_cb) {
        new_slot = OBJ_NEW(mca_base_event_dropped_cb_t);
        if (NULL == new_slot) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
        new_slot->dropped_cb = dropped_cb;
        new_slot->user_data = user_data;
        new_slot->release_cb = release_cb;
        new_slot->refcount = 1;
        new_slot->dead = false;
    }

    /* Drain a deferred-release source's pending releases -- delivering the
       aggregated event under this registration's CURRENT (old) dropped handler --
       BEFORE swapping or clearing it, so releases that accumulated under the old
       handler are reported to it rather than mis-attributed to the replacement.
       Validate first to safely read reg->event; fold_event is a no-op for
       ordinary sources.  (Mirrors the drain prologue in handle_free; the
       validate-and-mutate section below still rejects a stale handle.) */
    {
        mca_base_event_t *event = NULL;
        opal_mutex_lock(&mca_base_event_lock);
        if (handle_is_valid_locked(reg)) {
            event = reg->event;
        }
        opal_mutex_unlock(&mca_base_event_lock);
        if (NULL != event) {
            fold_event(event);
        }
    }

    /* Validate AND mutate under one lock acquisition (no TOCTOU). */
    opal_mutex_lock(&mca_base_event_lock);
    if (!handle_is_valid_locked(reg)) {
        opal_mutex_unlock(&mca_base_event_lock);
        if (NULL != new_slot) {
            OBJ_RELEASE(new_slot);
        }
        return OPAL_ERR_BAD_PARAM;
    }

    to_free_init(&tf);
    was_active = reg_is_active(reg);

    old_slot = reg->dropped_slot;
    if (NULL != old_slot) {
        old_slot->dead = true;
    }
    reg->dropped_slot = new_slot;

    /* (Re)installing or clearing the handler resets pending drops: MPI-5.0
       sec. 15.3.8 (p.755) scopes the dropped count to "since the registration of
       the dropped-callback handler", so a newly installed handler must not
       observe drops that predate it (and clearing discards them).  A
       deferred-release source already drained its pending releases to the OLD
       handler in the fold prologue above, so nothing owed to it is lost here. */
    (void) opal_atomic_swap_64(&reg->dropped, 0);

    update_num_active(reg, was_active);

    release_dropped_slot(old_slot, &tf);
    opal_mutex_unlock(&mca_base_event_lock);

    process_to_free(&tf);
    return OPAL_SUCCESS;
}

/* ======================================================================== */
/* In-callback accessors (lock-free; instance-owned data)                    */
/* ======================================================================== */

int mca_base_event_read(mca_base_event_instance_t *inst, int element_index, void *buf)
{
    mca_base_event_t *event;
    size_t tsize, offset;

    if (NULL == inst || NULL == buf) {
        return OPAL_ERR_BAD_PARAM;
    }
    event = inst->event;
    if (NULL == event || element_index < 0 || element_index >= event->num_elements) {
        return OPAL_ERR_VALUE_OUT_OF_BOUNDS;
    }

    /* Write exactly the validated element's width from instance-owned memory;
       no tool-supplied length participates (sec. 6). */
    tsize = ompi_var_type_sizes[event->element_types[element_index]];
    offset = (size_t) event->element_offsets[element_index];
    memcpy(buf, inst->data + offset, tsize);

    return OPAL_SUCCESS;
}

int mca_base_event_copy(mca_base_event_instance_t *inst, void *buf)
{
    mca_base_event_t *event;
    int i;

    if (NULL == inst || NULL == buf) {
        return OPAL_ERR_BAD_PARAM;
    }
    event = inst->event;
    if (NULL == event) {
        return OPAL_ERR_BAD_PARAM;
    }

    /* Zero then per-element copy from the validated layout (not a raw memcpy of
       padding, sec. 6).  Bound the zero-fill to the un-rounded element extent --
       what the caller sized buf from via MPI_T_event_get_info -- NOT the internal
       8-byte-rounded buffer_size, or the trailing rounding pad would overflow the
       caller's buffer. */
    memset(buf, 0, event->data_extent);
    for (i = 0; i < event->num_elements; ++i) {
        size_t tsize = ompi_var_type_sizes[event->element_types[i]];
        size_t offset = (size_t) event->element_offsets[i];
        memcpy((unsigned char *) buf + offset, inst->data + offset, tsize);
    }

    return OPAL_SUCCESS;
}

int mca_base_event_instance_get_timestamp(mca_base_event_instance_t *inst, opal_count_t *timestamp)
{
    if (NULL == inst || NULL == timestamp) {
        return OPAL_ERR_BAD_PARAM;
    }
    *timestamp = inst->timestamp;
    return OPAL_SUCCESS;
}

int mca_base_event_instance_get_source(mca_base_event_instance_t *inst, int *source_index)
{
    if (NULL == inst || NULL == source_index) {
        return OPAL_ERR_BAD_PARAM;
    }
    *source_index = (NULL != inst->source) ? inst->source->source_index : -1;
    return OPAL_SUCCESS;
}

/* ======================================================================== */
/* The raise / dispatch path (sec. 5.9)                                      */
/* ======================================================================== */

typedef struct {
    mca_base_event_registration_t  *reg;
    mca_base_event_cb_t            *slot;
    mca_base_event_cb_fn_t          cb_function;
    void                           *cb_user_data;
    mca_base_event_dropped_cb_t    *dropped_slot;
    mca_base_event_dropped_cb_fn_t  dropped_cb;
    void                           *dropped_user_data;
    int64_t                         n;
} dispatch_entry_t;

#define MCA_BASE_EVENT_SNAP_INLINE 32

/* Select the callback for context C: smallest L in [C, THREAD_SAFE] with a
   slot.  ASYNC_SIGNAL_SAFE is registrable but never selected in v1. */
static mca_base_event_cb_t *select_callback(mca_base_event_registration_t *reg,
                                            mca_base_event_cb_safety_t context)
{
    int l;

    for (l = context; l <= MCA_BASE_EVENT_CB_REQUIRE_V1_MAX; ++l) {
        if (NULL != reg->callbacks[l]) {
            return reg->callbacks[l];
        }
    }
    return NULL;
}

void mca_base_event_raise_internal(mca_base_event_t *event, mca_base_event_source_t *source,
                                   void *obj_handle, const void *data,
                                   mca_base_event_cb_safety_t context)
{
    dispatch_entry_t inline_snap[MCA_BASE_EVENT_SNAP_INLINE];
    dispatch_entry_t *snap = inline_snap;
    int snap_cap = MCA_BASE_EVENT_SNAP_INLINE;
    int num_deliveries = 0;
    mca_base_event_instance_t *inst = NULL;
    to_free_t tf;
    opal_list_item_t *item;
    int i;

    /* Step 0: recursion-depth gate at ENTRY (sec. 5.9). */
    if (++dispatch_depth > MCA_BASE_EVENT_MAX_DISPATCH_DEPTH) {
        --dispatch_depth;
        return;
    }

#if OPAL_ENABLE_DEBUG
    if (NULL != mca_base_event_debug_raise_check_fn) {
        mca_base_event_debug_raise_check_fn();
    }
    assert(context != MCA_BASE_EVENT_CB_REQUIRE_ASYNC_SIGNAL_SAFE);
#endif

    /* Step 1: single-source check. */
    if (NULL == source) {
        source = event->source;
    } else if (source != event->source) {
        --dispatch_depth;
        return;
    }

    to_free_init(&tf);

    /* Step 2: snapshot the live registration set ONCE under the lock. */
    opal_mutex_lock(&mca_base_event_lock);
    for (item = opal_list_get_first(&event->registrations);
         item != opal_list_get_end(&event->registrations); item = opal_list_get_next(item)) {
        mca_base_event_registration_t *reg = (mca_base_event_registration_t *) item;
        mca_base_event_cb_t *slot;

        if (reg->dead) {
            continue;
        }
        /* Object-binding filter (sec. 6.1): a bound event reaches only
           registrations bound to the raised object.  For a NO_OBJECT event both
           sides are NULL, so every registration matches.  A non-matching bound
           registration is simply not a recipient -- it is skipped, NOT counted
           as a dropped event. */
        if (reg->obj_handle != obj_handle) {
            continue;
        }
        slot = select_callback(reg, context);

        if (NULL != slot) {
            /* Delivery: capture-and-clear pending drops, retain everything. */
            if (num_deliveries >= snap_cap) {
                int new_cap = snap_cap * 2;
                dispatch_entry_t *grown;
                grown = (snap == inline_snap) ? malloc(new_cap * sizeof(*grown))
                                              : realloc(snap, new_cap * sizeof(*grown));
                if (NULL == grown) {
                    /* Heap-fallback OOM: count this raise as a drop for the
                       remaining (un-snapshotted) registrations, within the same
                       lock hold, and stop snapshotting (sec. 5.9). */
                    (void) opal_atomic_add_fetch_64(&reg->dropped, 1);
                    for (item = opal_list_get_next(item);
                         item != opal_list_get_end(&event->registrations);
                         item = opal_list_get_next(item)) {
                        mca_base_event_registration_t *tail
                            = (mca_base_event_registration_t *) item;
                        if (!tail->dead && tail->obj_handle == obj_handle) {
                            (void) opal_atomic_add_fetch_64(&tail->dropped, 1);
                        }
                    }
                    break;
                }
                if (snap == inline_snap) {
                    memcpy(grown, inline_snap, snap_cap * sizeof(*grown));
                }
                snap = grown;
                snap_cap = new_cap;
            }
            snap[num_deliveries].reg = reg;
            snap[num_deliveries].slot = slot;
            snap[num_deliveries].cb_function = slot->cb_function;
            snap[num_deliveries].cb_user_data = slot->user_data;
            snap[num_deliveries].dropped_slot = reg->dropped_slot;
            snap[num_deliveries].dropped_cb = (NULL != reg->dropped_slot)
                                                  ? reg->dropped_slot->dropped_cb
                                                  : NULL;
            snap[num_deliveries].dropped_user_data = (NULL != reg->dropped_slot)
                                                         ? reg->dropped_slot->user_data
                                                         : NULL;
            snap[num_deliveries].n = opal_atomic_swap_64(&reg->dropped, 0);

            (void) opal_atomic_add_fetch_32(&reg->refcount, 1);
            (void) opal_atomic_add_fetch_32(&slot->refcount, 1);
            if (NULL != reg->dropped_slot) {
                (void) opal_atomic_add_fetch_32(&reg->dropped_slot->refcount, 1);
            }
            num_deliveries++;
        } else {
            /* Drop: accumulate, record nothing (flushed before next delivery). */
            (void) opal_atomic_add_fetch_64(&reg->dropped, 1);
        }
    }
    opal_mutex_unlock(&mca_base_event_lock);

    /* Step 3: acquire one instance (lazy), copy payload, invoke -- NO lock. */
    if (num_deliveries > 0) {
        opal_free_list_item_t *fli = force_pool_exhaustion ? NULL
                                                           : opal_free_list_get(&instance_pool);
        if (NULL == fli) {
            /* Pool exhaustion: restore the captured counts (sec. 5.9 step 3),
               saturating -- a near-max or already-wrapped captured count must
               not overflow the (non-atomic) n+1 computation. */
            for (i = 0; i < num_deliveries; ++i) {
                int64_t addend;
                if (snap[i].n < 0 || snap[i].n >= INT64_MAX) {
                    addend = OPAL_COUNT_MAX;
                } else {
                    addend = snap[i].n + 1;
                }
                (void) opal_atomic_add_fetch_64(&snap[i].reg->dropped, addend);
            }
        } else {
            inst = (mca_base_event_instance_t *) fli;
            (void) opal_atomic_add_fetch_64(&pool_acquire_count, 1);
            inst->event = event;
            inst->source = source;
            inst->timestamp = source_timestamp(source);
            /* Copy only the un-rounded element extent the producer supplied, not
               the 8-byte-rounded buffer_size -- otherwise this would over-read
               past a producer payload sized to the element layout, e.g. a bare
               int32 (sec. 6).  inst->data is buffer_size bytes; the trailing pad
               stays uninitialized and is never read (event_read/copy are bounded
               by per-element offsets / data_extent). */
            inst->data_len = event->data_extent;
            if (event->data_extent > 0 && NULL != data) {
                memcpy(inst->data, data, event->data_extent);
            }

            for (i = 0; i < num_deliveries; ++i) {
                if (snap[i].n != 0 && NULL != snap[i].dropped_cb) {
                    snap[i].dropped_cb(mca_base_event_saturate_to(snap[i].n, OPAL_COUNT_MAX),
                                       snap[i].reg, source->source_index, context,
                                       snap[i].dropped_user_data);
                }
                snap[i].cb_function(inst, snap[i].reg, context, snap[i].cb_user_data);
            }
        }
    }

    /* Step 4: reconcile -- release retained refs, collect dead objects. */
    opal_mutex_lock(&mca_base_event_lock);
    for (i = 0; i < num_deliveries; ++i) {
        release_dropped_slot(snap[i].dropped_slot, &tf);
        release_cb_slot(snap[i].slot, &tf);
        release_reg(snap[i].reg, &tf);
    }
    opal_mutex_unlock(&mca_base_event_lock);
    process_to_free(&tf);

    /* Step 5: return the instance; decrement the depth counter. */
    if (NULL != inst) {
        opal_free_list_return(&instance_pool, &inst->super);
    }
    if (snap != inline_snap) {
        free(snap);
    }
    --dispatch_depth;
}

/* ======================================================================== */
/* Instance OBJ class (kept here so opal_free_list stays out of core)        */
/* ======================================================================== */

static void event_instance_constructor(mca_base_event_instance_t *inst)
{
    inst->event = NULL;
    inst->source = NULL;
    inst->timestamp = 0;
    inst->data_len = 0;
}

OBJ_CLASS_INSTANCE(mca_base_event_instance_t, opal_free_list_item_t, event_instance_constructor,
                   NULL);
