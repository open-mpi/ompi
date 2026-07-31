/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * The OS memory-release producer behind ompi.mca.memory.patcher.released
 * (spec sec. 7.1).
 *
 * The OPAL memory-release hook runs *inside* intercepted free()/munmap()/brk()
 * (etc.): it is re-entrant and not thread-safe, and may not take a lock,
 * allocate, or walk the registration list.  So the hook does the minimum -- if
 * a listener is attached, two atomic adds into a single never-freed sink: one
 * bumping the release COUNT and one adding the release length to a cumulative
 * BYTE total.  The framework later *folds* that {count, bytes} pair (at safe
 * framework operations touching the event, never in the hook) and delivers it
 * as ONE aggregated, data-bearing event whose payload carries the count and
 * byte total.  A registration that has only a dropped handler (no callback)
 * instead observes the aggregated event as a single drop.
 *
 * The OPAL memory hooks are chunk-level (they fire only when memory is actually
 * returned to the OS -- munmap, brk shrink, etc. -- not on every free()), and
 * the hook callback cannot distinguish which API caused the release, so the
 * payload is a coarse aggregate, not a per-API breakdown.
 *
 * Hard invariant: the sink is allocated once (static) and NEVER freed for the
 * process lifetime, and the hook body touches only the sink, so an in-flight
 * hook racing unregister_release is safe by construction.
 */

#include "opal_config.h"

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include "opal/mca/base/mca_base_event.h"
#include "opal/mca/base/mca_base_var.h"
#include "opal/mca/base/mca_base_pvar.h"
#include "opal/sys/atomic.h"
#include "opal/memoryhooks/memory.h"
#include "opal/constants.h"

/* The single, never-freed sink (spec sec. 7.1): the number of releases seen
   and their cumulative byte total since the last fold. */
typedef struct {
    opal_atomic_int32_t active;
    opal_atomic_int64_t count;
    opal_atomic_int64_t bytes;
} memory_sink_t;

static memory_sink_t memory_sink = {.active = 0, .count = 0, .bytes = 0};
static bool memory_registered = false;

/* Test seam: suppress installing the real OS hook so a test's sink bumps are
   the only source of releases (deterministic fold count). */
static bool memory_suppress_hook = false;

/* Runs inside free()/munmap()/brk() (etc.): no lock, no alloc, no list walk.
   The body is an atomic load plus two inline atomic adds, so it cannot reach
   free()/munmap()/brk() and cannot recurse; concurrent hooks on other threads
   are handled by the atomics.

   `active` is a plain load of an atomic (opal_atomic_int32_t is _Atomic int32_t
   under C11 atomics, volatile int32_t otherwise -- never a torn read), paired
   with the full-barrier swap at detach.  A hook already past this load when
   detach stores false will still bump the sink; that is deliberate best-effort.
   Such residue is never mis-attributed: mca_base_event_handle_alloc() folds the
   event before a new registration joins it (fold-before-membership-change), so
   the stale count is consumed and raised to the then-current -- empty --
   registration set rather than credited to the next listener.

   Only ever touches the never-freed sink.  `length` is the number of bytes the
   release returned to the OS; `from_alloc` is not recorded -- it cannot
   distinguish the releasing API (munmap/brk/etc. all report identically), only
   SysV shm vs the heap/anon family, which is not a useful split here. */
static void memory_release_hook(void *buf __opal_attribute_unused__, size_t length,
                                void *cbdata __opal_attribute_unused__,
                                bool from_alloc __opal_attribute_unused__)
{
    if (memory_sink.active) {
        (void) opal_atomic_add_fetch_64(&memory_sink.count, 1);
        (void) opal_atomic_add_fetch_64(&memory_sink.bytes, (int64_t) length);
    }
}

/* Consume-and-clear the pending release count and byte total; called by the
   fold (spec sec. 7.1).  The sink is lock-free, so no framework lock is held. */
static void memory_fold_consume(void *ctx __opal_attribute_unused__, int64_t *count,
                                int64_t *bytes)
{
    *count = opal_atomic_swap_64(&memory_sink.count, 0);
    *bytes = opal_atomic_swap_64(&memory_sink.bytes, 0);
}

/* Lazily install the OS hook on the first listener-attach and remove it on the
   last detach; called under the framework lock (spec sec. 7.1). */
static void memory_activate(void *ctx __opal_attribute_unused__, bool active)
{
    if (active) {
        /* Publish active=true (full-barrier swap), then install the hook. */
        (void) opal_atomic_swap_32(&memory_sink.active, 1);
        if (!memory_suppress_hook) {
            (void) opal_mem_hooks_register_release(memory_release_hook, NULL);
        }
    } else {
        /* Release store active=false (paired with the hook's read) BEFORE
           unregister, so an in-flight hook sees inactive and only touches the
           never-freed sink even if it races the unregister. */
        (void) opal_atomic_swap_32(&memory_sink.active, 0);
        if (!memory_suppress_hook) {
            (void) opal_mem_hooks_unregister_release(memory_release_hook);
        }
    }
}

bool mca_base_event_memory_supported(void)
{
    /* This producer can only be offered where the OPAL memory hooks can report
       releases; without them the producer is simply absent (a smaller,
       still-conformant event set; spec sec. 7.1). */
    return 0 != (opal_mem_hooks_support_level() & OPAL_MEMORY_FREE_SUPPORT);
}

void mca_base_event_memory_attach(mca_base_event_source_t *source, mca_base_event_t *event)
{
    /* Wire the deferred-release machinery (the framework drives it) into a
       source/event registered by the caller (the OMPI producer layer chooses
       the user-facing names).  Idempotent across MPI_T_init/finalize cycles. */
    source->fold_consume_fn = memory_fold_consume;
    source->activate_fn = memory_activate;
    source->fold_event = event;
    source->ctx = NULL;

    memory_registered = true;
}

bool mca_base_event_memory_test_bump(int64_t count, int64_t bytes)
{
    if (!memory_registered) {
        return false;
    }
    (void) opal_atomic_add_fetch_64(&memory_sink.count, count);
    (void) opal_atomic_add_fetch_64(&memory_sink.bytes, bytes);
    return true;
}

void mca_base_event_memory_test_suppress_hook(bool suppress)
{
    memory_suppress_hook = suppress;
}
