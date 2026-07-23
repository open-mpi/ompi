/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * White-box unit test for the OPAL mca_base_event framework (dispatch
 * core).  Links libopen-pal directly and drives mca_base_event_* with no
 * MPI/MPI_T layer.  Runs as a 'make check' program.
 */

#include "opal_config.h"

#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "opal/mca/base/mca_base_event.h"
#include "opal/mca/base/mca_base_pvar.h" /* MCA_BASE_VAR_BIND_NO_OBJECT */
#include "opal/mca/memory/base/base.h"   /* opal_memory_base_framework */
#include "opal/runtime/opal.h"

/* White-box hooks exported by mca_base_event_dispatch.c (not in the public
   header). */
extern int64_t mca_base_event_test_pool_acquires(void);
extern void    mca_base_event_test_set_force_exhaustion(bool v);

static int failures = 0;

static void check(const char *what, int cond)
{
    if (cond) {
        printf("PASS: %s\n", what);
    } else {
        ++failures;
        printf("FAIL: %s\n", what);
    }
}

/* Shared test state reachable from the callbacks. */
static int          cb_fired = 0;
static int32_t      cb_read_a = 0;
static uint64_t     cb_read_b = 0;
static opal_count_t cb_safety_seen = -1;
static opal_count_t cb_timestamp = 0;
static int          cb_source_index = -1;

static opal_count_t dropped_count_seen = 0;
static int          dropped_fired = 0;

static int          free_fired = 0;
static mca_base_event_registration_t *self_free_reg = NULL;

static void event_cb(mca_base_event_instance_t *inst,
                     mca_base_event_registration_t *reg __opal_attribute_unused__,
                     mca_base_event_cb_safety_t cb_safety, void *user_data __opal_attribute_unused__)
{
    int src = -1;
    opal_count_t ts = 0;

    ++cb_fired;
    cb_safety_seen = cb_safety;
    (void) mca_base_event_read(inst, 0, &cb_read_a);
    (void) mca_base_event_read(inst, 1, &cb_read_b);
    (void) mca_base_event_instance_get_timestamp(inst, &ts);
    (void) mca_base_event_instance_get_source(inst, &src);
    cb_timestamp = ts;
    cb_source_index = src;
}

/* --- Object binding (sec. 6.1): record which bound registration was reached
   (its object, passed through as user_data). */
static int   bound_count = 0;
static void *bound_last = NULL;
static void bound_cb(mca_base_event_instance_t *inst __opal_attribute_unused__,
                     mca_base_event_registration_t *reg __opal_attribute_unused__,
                     mca_base_event_cb_safety_t cb_safety __opal_attribute_unused__,
                     void *user_data)
{
    ++bound_count;
    bound_last = user_data;
}

static void dropped_cb(opal_count_t count, mca_base_event_registration_t *reg __opal_attribute_unused__,
                       int source_index __opal_attribute_unused__,
                       mca_base_event_cb_safety_t cb_safety __opal_attribute_unused__,
                       void *user_data __opal_attribute_unused__)
{
    ++dropped_fired;
    dropped_count_seen += count;
}

/* A synthetic deferred-release producer's consume hook, so the fold mechanism
   (sec. 7.1) is exercised on every platform regardless of whether the real
   OS-memory free-hooks are available.  Hands back an accumulated {count, bytes}
   pair the framework delivers as one aggregated, data-bearing event. */
static opal_atomic_int64_t fold_test_counter = 0;
static opal_atomic_int64_t fold_test_bytes = 0;
static void fold_test_consume(void *ctx __opal_attribute_unused__, int64_t *count, int64_t *bytes)
{
    *count = opal_atomic_swap_64(&fold_test_counter, 0);
    *bytes = opal_atomic_swap_64(&fold_test_bytes, 0);
}

/* Records the aggregated payload the fold delivers to a registration's callback. */
static int      fold_cb_fired = 0;
static uint64_t fold_cb_count = 0;
static uint64_t fold_cb_bytes = 0;
static void fold_event_cb(mca_base_event_instance_t *inst,
                          mca_base_event_registration_t *reg __opal_attribute_unused__,
                          mca_base_event_cb_safety_t cb_safety __opal_attribute_unused__,
                          void *user_data __opal_attribute_unused__)
{
    ++fold_cb_fired;
    (void) mca_base_event_read(inst, 0, &fold_cb_count);
    (void) mca_base_event_read(inst, 1, &fold_cb_bytes);
}

static void self_free_cb(mca_base_event_instance_t *inst __opal_attribute_unused__,
                         mca_base_event_registration_t *reg,
                         mca_base_event_cb_safety_t cb_safety __opal_attribute_unused__,
                         void *user_data __opal_attribute_unused__)
{
    /* Free our own handle from inside the callback (sec. 5.10). */
    (void) mca_base_event_handle_free(reg, NULL, NULL);
}

static void free_marker_cb(mca_base_event_registration_t *reg __opal_attribute_unused__,
                           mca_base_event_cb_safety_t cb_safety __opal_attribute_unused__,
                           void *user_data __opal_attribute_unused__)
{
    ++free_fired;
}

static void self_free_event_cb(mca_base_event_instance_t *inst __opal_attribute_unused__,
                               mca_base_event_registration_t *reg,
                               mca_base_event_cb_safety_t cb_safety __opal_attribute_unused__,
                               void *user_data __opal_attribute_unused__)
{
    self_free_reg = reg;
}

/* --- Ordering: record D (dropped handler) and C (event callback) in sequence. */
static char seq[8];
static int  seq_n = 0;

static void seq_event_cb(mca_base_event_instance_t *inst __opal_attribute_unused__,
                         mca_base_event_registration_t *reg __opal_attribute_unused__,
                         mca_base_event_cb_safety_t cb_safety __opal_attribute_unused__,
                         void *user_data __opal_attribute_unused__)
{
    if (seq_n < (int) sizeof(seq)) {
        seq[seq_n++] = 'C';
    }
}

static void seq_dropped_cb(opal_count_t count __opal_attribute_unused__,
                           mca_base_event_registration_t *reg __opal_attribute_unused__,
                           int source_index __opal_attribute_unused__,
                           mca_base_event_cb_safety_t cb_safety __opal_attribute_unused__,
                           void *user_data __opal_attribute_unused__)
{
    if (seq_n < (int) sizeof(seq)) {
        seq[seq_n++] = 'D';
    }
}

/* --- Selection: record which safety level was delivered. */
static mca_base_event_cb_safety_t selected_level = -1;

static void level_cb(mca_base_event_instance_t *inst __opal_attribute_unused__,
                     mca_base_event_registration_t *reg __opal_attribute_unused__,
                     mca_base_event_cb_safety_t cb_safety __opal_attribute_unused__, void *user_data)
{
    /* user_data carries the registered level so we can tell which slot fired. */
    selected_level = (mca_base_event_cb_safety_t) (intptr_t) user_data;
}

/* --- Recursion depth: re-raise unconditionally, track max observed nesting. */
static int depth_now = 0;
static int depth_max = 0;

static void depth_cb(mca_base_event_instance_t *inst __opal_attribute_unused__,
                     mca_base_event_registration_t *reg,
                     mca_base_event_cb_safety_t cb_safety __opal_attribute_unused__,
                     void *user_data __opal_attribute_unused__)
{
    unsigned char b[16] = {0};
    ++depth_now;
    if (depth_now > depth_max) {
        depth_max = depth_now;
    }
    /* Re-raise the same delivered event; the framework's depth cap must stop
       the unbounded recursion. */
    mca_base_event_raise_internal(reg->event, reg->event->source, NULL, b,
                                  MCA_BASE_EVENT_CB_REQUIRE_NONE);
    --depth_now;
}

/* --- Concurrency stress: producer raises while consumer allocs/frees. */
static volatile int stress_stop = 0;
static mca_base_event_t        *stress_event = NULL;
static mca_base_event_source_t *stress_source = NULL;
static int                      stress_ev_index = -1;

static void stress_cb(mca_base_event_instance_t *inst __opal_attribute_unused__,
                      mca_base_event_registration_t *reg __opal_attribute_unused__,
                      mca_base_event_cb_safety_t cb_safety __opal_attribute_unused__,
                      void *user_data __opal_attribute_unused__)
{
}

static void *stress_producer(void *arg __opal_attribute_unused__)
{
    unsigned char b[16] = {0};
    while (!stress_stop) {
        mca_base_event_raise_internal(stress_event, stress_source, NULL, b,
                                      MCA_BASE_EVENT_CB_REQUIRE_NONE);
    }
    return NULL;
}

static void *stress_consumer(void *arg __opal_attribute_unused__)
{
    int i;
    for (i = 0; i < 20000; ++i) {
        mca_base_event_registration_t *r;
        if (OPAL_SUCCESS == mca_base_event_handle_alloc(stress_ev_index, NULL, NULL, &r)) {
            (void) mca_base_event_register_callback(r, MCA_BASE_EVENT_CB_REQUIRE_NONE, NULL, NULL,
                                                    NULL, stress_cb);
            (void) mca_base_event_handle_free(r, NULL, NULL);
        }
    }
    return NULL;
}

int main(int argc, char *argv[])
{
    int rc, src_index, ev_index, i;
    mca_base_event_source_t *source;
    mca_base_event_t *event;
    mca_base_event_registration_t *reg;
    mca_base_var_type_t types[2] = {MCA_BASE_VAR_TYPE_INT32_T, MCA_BASE_VAR_TYPE_UINT64_T};
    ptrdiff_t offsets[2] = {0, 8};
    unsigned char buf[16];

    opal_init(&argc, &argv);

    /* --- saturate_to: clamp at an INJECTED 32-bit ceiling on a 64-bit host. */
    {
        int64_t c32 = 0x7fffffffLL;
        check("saturate below ceiling is identity",
              c32 - 1 == (int64_t) mca_base_event_saturate_to(c32 - 1, c32));
        check("saturate above ceiling clamps (no negative wrap)",
              c32 == (int64_t) mca_base_event_saturate_to(c32 + 1000, c32)
                  && (int64_t) mca_base_event_saturate_to(c32 + 1000, c32) > 0);
    }

    /* --- Register a source + a 2-element event type. */
    src_index = mca_base_event_source_register("test.src", "test source",
                                               MCA_BASE_EVENT_SOURCE_ORDERED, 1000000000,
                                               OPAL_COUNT_MAX, NULL, true, NULL);
    check("source_register succeeds", src_index >= 0);
    rc = mca_base_event_source_get_by_index(src_index, &source);
    check("source_get_by_index", OPAL_SUCCESS == rc);

    ev_index = mca_base_event_register("test", "test", "comp", "event", "a test event",
                                       OPAL_INFO_LVL_4, 2, types, offsets, NULL,
                                       MCA_BASE_VAR_BIND_NO_OBJECT, 0, source, NULL);
    check("event_register succeeds", ev_index >= 0);
    rc = mca_base_event_get_by_index(ev_index, &event);
    check("event_get_by_index", OPAL_SUCCESS == rc);
    check("buffer_size computed (16)", 16 == (int) event->buffer_size);

    /* --- Oversized payload is rejected. */
    {
        mca_base_var_type_t big_types[1] = {MCA_BASE_VAR_TYPE_UINT64_T};
        ptrdiff_t big_offsets[1] = {MCA_BASE_EVENT_MAX_PAYLOAD};
        rc = mca_base_event_register("test", "test", "comp", "toobig", "oversized",
                                     OPAL_INFO_LVL_4, 1, big_types, big_offsets, NULL,
                                     MCA_BASE_VAR_BIND_NO_OBJECT, 0, source, NULL);
        check("oversized payload rejected", rc < 0);
    }

    /* --- Basic raise -> callback delivery, payload read, timestamp, source. */
    rc = mca_base_event_handle_alloc(ev_index, NULL, NULL, &reg);
    check("handle_alloc", OPAL_SUCCESS == rc);
    rc = mca_base_event_register_callback(reg, MCA_BASE_EVENT_CB_REQUIRE_NONE, NULL, NULL, NULL,
                                          event_cb);
    check("register NONE callback", OPAL_SUCCESS == rc);
    check("event is now active", mca_base_event_active(event));

    {
        int32_t v32 = 42;
        uint64_t v64 = 0x1122334455667788ULL;
        /* memcpy, not an aligned store: buf[] (a char array) has no guaranteed
           8-byte alignment, so *(uint64_t *)(buf + 8) would be a misaligned
           access (SIGBUS on strict-alignment targets). */
        memcpy(buf + 0, &v32, sizeof(v32));
        memcpy(buf + 8, &v64, sizeof(v64));
    }
    cb_fired = 0;
    mca_base_event_raise_internal(event, source, NULL, buf, MCA_BASE_EVENT_CB_REQUIRE_NONE);
    check("callback fired once", 1 == cb_fired);
    check("event_read element 0", 42 == cb_read_a);
    check("event_read element 1", 0x1122334455667788ULL == cb_read_b);
    check("cb_safety == raise context (NONE)", MCA_BASE_EVENT_CB_REQUIRE_NONE == cb_safety_seen);
    check("event_get_source", src_index == cb_source_index);
    check("timestamp is non-negative", cb_timestamp >= 0);

    /* --- Drop accounting + dropped-handler flush before next delivery. */
    rc = mca_base_event_set_dropped_handler(reg, dropped_cb, NULL, NULL);
    check("set_dropped_handler", OPAL_SUCCESS == rc);
    dropped_fired = 0;
    dropped_count_seen = 0;
    /* Raise N times at THREAD_SAFE: only a NONE callback exists, so none is
       selected (cap at THREAD_SAFE means NONE<THREAD_SAFE is not searched up) ->
       all drop. */
    for (i = 0; i < 5; ++i) {
        mca_base_event_raise_internal(event, source, NULL, buf, MCA_BASE_EVENT_CB_REQUIRE_THREAD_SAFE);
    }
    check("no dropped-handler fire mid-run (no delivery yet)", 0 == dropped_fired);
    /* Now a NONE-context raise IS delivered, flushing the 5 drops first. */
    cb_fired = 0;
    mca_base_event_raise_internal(event, source, NULL, buf, MCA_BASE_EVENT_CB_REQUIRE_NONE);
    check("delivery after drops fired callback", 1 == cb_fired);
    check("dropped handler fired once on flush", 1 == dropped_fired);
    check("dropped count == 5", 5 == (int) dropped_count_seen);

    /* A clean delivery does not re-fire the dropped handler. */
    dropped_fired = 0;
    mca_base_event_raise_internal(event, source, NULL, buf, MCA_BASE_EVENT_CB_REQUIRE_NONE);
    check("clean delivery does not re-fire dropped handler", 0 == dropped_fired);

    rc = mca_base_event_handle_free(reg, NULL, NULL);
    check("handle_free", OPAL_SUCCESS == rc);
    check("double handle_free rejected", OPAL_SUCCESS != mca_base_event_handle_free(reg, NULL, NULL));
    check("event inactive after free", !mca_base_event_active(event));

    /* --- Free-from-inside-its-own-callback + free_cb fires exactly once. */
    rc = mca_base_event_handle_alloc(ev_index, NULL, NULL, &reg);
    check("handle_alloc (2)", OPAL_SUCCESS == rc);
    rc = mca_base_event_register_callback(reg, MCA_BASE_EVENT_CB_REQUIRE_NONE, NULL, NULL, NULL,
                                          self_free_cb);
    check("register self-free callback", OPAL_SUCCESS == rc);
    free_fired = 0;
    /* Use a wrapper free path: the self_free_cb calls handle_free(reg, NULL,
       NULL); to observe free_cb we instead free via a marker.  Re-register a
       handle that records its reg, then free it with a marker free_cb. */
    self_free_reg = NULL;
    (void) mca_base_event_register_callback(reg, MCA_BASE_EVENT_CB_REQUIRE_NONE, NULL, NULL, NULL,
                                            self_free_event_cb);
    mca_base_event_raise_internal(event, source, NULL, buf, MCA_BASE_EVENT_CB_REQUIRE_NONE);
    check("callback captured its reg", self_free_reg == reg);
    rc = mca_base_event_handle_free(reg, NULL, free_marker_cb);
    check("handle_free with free_cb", OPAL_SUCCESS == rc);
    check("free_cb fired exactly once", 1 == free_fired);

    /* --- ASYNC-only callback never fires; drops accrue; flush at handle_free. */
    rc = mca_base_event_handle_alloc(ev_index, NULL, NULL, &reg);
    check("handle_alloc (3)", OPAL_SUCCESS == rc);
    rc = mca_base_event_register_callback(reg, MCA_BASE_EVENT_CB_REQUIRE_ASYNC_SIGNAL_SAFE, NULL,
                                          NULL, NULL, event_cb);
    check("register ASYNC-only callback succeeds", OPAL_SUCCESS == rc);
    rc = mca_base_event_set_dropped_handler(reg, dropped_cb, NULL, NULL);
    check("set dropped handler (async-only)", OPAL_SUCCESS == rc);
    cb_fired = 0;
    dropped_fired = 0;
    dropped_count_seen = 0;
    for (i = 0; i < 3; ++i) {
        mca_base_event_raise_internal(event, source, NULL, buf, MCA_BASE_EVENT_CB_REQUIRE_NONE);
    }
    check("ASYNC-only callback never fires", 0 == cb_fired);
    check("ASYNC-only drops do not flush mid-run", 0 == dropped_fired);
    rc = mca_base_event_handle_free(reg, NULL, NULL);
    check("handle_free flushes ASYNC-only drops", OPAL_SUCCESS == rc);
    check("final flush fired dropped handler once", 1 == dropped_fired);
    check("final flush count == 3", 3 == (int) dropped_count_seen);

    /* --- All-drops raise consumes no pool instance (sec. 12.4); a delivery
       does (lazy acquisition). */
    rc = mca_base_event_handle_alloc(ev_index, NULL, NULL, &reg);
    check("handle_alloc (4)", OPAL_SUCCESS == rc);
    (void) mca_base_event_register_callback(reg, MCA_BASE_EVENT_CB_REQUIRE_NONE, NULL, NULL, NULL,
                                            event_cb);
    {
        int64_t acq_before = mca_base_event_test_pool_acquires();
        for (i = 0; i < 5; ++i) {
            mca_base_event_raise_internal(event, source, NULL, buf, MCA_BASE_EVENT_CB_REQUIRE_THREAD_SAFE);
        }
        check("all-drops raise consumes no instance",
              acq_before == mca_base_event_test_pool_acquires());
        mca_base_event_raise_internal(event, source, NULL, buf, MCA_BASE_EVENT_CB_REQUIRE_NONE);
        check("a delivery acquires exactly one instance",
              acq_before + 1 == mca_base_event_test_pool_acquires());
    }
    (void) mca_base_event_handle_free(reg, NULL, NULL);

    /* --- ORDERED timestamps are monotonic across successive deliveries. */
    {
        opal_count_t t1, t2;
        cb_timestamp = 0;
        mca_base_event_raise_internal(event, source, NULL, buf, MCA_BASE_EVENT_CB_REQUIRE_NONE);
        /* No callback now (handle freed); read the source ad hoc instead. */
        (void) mca_base_event_source_get_timestamp(src_index, &t1);
        (void) mca_base_event_source_get_timestamp(src_index, &t2);
        check("ORDERED source timestamps are non-decreasing", t2 >= t1);
    }

    /* --- Drop-restore on (forced) pool exhaustion: with m pending drops, a
       delivery that finds the pool exhausted must restore reg->dropped to
       m + 1 (the captured m plus this now-dropped raise). */
    rc = mca_base_event_handle_alloc(ev_index, NULL, NULL, &reg);
    check("handle_alloc (5)", OPAL_SUCCESS == rc);
    (void) mca_base_event_register_callback(reg, MCA_BASE_EVENT_CB_REQUIRE_NONE, NULL, NULL, NULL,
                                            event_cb);
    {
        int m = 4;
        /* Accumulate m drops (THREAD_SAFE finds no selectable callback). */
        for (i = 0; i < m; ++i) {
            mca_base_event_raise_internal(event, source, NULL, buf, MCA_BASE_EVENT_CB_REQUIRE_THREAD_SAFE);
        }
        mca_base_event_test_set_force_exhaustion(true);
        mca_base_event_raise_internal(event, source, NULL, buf, MCA_BASE_EVENT_CB_REQUIRE_NONE);
        mca_base_event_test_set_force_exhaustion(false);
        check("drop-restore on exhaustion: reg->dropped == m + 1",
              (int64_t) m + 1 == (int64_t) reg->dropped);
    }
    (void) mca_base_event_handle_free(reg, NULL, NULL);

    /* --- Ordering: the dropped handler runs BEFORE the event callback within
       the flushing delivery (sec. 5.9 step 3). */
    rc = mca_base_event_handle_alloc(ev_index, NULL, NULL, &reg);
    check("handle_alloc (ordering)", OPAL_SUCCESS == rc);
    (void) mca_base_event_register_callback(reg, MCA_BASE_EVENT_CB_REQUIRE_NONE, NULL, NULL, NULL,
                                            seq_event_cb);
    (void) mca_base_event_set_dropped_handler(reg, seq_dropped_cb, NULL, NULL);
    seq_n = 0;
    for (i = 0; i < 2; ++i) {
        mca_base_event_raise_internal(event, source, NULL, buf, MCA_BASE_EVENT_CB_REQUIRE_THREAD_SAFE);
    }
    mca_base_event_raise_internal(event, source, NULL, buf, MCA_BASE_EVENT_CB_REQUIRE_NONE);
    check("dropped handler runs before event callback (sequence 'DC')",
          2 == seq_n && 'D' == seq[0] && 'C' == seq[1]);
    (void) mca_base_event_handle_free(reg, NULL, NULL);

    /* --- Selection: deliver to the SMALLEST valid safety level, not the most
       permissive (sec. 2 / sec. 5.9). */
    rc = mca_base_event_handle_alloc(ev_index, NULL, NULL, &reg);
    check("handle_alloc (selection)", OPAL_SUCCESS == rc);
    (void) mca_base_event_register_callback(reg, MCA_BASE_EVENT_CB_REQUIRE_MPI_RESTRICTED, NULL,
                                            (void *) (intptr_t) MCA_BASE_EVENT_CB_REQUIRE_MPI_RESTRICTED,
                                            NULL, level_cb);
    (void) mca_base_event_register_callback(reg, MCA_BASE_EVENT_CB_REQUIRE_THREAD_SAFE, NULL,
                                            (void *) (intptr_t) MCA_BASE_EVENT_CB_REQUIRE_THREAD_SAFE,
                                            NULL, level_cb);
    selected_level = -1;
    mca_base_event_raise_internal(event, source, NULL, buf, MCA_BASE_EVENT_CB_REQUIRE_NONE);
    check("NONE-context raise picks the smallest valid level (MPI_RESTRICTED)",
          MCA_BASE_EVENT_CB_REQUIRE_MPI_RESTRICTED == selected_level);
    selected_level = -1;
    mca_base_event_raise_internal(event, source, NULL, buf, MCA_BASE_EVENT_CB_REQUIRE_THREAD_SAFE);
    check("THREAD_SAFE-context raise picks the THREAD_SAFE level",
          MCA_BASE_EVENT_CB_REQUIRE_THREAD_SAFE == selected_level);
    (void) mca_base_event_handle_free(reg, NULL, NULL);

    /* --- set_dropped_handler(NULL) discards pending drops (p.755). */
    rc = mca_base_event_handle_alloc(ev_index, NULL, NULL, &reg);
    check("handle_alloc (clear-drops)", OPAL_SUCCESS == rc);
    (void) mca_base_event_register_callback(reg, MCA_BASE_EVENT_CB_REQUIRE_NONE, NULL, NULL, NULL,
                                            event_cb);
    (void) mca_base_event_set_dropped_handler(reg, dropped_cb, NULL, NULL);
    for (i = 0; i < 4; ++i) {
        mca_base_event_raise_internal(event, source, NULL, buf, MCA_BASE_EVENT_CB_REQUIRE_THREAD_SAFE);
    }
    (void) mca_base_event_set_dropped_handler(reg, NULL, NULL, NULL);       /* clears */
    (void) mca_base_event_set_dropped_handler(reg, dropped_cb, NULL, NULL); /* re-arm */
    dropped_fired = 0;
    dropped_count_seen = 0;
    mca_base_event_raise_internal(event, source, NULL, buf, MCA_BASE_EVENT_CB_REQUIRE_NONE);
    check("set_dropped_handler(NULL) discarded the pending drops", 0 == dropped_fired);
    (void) mca_base_event_handle_free(reg, NULL, NULL);

    /* --- The per-thread dispatch-depth cap bounds an unconditional re-raise at
       exactly MCA_BASE_EVENT_MAX_DISPATCH_DEPTH (with a production-sized pool,
       so the depth cap -- not pool exhaustion -- is the limiter). */
    rc = mca_base_event_handle_alloc(ev_index, NULL, NULL, &reg);
    check("handle_alloc (recursion)", OPAL_SUCCESS == rc);
    (void) mca_base_event_register_callback(reg, MCA_BASE_EVENT_CB_REQUIRE_NONE, NULL, NULL, NULL,
                                            depth_cb);
    depth_now = 0;
    depth_max = 0;
    mca_base_event_raise_internal(event, source, NULL, buf, MCA_BASE_EVENT_CB_REQUIRE_NONE);
    check("re-raising callback is bounded at the dispatch-depth cap",
          MCA_BASE_EVENT_MAX_DISPATCH_DEPTH == depth_max);
    (void) mca_base_event_handle_free(reg, NULL, NULL);

    /* --- Heap-fallback snapshot: > 32 registrations on one event all deliver. */
    {
        mca_base_event_registration_t *many[40];
        int n = 40, ok_all = 1;
        for (i = 0; i < n; ++i) {
            if (OPAL_SUCCESS != mca_base_event_handle_alloc(ev_index, NULL, NULL, &many[i])) {
                ok_all = 0;
                break;
            }
            (void) mca_base_event_register_callback(many[i], MCA_BASE_EVENT_CB_REQUIRE_NONE, NULL,
                                                    NULL, NULL, event_cb);
        }
        check("alloc 40 registrations on one event", ok_all);
        cb_fired = 0;
        mca_base_event_raise_internal(event, source, NULL, buf, MCA_BASE_EVENT_CB_REQUIRE_NONE);
        check("one raise delivers to all 40 registrations (heap-grown snapshot)", 40 == cb_fired);
        for (i = 0; i < n; ++i) {
            (void) mca_base_event_handle_free(many[i], NULL, NULL);
        }
        check("event inactive after freeing all 40", !mca_base_event_active(event));
    }

    /* --- Concurrency: producer raising while consumer allocs/frees handles. */
    {
        pthread_t prod, cons;
        stress_event = event;
        stress_source = source;
        stress_ev_index = ev_index;
        stress_stop = 0;
        pthread_create(&prod, NULL, stress_producer, NULL);
        pthread_create(&cons, NULL, stress_consumer, NULL);
        pthread_join(cons, NULL);
        stress_stop = 1;
        pthread_join(prod, NULL);
        check("concurrent raise vs. alloc/free completes without crash/deadlock", 1);
        check("event inactive after concurrent stress", !mca_base_event_active(event));
    }

    /* --- For a NO_OBJECT event, a non-NULL obj_handle is IGNORED, not rejected
       (MPI-5.0 sec. 15.3.8): handle_alloc succeeds and ignores the pointer. */
    {
        int dummy = 0;
        rc = mca_base_event_handle_alloc(ev_index, &dummy, NULL, &reg);
        check("non-NULL obj_handle ignored for a NO_OBJECT event", OPAL_SUCCESS == rc);
        if (OPAL_SUCCESS == rc) {
            (void) mca_base_event_handle_free(reg, NULL, NULL);
        }
    }

    /* --- The fold mechanism (sec. 7.1) via a synthetic deferred-release
       source: a source with a fold-consume hook accumulates a {count, bytes}
       pair that the framework drains at a framework op (here
       source_get_timestamp) and delivers as ONE aggregated, data-bearing event.
       This runs on every platform (it does not need the real free-hooks). */
    {
        int fsi, fei, fsrc;
        mca_base_event_source_t *fsource = NULL;
        mca_base_event_t *fevent = NULL;
        mca_base_event_registration_t *freg = NULL;
        opal_count_t fts = 0;
        mca_base_var_type_t fu64x2[2] = {MCA_BASE_VAR_TYPE_UINT64_T, MCA_BASE_VAR_TYPE_UINT64_T};
        ptrdiff_t foff[2] = {0, 8};

        fsi = mca_base_event_source_register("test.fold", "synthetic fold source",
                                             MCA_BASE_EVENT_SOURCE_UNORDERED,
                                             (opal_count_t) 1000000000, OPAL_COUNT_MAX, NULL, true,
                                             NULL);
        check("fold source registered", fsi >= 0);
        (void) mca_base_event_source_get_by_index(fsi, &fsource);
        fei = mca_base_event_register("test", "fold", "comp", "released", "synthetic released",
                                      OPAL_INFO_LVL_4, 2, fu64x2, foff, NULL,
                                      MCA_BASE_VAR_BIND_NO_OBJECT, 0, fsource, NULL);
        check("fold event registered", fei >= 0);
        (void) mca_base_event_get_by_index(fei, &fevent);

        /* Wire the deferred-release hooks exactly as a real producer would. */
        fsource->fold_consume_fn = fold_test_consume;
        fsource->fold_event = fevent;
        fsrc = fsource->source_index;

        rc = mca_base_event_handle_alloc(fei, NULL, NULL, &freg);
        check("fold handle_alloc", OPAL_SUCCESS == rc);
        rc = mca_base_event_register_callback(freg, MCA_BASE_EVENT_CB_REQUIRE_NONE, NULL, NULL, NULL,
                                              fold_event_cb);
        check("fold register_callback", OPAL_SUCCESS == rc);

        (void) opal_atomic_swap_64(&fold_test_counter, 5);    /* 5 pending releases  */
        (void) opal_atomic_swap_64(&fold_test_bytes, 4096);   /* totalling 4096 bytes */
        fold_cb_fired = 0;
        fold_cb_count = fold_cb_bytes = 0;
        (void) mca_base_event_source_get_timestamp(fsrc, &fts); /* drives the fold */
        check("fold delivered one aggregated event", 1 == fold_cb_fired);
        check("aggregated event reports 5 releases", 5 == (int) fold_cb_count);
        check("aggregated event reports 4096 bytes", 4096 == (int) fold_cb_bytes);

        /* Consume-and-clear: a second fold with no new releases must not re-deliver. */
        fold_cb_fired = 0;
        (void) mca_base_event_source_get_timestamp(fsrc, &fts);
        check("repeated fold is non-cumulative (no second delivery)", 0 == fold_cb_fired);

        /* handle_free drains the source's pending releases before the
           registration detaches (sec. 7.1), so a tool that frees the handle
           WITHOUT first polling source_get_timestamp still observes its final
           batch. */
        (void) opal_atomic_swap_64(&fold_test_counter, 3);
        (void) opal_atomic_swap_64(&fold_test_bytes, 2048);
        fold_cb_fired = 0;
        fold_cb_count = fold_cb_bytes = 0;
        (void) mca_base_event_handle_free(freg, NULL, NULL);
        check("handle_free drains pending releases before detach", 1 == fold_cb_fired);
        check("handle_free fold reports 3 releases", 3 == (int) fold_cb_count);
    }

    /* --- OS-memory deferred-release fold (sec. 7.1): a tool's
       source_get_timestamp drains accumulated releases and delivers them as one
       aggregated {count, bytes} event.  The OS free-hook machinery lives in
       OPAL; here we register a local source/event and wire it up with
       mca_base_event_memory_attach() (the OMPI producer layer does the same with
       the real ompi.mca.memory.patcher.released name).  Only runs where the OPAL
       free-hooks are supported (e.g. Linux; absent on macOS).

       opal_init() does not open the memory framework -- nothing in this test
       pulls in an rcache or a common/{ofi,ucx} that would -- so the free-hook
       support level is 0 until we open it ourselves.  Without this, the block
       below silently never runs on any platform. */
    (void) mca_base_framework_open(&opal_memory_base_framework, 0);
    if (mca_base_event_memory_supported()) {
        const mca_base_var_type_t u64x2[2] = {MCA_BASE_VAR_TYPE_UINT64_T,
                                              MCA_BASE_VAR_TYPE_UINT64_T};
        const ptrdiff_t off[2] = {0, 8};
        mca_base_event_source_t *msource = NULL;
        mca_base_event_t *mev = NULL;
        mca_base_event_registration_t *mreg = NULL;
        opal_count_t mts = 0;
        int midx, msrc, si;

        /* Suppress the real free() hook so our bumps are the only releases. */
        mca_base_event_memory_test_suppress_hook(true);

        si = mca_base_event_source_register("test.os.memory", "white-box OS memory",
                                            MCA_BASE_EVENT_SOURCE_UNORDERED,
                                            (opal_count_t) 1000000000, OPAL_COUNT_MAX, NULL, true,
                                            NULL);
        check("os-memory source registered",
              0 <= si && OPAL_SUCCESS == mca_base_event_source_get_by_index(si, &msource));
        midx = mca_base_event_register("opal", "test", "osmem", "released", "released (white-box)",
                                       OPAL_INFO_LVL_4, 2, u64x2, off, NULL,
                                       MCA_BASE_VAR_BIND_NO_OBJECT, 0, msource, NULL);
        check("os-memory event registered",
              0 <= midx && OPAL_SUCCESS == mca_base_event_get_by_index(midx, &mev));

        /* Wire the OPAL OS-memory machinery into our source/event. */
        mca_base_event_memory_attach(msource, mev);
        msrc = mev->source->source_index;

        rc = mca_base_event_handle_alloc(midx, NULL, NULL, &mreg);
        check("memory handle_alloc", OPAL_SUCCESS == rc);
        rc = mca_base_event_register_callback(mreg, MCA_BASE_EVENT_CB_REQUIRE_NONE, NULL, NULL, NULL,
                                              fold_event_cb);
        check("memory register_callback", OPAL_SUCCESS == rc);

        (void) mca_base_event_memory_test_bump(5, 8192); /* 5 releases, 8192 bytes */
        fold_cb_fired = 0;
        fold_cb_count = fold_cb_bytes = 0;
        (void) mca_base_event_source_get_timestamp(msrc, &mts); /* drives the fold */
        check("memory fold delivered one aggregated event", 1 == fold_cb_fired);
        check("memory fold reports 5 releases", 5 == (int) fold_cb_count);
        check("memory fold reports 8192 bytes", 8192 == (int) fold_cb_bytes);

        /* Consume-and-clear: a second fold with no new releases must not re-deliver. */
        fold_cb_fired = 0;
        (void) mca_base_event_source_get_timestamp(msrc, &mts);
        check("repeated fold is non-cumulative (no second delivery)", 0 == fold_cb_fired);

        /* Detaching the last listener deactivates the producer. */
        (void) mca_base_event_handle_free(mreg, NULL, NULL);

        /* A hook still in flight when detach stored active=false can land a bump
           after the producer is deactivated.  Such residue must never be
           credited to the *next* listener: handle_alloc() folds the event before
           the new registration joins it (fold-before-membership-change, sec.
           7.1), so the stale count is raised to the then-empty registration set
           and discarded.  Watch the whole re-attach, not just the final fold. */
        (void) mca_base_event_memory_test_bump(3, 1024);

        fold_cb_fired = 0;
        fold_cb_count = fold_cb_bytes = 0;

        mreg = NULL;
        rc = mca_base_event_handle_alloc(midx, NULL, NULL, &mreg);
        check("memory handle_alloc (re-attach)", OPAL_SUCCESS == rc);
        rc = mca_base_event_register_callback(mreg, MCA_BASE_EVENT_CB_REQUIRE_NONE, NULL, NULL, NULL,
                                              fold_event_cb);
        check("memory register_callback (re-attach)", OPAL_SUCCESS == rc);
        (void) mca_base_event_source_get_timestamp(msrc, &mts);

        check("post-detach residue is not credited to the next listener",
              0 == fold_cb_fired && 0 == (int) fold_cb_count && 0 == (int) fold_cb_bytes);

        (void) mca_base_event_handle_free(mreg, NULL, NULL);
        mca_base_event_memory_test_suppress_hook(false);
    } else {
        printf("(ompi.unordered memory producer absent: no free-hook support on this platform)\n");
    }

    /* --- Dump routines: parsable + readable formatting for an event and a
       source.  Each hands back a fresh, NULL-terminated, caller-owned char*
       array (calloc'd by the dump routine, one opal_asprintf'd string per
       line); the caller frees every line, then the array. */
    {
        char **strings;
        int    j, n;

        /* Event, parsable: lines look like
           "mpi_t_event:type:<name>:<field>:<value>". */
        strings = NULL;
        check("event_dump (parsable) succeeds",
              OPAL_SUCCESS == mca_base_event_dump(ev_index, &strings, MCA_BASE_VAR_DUMP_PARSABLE));
        check("event_dump (parsable) returned an array", NULL != strings);
        if (NULL != strings) {
            int saw_prefix = 0;
            for (n = 0; NULL != strings[n]; ++n) {
                if (NULL != strstr(strings[n], "mpi_t_event:")) {
                    saw_prefix = 1;
                }
            }
            check("event_dump (parsable) is NULL-terminated with >= 1 line", n >= 1);
            check("event_dump (parsable) line carries the mpi_t_event: prefix", saw_prefix);
            for (j = 0; j < n; ++j) {
                free(strings[j]);
            }
            free(strings);
        }

        /* Event, readable: a human summary line (+ optional description). */
        strings = NULL;
        check("event_dump (readable) succeeds",
              OPAL_SUCCESS == mca_base_event_dump(ev_index, &strings, MCA_BASE_VAR_DUMP_READABLE));
        check("event_dump (readable) returned an array", NULL != strings);
        if (NULL != strings) {
            for (n = 0; NULL != strings[n]; ++n) {
                continue;
            }
            check("event_dump (readable) is NULL-terminated with >= 1 line", n >= 1);
            for (j = 0; j < n; ++j) {
                free(strings[j]);
            }
            free(strings);
        }

        /* Source, parsable: lines look like
           "mpi_t_event:source:<name>:<field>:<value>". */
        strings = NULL;
        check("source_dump (parsable) succeeds",
              OPAL_SUCCESS
                  == mca_base_event_source_dump(src_index, &strings, MCA_BASE_VAR_DUMP_PARSABLE));
        check("source_dump (parsable) returned an array", NULL != strings);
        if (NULL != strings) {
            int saw_prefix = 0;
            for (n = 0; NULL != strings[n]; ++n) {
                if (NULL != strstr(strings[n], "mpi_t_event:")) {
                    saw_prefix = 1;
                }
            }
            check("source_dump (parsable) is NULL-terminated with >= 1 line", n >= 1);
            check("source_dump (parsable) line carries the mpi_t_event: prefix", saw_prefix);
            for (j = 0; j < n; ++j) {
                free(strings[j]);
            }
            free(strings);
        }

        /* Source, readable: a human summary line (+ optional description). */
        strings = NULL;
        check("source_dump (readable) succeeds",
              OPAL_SUCCESS
                  == mca_base_event_source_dump(src_index, &strings, MCA_BASE_VAR_DUMP_READABLE));
        check("source_dump (readable) returned an array", NULL != strings);
        if (NULL != strings) {
            for (n = 0; NULL != strings[n]; ++n) {
                continue;
            }
            check("source_dump (readable) is NULL-terminated with >= 1 line", n >= 1);
            for (j = 0; j < n; ++j) {
                free(strings[j]);
            }
            free(strings);
        }
    }

    /* --- Object binding (sec. 6.1): a bound event reaches ONLY the registration
       bound to the raised object; NO_OBJECT raises (above) reach everyone. */
    {
        int bev_index;
        mca_base_event_t *bevent;
        mca_base_event_registration_t *regA, *regB;
        /* Opaque fake object identities (never dereferenced; matched by value). */
        void *objA = (void *) (uintptr_t) 0xA1A1;
        void *objB = (void *) (uintptr_t) 0xB2B2;

        bev_index = mca_base_event_register("test", "test", "bound", "bound",
                                            "an object-bound test event", OPAL_INFO_LVL_4, 2, types,
                                            offsets, NULL, MCA_BASE_VAR_BIND_MPI_COMM, 0, source,
                                            NULL);
        check("bound event_register succeeds", bev_index >= 0);
        rc = mca_base_event_get_by_index(bev_index, &bevent);
        check("bound event_get_by_index", OPAL_SUCCESS == rc);
        check("bound event reports its MPI_COMM binding", MCA_BASE_VAR_BIND_MPI_COMM == bevent->bind);

        /* A bound event requires an object: a NULL obj_handle is rejected. */
        rc = mca_base_event_handle_alloc(bev_index, NULL, NULL, &regA);
        check("bound handle_alloc rejects a NULL obj_handle", OPAL_ERR_BAD_PARAM == rc);

        /* obj_handle is the ADDRESS of the handle variable (MPI-5.0 p.751). */
        rc = mca_base_event_handle_alloc(bev_index, &objA, NULL, &regA);
        check("bound handle_alloc (object A)", OPAL_SUCCESS == rc);
        rc = mca_base_event_handle_alloc(bev_index, &objB, NULL, &regB);
        check("bound handle_alloc (object B)", OPAL_SUCCESS == rc);
        (void) mca_base_event_register_callback(regA, MCA_BASE_EVENT_CB_REQUIRE_NONE, NULL, objA,
                                                NULL, bound_cb);
        (void) mca_base_event_register_callback(regB, MCA_BASE_EVENT_CB_REQUIRE_NONE, NULL, objB,
                                                NULL, bound_cb);

        /* Raise for object A: only A's registration is notified. */
        bound_count = 0;
        bound_last = NULL;
        mca_base_event_raise_internal(bevent, source, objA, buf, MCA_BASE_EVENT_CB_REQUIRE_NONE);
        check("bound raise(A) delivered exactly once", 1 == bound_count);
        check("bound raise(A) reached the A registration only", objA == bound_last);

        /* Raise for object B: only B's registration is notified. */
        bound_count = 0;
        bound_last = NULL;
        mca_base_event_raise_internal(bevent, source, objB, buf, MCA_BASE_EVENT_CB_REQUIRE_NONE);
        check("bound raise(B) delivered exactly once", 1 == bound_count);
        check("bound raise(B) reached the B registration only", objB == bound_last);

        /* Raise for an object nobody bound to: delivered to no one. */
        bound_count = 0;
        mca_base_event_raise_internal(bevent, source, (void *) (uintptr_t) 0xCCCC, buf,
                                      MCA_BASE_EVENT_CB_REQUIRE_NONE);
        check("bound raise(unbound object) reaches no registration", 0 == bound_count);

        (void) mca_base_event_handle_free(regA, NULL, NULL);
        (void) mca_base_event_handle_free(regB, NULL, NULL);
    }

    opal_finalize();

    printf("RESULT: %s (%d failures)\n", 0 == failures ? "PASS" : "FAIL", failures);
    return 0 == failures ? 0 : 1;
}
