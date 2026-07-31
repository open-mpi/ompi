/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Self-contained MPI_T events test: the test program itself acts as an event
 * *producer*, registering its own event source(s) and event type(s) through the
 * internal mca_base_event_* API, then driving every MPI_T-level path against
 * them.  This keeps the tool-facing paths covered without relying on any shipped
 * (and possibly platform-dependent) producer:
 *
 *   1. ad-hoc MPI_T_source_get_timestamp on a default-clock source, read
 *      BEFORE any handle is allocated -- so the clock origin is captured on the
 *      read itself, not left at zero (a zero origin would return a timestamp in
 *      the wrong epoch);
 *   2. MPI_T_ERR_NOT_SUPPORTED from a source without ad-hoc timestamp support;
 *   3. safety-level dropping: a NONE callback is delivered at a NONE-context
 *      raise but dropped at a THREAD_SAFE-context raise, and the dropped-event
 *      handler reports the accumulated count at handle_free;
 *   4. a multi-field payload read element-by-element and copied whole, plus the
 *      in-callback MPI_T_event_get_source accessor.
 */

#include <mpi.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "opal/constants.h"
#include "opal/mca/base/mca_base_var.h"
#include "opal/mca/base/mca_base_pvar.h"
#include "opal/mca/base/mca_base_event.h"

static int       failures = 0;
static int       cb_fired = 0;
static int       dropped_fired = 0;
static MPI_Count dropped_count = 0;
static int32_t   ms_a = 0;
static int64_t   ms_b = 0;
static uint32_t  ms_c = 0;
static int       got_source = -1;
static int       copy_ok = 0;
static int       copy_canary_ok = 0;

static void expect(const char *what, int cond)
{
    printf("%s: %s\n", cond ? "PASS" : "FAIL", what);
    if (!cond) {
        ++failures;
    }
}

static void lossy_cb(MPI_T_event_instance ev __opal_attribute_unused__,
                     MPI_T_event_registration h __opal_attribute_unused__,
                     MPI_T_cb_safety s __opal_attribute_unused__,
                     void *u __opal_attribute_unused__)
{
    ++cb_fired;
}

static void dropped_cb(MPI_Count count, MPI_T_event_registration h __opal_attribute_unused__,
                       int src __opal_attribute_unused__, MPI_T_cb_safety s __opal_attribute_unused__,
                       void *u __opal_attribute_unused__)
{
    ++dropped_fired;
    dropped_count = count;
}

static void multi_cb(MPI_T_event_instance ev,
                     MPI_T_event_registration h __opal_attribute_unused__,
                     MPI_T_cb_safety s __opal_attribute_unused__,
                     void *u __opal_attribute_unused__)
{
    struct {
        int32_t  a;
        int32_t  pad;
        int64_t  b;
        uint32_t c;
    } buf;
    (void) MPI_T_event_read(ev, 0, &ms_a);
    (void) MPI_T_event_read(ev, 1, &ms_b);
    (void) MPI_T_event_read(ev, 2, &ms_c);
    (void) MPI_T_event_get_source(ev, &got_source);
    memset(&buf, 0, sizeof(buf));
    if (MPI_SUCCESS == MPI_T_event_copy(ev, &buf)) {
        copy_ok = (buf.a == ms_a && buf.b == ms_b && buf.c == ms_c);
    }

    /* Regression: a conforming tool sizes its buffer to the un-rounded element
       extent (here 20 bytes = offset 16 + a 4-byte uint32), so event_copy must
       not write past it -- it must not memset the internally 8-byte-rounded
       buffer_size (24).  Copy into a buffer whose bytes from offset 20 on are a
       canary and confirm they survive. */
    {
        unsigned char guarded[32];
        memset(guarded, 0x5a, sizeof(guarded));
        if (MPI_SUCCESS == MPI_T_event_copy(ev, guarded)) {
            int k;
            copy_canary_ok = 1;
            for (k = 20; k < (int) sizeof(guarded); ++k) {
                if (0x5a != guarded[k]) {
                    copy_canary_ok = 0;
                    break;
                }
            }
        }
    }
}

/* Find an event whose name contains both substrings; -1 if none. */
static int find_event(int num, const char *a, const char *b)
{
    for (int i = 0; i < num; ++i) {
        char name[256];
        int nl = sizeof(name), ne = 0;
        if (MPI_SUCCESS
                == MPI_T_event_get_info(i, name, &nl, NULL, NULL, NULL, &ne, NULL, NULL, NULL, NULL,
                                        NULL)
            && NULL != strstr(name, a) && NULL != strstr(name, b)) {
            return i;
        }
    }
    return -1;
}

int main(int argc, char **argv)
{
    int provided, ev_num = 0, src_num = 0;
    int src_a, src_b, lossy_idx, multi_idx;
    mca_base_event_source_t *sp = NULL;
    mca_base_event_t *lossy_ev = NULL, *multi_ev = NULL;

    const mca_base_var_type_t lossy_types[1] = {MCA_BASE_VAR_TYPE_INT32_T};
    const ptrdiff_t lossy_off[1] = {0};
    const mca_base_var_type_t multi_types[3] = {MCA_BASE_VAR_TYPE_INT32_T,
                                                MCA_BASE_VAR_TYPE_INT64_T,
                                                MCA_BASE_VAR_TYPE_UINT32_T};
    const ptrdiff_t multi_off[3] = {0, 8, 16};

    if (MPI_SUCCESS != MPI_Init(&argc, &argv)) {
        printf("SKIP: MPI_Init failed (no singleton support here)\n");
        return 77;
    }
    (void) MPI_T_init_thread(MPI_THREAD_SINGLE, &provided);

    /* ---- The test registers its OWN source(s) and event type(s). ---- */
    src_a = mca_base_event_source_register("test.selftest", "self-test default-clock source",
                                           MCA_BASE_EVENT_SOURCE_ORDERED, (opal_count_t) 1000000000,
                                           (opal_count_t) INT64_MAX, NULL, true, NULL);
    src_b = mca_base_event_source_register("test.selftest.noclock", "self-test no-adhoc source",
                                           MCA_BASE_EVENT_SOURCE_ORDERED, (opal_count_t) 1000000000,
                                           (opal_count_t) INT64_MAX, NULL, false, NULL);
    expect("test source A (ad-hoc) registered", src_a >= 0);
    expect("test source B (no ad-hoc) registered", src_b >= 0);
    expect("got source A pointer",
           src_a >= 0 && OPAL_SUCCESS == mca_base_event_source_get_by_index(src_a, &sp));

    if (NULL != sp) {
        lossy_idx = mca_base_event_register("test", "selftest", "base", "lossy",
                                            "self-test lossy event", OPAL_INFO_LVL_4, 1, lossy_types,
                                            lossy_off, NULL, MCA_BASE_VAR_BIND_NO_OBJECT, 0, sp, NULL);
        multi_idx = mca_base_event_register("test", "selftest", "base", "multi",
                                            "self-test multi-field event", OPAL_INFO_LVL_4, 3,
                                            multi_types, multi_off, NULL, MCA_BASE_VAR_BIND_NO_OBJECT,
                                            0, sp, NULL);
        expect("test lossy event registered", lossy_idx >= 0);
        expect("test multi event registered", multi_idx >= 0);
        (void) mca_base_event_get_by_index(lossy_idx, &lossy_ev);
        (void) mca_base_event_get_by_index(multi_idx, &multi_ev);
    }

    /* The test producer must now be visible through the MPI_T enumeration. */
    (void) MPI_T_event_get_num(&ev_num);
    (void) MPI_T_source_get_num(&src_num);
    expect("MPI_T enumerates the test lossy event", find_event(ev_num, "selftest", "lossy") >= 0);
    expect("MPI_T enumerates the test multi event", find_event(ev_num, "selftest", "multi") >= 0);
    expect("MPI_T enumerates the test sources", src_num > src_a && src_num > src_b);

    /* ---- 1. Ad-hoc timestamp on the default clock, read BEFORE any handle is
       allocated, so the origin is captured on this read (not left at zero). ---- */
    if (src_a >= 0) {
        MPI_Count ts1 = -1, ts2 = -1;
        int rc1 = MPI_T_source_get_timestamp(src_a, &ts1);
        int rc2 = MPI_T_source_get_timestamp(src_a, &ts2);
        expect("ad-hoc source_get_timestamp returns MPI_SUCCESS", MPI_SUCCESS == rc1);
        /* A captured origin yields a tiny value; a zero origin would yield
           ~1.7e18 ns (since the Unix epoch).  Reject the latter. */
        expect("ad-hoc timestamp is relative to the source origin, not the epoch",
               ts1 >= 0 && ts1 < (MPI_Count) 1000000000000000LL);
        expect("ad-hoc timestamp is monotonic", MPI_SUCCESS == rc2 && ts2 >= ts1);
    }

    /* ---- 2. A source without ad-hoc support reports NOT_SUPPORTED. ---- */
    if (src_b >= 0) {
        MPI_Count ts = -1;
        int rc = MPI_T_source_get_timestamp(src_b, &ts);
        expect("no-adhoc source_get_timestamp returns MPI_T_ERR_NOT_SUPPORTED",
               MPI_T_ERR_NOT_SUPPORTED == rc);
    }

    /* ---- 3. Safety-level dropping + the dropped-event handler. ---- */
    if (NULL != lossy_ev) {
        const int deliver = 3, drop = 5;
        MPI_T_event_registration reg;
        int32_t v = 7;
        lossy_idx = find_event(ev_num, "selftest", "lossy");
        (void) MPI_T_event_handle_alloc(lossy_idx, NULL, MPI_INFO_NULL, &reg);
        (void) MPI_T_event_register_callback(reg, MPI_T_CB_REQUIRE_NONE, MPI_INFO_NULL, NULL,
                                             lossy_cb);
        (void) MPI_T_event_set_dropped_handler(reg, dropped_cb);

        cb_fired = dropped_fired = 0;
        dropped_count = 0;
        /* Delivered: a NONE callback is selectable at a NONE-context raise. */
        for (int i = 0; i < deliver; ++i) {
            mca_base_event_raise(lossy_ev, NULL, &v);
        }
        /* Dropped: the same NONE callback cannot be selected at a THREAD_SAFE
           raise, so every one of these is accounted as a drop. */
        for (int i = 0; i < drop; ++i) {
            mca_base_event_raise_internal(lossy_ev, NULL, NULL, &v,
                                          MCA_BASE_EVENT_CB_REQUIRE_THREAD_SAFE);
        }
        expect("NONE callback delivered at a NONE-context raise", cb_fired == deliver);

        /* Info round-trip on the live handle and its NONE-level callback:
           get_info hands back a fresh, user-freeable MPI_Info (never
           MPI_INFO_NULL); set_info with MPI_INFO_NULL is accepted.  The
           callback-info calls use the level the callback was registered at. */
        {
            MPI_Info hinfo = MPI_INFO_NULL, cinfo = MPI_INFO_NULL;
            expect("event_handle_get_info returns MPI_SUCCESS",
                   MPI_SUCCESS == MPI_T_event_handle_get_info(reg, &hinfo));
            expect("event_handle_get_info returns a non-null info", MPI_INFO_NULL != hinfo);
            if (MPI_INFO_NULL != hinfo) {
                MPI_Info_free(&hinfo);
            }
            expect("event_callback_get_info returns MPI_SUCCESS",
                   MPI_SUCCESS
                       == MPI_T_event_callback_get_info(reg, MPI_T_CB_REQUIRE_NONE, &cinfo));
            expect("event_callback_get_info returns a non-null info", MPI_INFO_NULL != cinfo);
            if (MPI_INFO_NULL != cinfo) {
                MPI_Info_free(&cinfo);
            }
            expect("event_handle_set_info(MPI_INFO_NULL) returns MPI_SUCCESS",
                   MPI_SUCCESS == MPI_T_event_handle_set_info(reg, MPI_INFO_NULL));
            expect("event_callback_set_info(MPI_INFO_NULL) returns MPI_SUCCESS",
                   MPI_SUCCESS
                       == MPI_T_event_callback_set_info(reg, MPI_T_CB_REQUIRE_NONE, MPI_INFO_NULL));
        }

        (void) MPI_T_event_handle_free(reg, NULL, NULL);
        expect("dropped handler flushed at handle_free", dropped_fired >= 1);
        expect("dropped handler reported every THREAD_SAFE raise as dropped",
               (int) dropped_count == drop);

        /* get_info on the now-freed handle reports an invalid handle. */
        {
            MPI_Info dead = MPI_INFO_NULL;
            expect("event_handle_get_info on a freed handle returns MPI_T_ERR_INVALID_HANDLE",
                   MPI_T_ERR_INVALID_HANDLE == MPI_T_event_handle_get_info(reg, &dead));
        }
    }

    /* ---- 4. Multi-field payload: read each element, copy the whole buffer,
       and read the source from inside the callback. ---- */
    if (NULL != multi_ev) {
        struct {
            int32_t  a;
            int32_t  pad;
            int64_t  b;
            uint32_t c;
        } payload;
        MPI_T_event_registration reg;
        memset(&payload, 0, sizeof(payload));
        payload.a = -42;
        payload.b = 0x1122334455667788LL;
        payload.c = 0xdeadbeefu;
        multi_idx = find_event(ev_num, "selftest", "multi");
        ms_a = 0; ms_b = 0; ms_c = 0; got_source = -1; copy_ok = 0; copy_canary_ok = 0;
        (void) MPI_T_event_handle_alloc(multi_idx, NULL, MPI_INFO_NULL, &reg);
        (void) MPI_T_event_register_callback(reg, MPI_T_CB_REQUIRE_NONE, MPI_INFO_NULL, NULL,
                                             multi_cb);
        mca_base_event_raise(multi_ev, NULL, &payload);
        expect("multi-field event_read returns each element",
               ms_a == payload.a && ms_b == payload.b && ms_c == payload.c);
        expect("event_copy returns the full payload", copy_ok);
        expect("event_copy does not write past the element extent", copy_canary_ok);
        expect("in-callback get_source returns the event's source", got_source == src_a);
        (void) MPI_T_event_handle_free(reg, NULL, NULL);
    }

    MPI_T_finalize();
    MPI_Finalize();

    printf("RESULT: %s (%d failures)\n", 0 == failures ? "PASS" : "FAIL", failures);
    return 0 == failures ? 0 : 1;
}
