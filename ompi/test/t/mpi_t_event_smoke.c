/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Singleton smoke test for the MPI_T events tool layer (Phase 3): it
 * registers a source + event type directly via the OPAL mca_base_event
 * API, then drives the full MPI_T path -- enumerate, get_info,
 * handle_alloc, register_callback -- raises the event through OPAL, and
 * confirms the user callback fires THROUGH the OMPI trampoline with the
 * payload, timestamp, and source readable via the MPI_T accessors.
 * Also checks the index/name error contracts.
 */

#include <mpi.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "opal/mca/base/mca_base_event.h"

static int      failures = 0;
static int      cb_fired = 0;
static int32_t  cb_a = 0;
static uint64_t cb_b = 0;
static int      cb_src = -1;

static void expect(const char *what, int cond)
{
    if (cond) {
        printf("PASS: %s\n", what);
    } else {
        ++failures;
        printf("FAIL: %s\n", what);
    }
}

/* User MPI_T event callback -- reached only through the OMPI trampoline. */
static void my_cb(MPI_T_event_instance event,
                  MPI_T_event_registration handle __opal_attribute_unused__,
                  MPI_T_cb_safety cb_safety __opal_attribute_unused__,
                  void *user_data __opal_attribute_unused__)
{
    MPI_Count ts = 0;
    ++cb_fired;
    (void) MPI_T_event_read(event, 0, &cb_a);
    (void) MPI_T_event_read(event, 1, &cb_b);
    (void) MPI_T_event_get_timestamp(event, &ts);
    (void) MPI_T_event_get_source(event, &cb_src);
}

int main(void)
{
    int provided, rc, ev_index, src_index, num, bind, verbosity;
    mca_base_event_source_t *source;
    mca_base_event_t *event;
    MPI_T_event_registration reg;
    mca_base_var_type_t types[2] = {MCA_BASE_VAR_TYPE_INT32_T, MCA_BASE_VAR_TYPE_UINT64_T};
    ptrdiff_t offsets[2] = {0, 8};
    unsigned char buf[16];
    char name[256];
    int name_len = sizeof(name);

    if (MPI_SUCCESS != MPI_T_init_thread(MPI_THREAD_SINGLE, &provided)) {
        printf("FAIL: MPI_T_init_thread\nRESULT: FAIL\n");
        return 1;
    }

    /* Register a source + event type via the OPAL back end. */
    src_index = mca_base_event_source_register("test.smoke.src", "smoke source",
                                               MCA_BASE_EVENT_SOURCE_ORDERED, 1000000000,
                                               OPAL_COUNT_MAX, NULL, true, NULL);
    expect("OPAL source_register", src_index >= 0);
    (void) mca_base_event_source_get_by_index(src_index, &source);
    ev_index = mca_base_event_register("test", "smoke", "comp", "event", "smoke event",
                                       OPAL_INFO_LVL_4, 2, types, offsets, NULL,
                                       MPI_T_BIND_NO_OBJECT, 0, source, NULL);
    expect("OPAL event_register", ev_index >= 0);
    (void) mca_base_event_get_by_index(ev_index, &event);

    /* MPI_T sees the registered event. */
    (void) MPI_T_event_get_num(&num);
    expect("MPI_T_event_get_num >= 1", num >= 1);

    rc = MPI_T_event_get_info(ev_index, name, &name_len, &verbosity, NULL, NULL, &num, NULL, NULL,
                              NULL, NULL, &bind);
    expect("MPI_T_event_get_info ok", MPI_SUCCESS == rc);
    expect("get_info reports 2 elements", 2 == num);
    expect("get_info reports NO_OBJECT bind", MPI_T_BIND_NO_OBJECT == bind);

    /* The event appears in its category (category_get_events, Phase 4). */
    {
        int cat = event->group_index, nevents = 0, idx[32], k, found = 0;
        expect("event has a category", cat >= 0);
        (void) MPI_T_category_get_num_events(cat, &nevents);
        expect("category_get_num_events >= 1", nevents >= 1);
        expect("category_get_events ok", MPI_SUCCESS == MPI_T_category_get_events(cat, 32, idx));
        for (k = 0; k < nevents && k < 32; ++k) {
            if (idx[k] == ev_index) {
                found = 1;
            }
        }
        expect("category_get_events lists our event", found);
    }

    /* Index/name error contracts. */
    expect("event_get_info(bad index) -> INVALID_INDEX",
           MPI_T_ERR_INVALID_INDEX == MPI_T_event_get_info(999999, NULL, NULL, NULL, NULL, NULL,
                                                           &num, NULL, NULL, NULL, NULL, NULL));
    {
        int bogus;
        expect("event_get_index(bogus) -> INVALID_NAME",
               MPI_T_ERR_INVALID_NAME == MPI_T_event_get_index("no.such.event", &bogus));
        expect("event_get_index(real name) round-trips",
               MPI_SUCCESS == MPI_T_event_get_index(event->name, &bogus) && bogus == ev_index);
    }

    /* Alloc a handle + register a callback through the MPI_T API. */
    rc = MPI_T_event_handle_alloc(ev_index, NULL, MPI_INFO_NULL, &reg);
    expect("MPI_T_event_handle_alloc", MPI_SUCCESS == rc);
    rc = MPI_T_event_register_callback(reg, MPI_T_CB_REQUIRE_NONE, MPI_INFO_NULL, NULL, my_cb);
    expect("MPI_T_event_register_callback", MPI_SUCCESS == rc);

    /* Raise through OPAL; the user callback must fire through the trampoline. */
    {
        int32_t v32 = 7;
        uint64_t v64 = 0xfeedface;
        /* memcpy, not an aligned store: buf[] (a char array) has no guaranteed
           8-byte alignment, so *(uint64_t *)(buf + 8) would be a misaligned
           access (SIGBUS on strict-alignment targets). */
        memcpy(buf + 0, &v32, sizeof(v32));
        memcpy(buf + 8, &v64, sizeof(v64));
    }
    cb_fired = 0;
    mca_base_event_raise_internal(event, source, NULL, buf, MCA_BASE_EVENT_CB_REQUIRE_NONE);
    expect("callback fired through the trampoline", 1 == cb_fired);
    expect("MPI_T_event_read element 0 (via trampoline path)", 7 == cb_a);
    expect("MPI_T_event_read element 1", 0xfeedface == cb_b);
    expect("MPI_T_event_get_source", src_index == cb_src);

    /* For a NO_OBJECT event, a non-NULL obj_handle must be IGNORED, not rejected
       (MPI-5.0 sec. 15.3.8): handle_alloc succeeds and ignores the pointer. */
    {
        int dummy = 0;
        MPI_T_event_registration r2;
        expect("non-NULL obj_handle ignored for a NO_OBJECT event",
               MPI_SUCCESS == MPI_T_event_handle_alloc(ev_index, &dummy, MPI_INFO_NULL, &r2));
        (void) MPI_T_event_handle_free(r2, NULL, NULL);
    }

    rc = MPI_T_event_handle_free(reg, NULL, NULL);
    expect("MPI_T_event_handle_free", MPI_SUCCESS == rc);

    MPI_T_finalize();

    printf("RESULT: %s (%d failures)\n", 0 == failures ? "PASS" : "FAIL", failures);
    return 0 == failures ? 0 : 1;
}
