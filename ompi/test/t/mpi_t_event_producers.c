/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Singleton test for the in-tree MPI_T event producers: after MPI_Init the
 * built-in event types must be registered, and a real MPI operation must drive
 * the corresponding producer.  It registers callbacks on the communicator,
 * error-handler, and RMA window producers and confirms they fire with the
 * expected payloads (including the MPI object handles) across MPI_Comm_dup/free,
 * a returning error handler, and MPI_Win_create/free.  It also exercises the
 * initialization/finalization events via the session model -- checking the
 * model element and that the instance ID correlates finalization with
 * initialization -- and checks the two-source layout: "ompi" is ORDERED and
 * "ompi.unordered" is UNORDERED.
 */

#include "opal_config.h"

#include <mpi.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

static int failures = 0;

static int      created_fired = 0;
static int      freed_fired = 0;
static int32_t  created_size = -1;
static uint64_t created_handle = 0;
static uint64_t freed_handle = 0;

static int      init_fired = 0;
static int      final_fired = 0;
static int32_t  init_model = -1;
static int32_t  init_world_rank = 999;
static int32_t  init_world_size = 999;
static uint64_t init_instance_id = 0;
static int32_t  final_model = -1;
static int32_t  final_world_rank = 999;
static uint64_t final_instance_id = 0;

static int      errh_fired = 0;
static int32_t  errh_object_type = -1;
static uint64_t errh_errhandler_handle = 0;
static uint64_t errh_object_handle = 0;

static int      win_created_fired = 0;
static int      win_freed_fired = 0;
static int64_t  win_size = -1;
static int32_t  win_disp = -1;
static int32_t  win_flavor = -1;
static uint64_t win_created_handle = 0;
static uint64_t win_freed_handle = 0;

static void expect(const char *what, int cond)
{
    printf("%s: %s\n", cond ? "PASS" : "FAIL", what);
    if (!cond) {
        ++failures;
    }
}

static void created_cb(MPI_T_event_instance ev,
                       MPI_T_event_registration h __opal_attribute_unused__,
                       MPI_T_cb_safety s __opal_attribute_unused__,
                       void *u __opal_attribute_unused__)
{
    ++created_fired;
    (void) MPI_T_event_read(ev, 0, &created_size);    /* int32 size        */
    (void) MPI_T_event_read(ev, 1, &created_handle);  /* uint64 MPI_Comm   */
}

static void freed_cb(MPI_T_event_instance ev,
                     MPI_T_event_registration h __opal_attribute_unused__,
                     MPI_T_cb_safety s __opal_attribute_unused__,
                     void *u __opal_attribute_unused__)
{
    ++freed_fired;
    (void) MPI_T_event_read(ev, 0, &freed_handle);    /* uint64 MPI_Comm   */
}

static void init_cb(MPI_T_event_instance ev,
                    MPI_T_event_registration h __opal_attribute_unused__,
                    MPI_T_cb_safety s __opal_attribute_unused__,
                    void *u __opal_attribute_unused__)
{
    ++init_fired;
    (void) MPI_T_event_read(ev, 0, &init_model);
    (void) MPI_T_event_read(ev, 2, &init_world_rank); /* -1 for a session  */
    (void) MPI_T_event_read(ev, 3, &init_world_size); /* -1 for a session  */
    (void) MPI_T_event_read(ev, 4, &init_instance_id);
}

static void final_cb(MPI_T_event_instance ev,
                     MPI_T_event_registration h __opal_attribute_unused__,
                     MPI_T_cb_safety s __opal_attribute_unused__,
                     void *u __opal_attribute_unused__)
{
    ++final_fired;
    (void) MPI_T_event_read(ev, 0, &final_model);
    (void) MPI_T_event_read(ev, 1, &final_world_rank);
    (void) MPI_T_event_read(ev, 2, &final_instance_id);
}

static void errh_cb(MPI_T_event_instance ev,
                    MPI_T_event_registration h __opal_attribute_unused__,
                    MPI_T_cb_safety s __opal_attribute_unused__,
                    void *u __opal_attribute_unused__)
{
    ++errh_fired;
    (void) MPI_T_event_read(ev, 1, &errh_object_type);
    (void) MPI_T_event_read(ev, 2, &errh_errhandler_handle);
    (void) MPI_T_event_read(ev, 3, &errh_object_handle);
}

static void win_created_cb(MPI_T_event_instance ev,
                           MPI_T_event_registration h __opal_attribute_unused__,
                           MPI_T_cb_safety s __opal_attribute_unused__,
                           void *u __opal_attribute_unused__)
{
    ++win_created_fired;
    (void) MPI_T_event_read(ev, 0, &win_size);          /* int64 local size  */
    (void) MPI_T_event_read(ev, 1, &win_disp);          /* int32 disp_unit   */
    (void) MPI_T_event_read(ev, 2, &win_flavor);        /* int32 win flavor  */
    (void) MPI_T_event_read(ev, 3, &win_created_handle); /* uint64 MPI_Win   */
}

static void win_freed_cb(MPI_T_event_instance ev,
                         MPI_T_event_registration h __opal_attribute_unused__,
                         MPI_T_cb_safety s __opal_attribute_unused__,
                         void *u __opal_attribute_unused__)
{
    ++win_freed_fired;
    (void) MPI_T_event_read(ev, 1, &win_freed_handle);  /* uint64 MPI_Win    */
}

/* Find an event whose name contains both substrings; -1 if none. */
static int find_event(int num, const char *a, const char *b)
{
    int i;
    for (i = 0; i < num; ++i) {
        char name[256];
        int name_len = sizeof(name), nelem = 0;
        if (MPI_SUCCESS
            == MPI_T_event_get_info(i, name, &name_len, NULL, NULL, NULL, &nelem, NULL, NULL,
                                    NULL, NULL, NULL)) {
            if (NULL != strstr(name, a) && NULL != strstr(name, b)) {
                return i;
            }
        }
    }
    return -1;
}

int main(int argc, char **argv)
{
    int provided, num = 0, created_idx, freed_idx;
    MPI_T_event_registration creg, freg;
    MPI_Comm dup;

    if (MPI_SUCCESS != MPI_Init(&argc, &argv)) {
        printf("SKIP: MPI_Init failed (no singleton support here)\n");
        return 77;
    }
    (void) MPI_T_init_thread(MPI_THREAD_SINGLE, &provided);

    (void) MPI_T_event_get_num(&num);
    expect("at least the built-in producers are registered", num >= 7);

    created_idx = find_event(num, "communicator", "created");
    freed_idx = find_event(num, "communicator", "freed");
    expect("communicator-created event is registered", created_idx >= 0);
    expect("communicator-freed event is registered", freed_idx >= 0);

    if (created_idx >= 0 && freed_idx >= 0) {
        expect("alloc created handle",
               MPI_SUCCESS == MPI_T_event_handle_alloc(created_idx, NULL, MPI_INFO_NULL, &creg));
        expect("alloc freed handle",
               MPI_SUCCESS == MPI_T_event_handle_alloc(freed_idx, NULL, MPI_INFO_NULL, &freg));
        (void) MPI_T_event_register_callback(creg, MPI_T_CB_REQUIRE_NONE, MPI_INFO_NULL, NULL,
                                             created_cb);
        (void) MPI_T_event_register_callback(freg, MPI_T_CB_REQUIRE_NONE, MPI_INFO_NULL, NULL,
                                             freed_cb);

        created_fired = freed_fired = 0;
        created_handle = freed_handle = 0;
        MPI_Comm_dup(MPI_COMM_WORLD, &dup);
        expect("communicator-created fired on MPI_Comm_dup", created_fired >= 1);
        expect("created payload carries the new comm size", 1 == created_size);
        expect("created payload carries the MPI_Comm handle", created_handle == (uint64_t) (uintptr_t) dup);

        MPI_Comm_free(&dup);
        expect("communicator-freed fired on MPI_Comm_free", freed_fired >= 1);
        expect("freed handle pairs with the created handle", freed_handle == created_handle);

        /* The non-blocking creation path (MPI_Comm_idup) must also raise the
           created event, so it pairs with the freed event the same way the
           blocking path does. */
        {
            MPI_Comm idup;
            MPI_Request req;
            created_fired = freed_fired = 0;
            MPI_Comm_idup(MPI_COMM_SELF, &idup, &req);
            MPI_Wait(&req, MPI_STATUS_IGNORE);
            expect("communicator-created fired on MPI_Comm_idup", created_fired >= 1);
            MPI_Comm_free(&idup);
            expect("communicator-freed fired after MPI_Comm_idup", freed_fired >= 1);
        }

        (void) MPI_T_event_handle_free(creg, NULL, NULL);
        (void) MPI_T_event_handle_free(freg, NULL, NULL);
    }

    /* Initialization/finalization producer: ompi.mpi.initialization and
       ompi.mpi.finalization fire for BOTH MPI models.  The world-model events
       fire during MPI_Init/MPI_Finalize (outside this test's observation
       window), so drive them here via the session model, which works in a
       singleton, and confirm the model element says "session" with
       world_rank/size = -1 (a process has no rank in a session) and that the
       instance ID correlates finalization with initialization. */
    {
        int init_idx = find_event(num, "initialization", "");
        int final_idx = find_event(num, "finalization", "");
        expect("initialization event is registered", init_idx >= 0);
        expect("finalization event is registered", final_idx >= 0);
        if (init_idx >= 0 && final_idx >= 0) {
            MPI_T_event_registration ireg, freg2;
            MPI_Session session;
            (void) MPI_T_event_handle_alloc(init_idx, NULL, MPI_INFO_NULL, &ireg);
            (void) MPI_T_event_handle_alloc(final_idx, NULL, MPI_INFO_NULL, &freg2);
            (void) MPI_T_event_register_callback(ireg, MPI_T_CB_REQUIRE_NONE, MPI_INFO_NULL, NULL,
                                                 init_cb);
            (void) MPI_T_event_register_callback(freg2, MPI_T_CB_REQUIRE_NONE, MPI_INFO_NULL, NULL,
                                                 final_cb);
            init_fired = final_fired = 0;
            init_instance_id = final_instance_id = 0;
            MPI_Session_init(MPI_INFO_NULL, MPI_ERRORS_RETURN, &session);
            expect("initialization fired on MPI_Session_init", init_fired >= 1);
            expect("initialization model says session", OMPI_T_MODEL_SESSION == init_model);
            expect("initialization world_rank is -1 for a session", -1 == init_world_rank);
            expect("initialization world_size is -1 for a session", -1 == init_world_size);
            expect("initialization carries a non-zero instance id", 0 != init_instance_id);
            MPI_Session_finalize(&session);
            expect("finalization fired on MPI_Session_finalize", final_fired >= 1);
            expect("finalization model says session", OMPI_T_MODEL_SESSION == final_model);
            expect("finalization world_rank is -1 for a session", -1 == final_world_rank);
            expect("finalization instance id correlates with initialization",
                   final_instance_id == init_instance_id);
            (void) MPI_T_event_handle_free(ireg, NULL, NULL);
            (void) MPI_T_event_handle_free(freg2, NULL, NULL);
        }
    }

    /* Error-handler producer: errhandler.invoked fires whenever an MPI error
       handler runs.  Trigger one with a returning handler (no abort) on
       MPI_COMM_SELF and confirm the payload identifies the object. */
    {
        int eidx = find_event(num, "errhandler", "invoked");
        expect("errhandler-invoked event registered", eidx >= 0);
        if (eidx >= 0) {
            MPI_T_event_registration ereg;
            int dummy = 0;
            (void) MPI_T_event_handle_alloc(eidx, NULL, MPI_INFO_NULL, &ereg);
            (void) MPI_T_event_register_callback(ereg, MPI_T_CB_REQUIRE_NONE, MPI_INFO_NULL, NULL,
                                                 errh_cb);
            errh_fired = 0;
            errh_object_handle = 0;
            MPI_Comm_set_errhandler(MPI_COMM_SELF, MPI_ERRORS_RETURN);
            /* An invalid count is rejected and routed to the comm's (returning)
               error handler, which raises errhandler.invoked. */
            (void) MPI_Bcast(&dummy, -1, MPI_INT, 0, MPI_COMM_SELF);
            expect("errhandler-invoked fired when an error handler ran", errh_fired >= 1);
            expect("errhandler payload object_type is MPI_T_BIND_MPI_COMM",
                   MPI_T_BIND_MPI_COMM == errh_object_type);
            expect("errhandler payload carries the MPI object handle (MPI_COMM_SELF)",
                   errh_object_handle == (uint64_t) (uintptr_t) MPI_COMM_SELF);
            expect("errhandler payload carries a non-zero errhandler handle",
                   0 != errh_errhandler_handle);
            (void) MPI_T_event_handle_free(ereg, NULL, NULL);
        }
    }

    /* RMA window producer: window-created/freed fire on MPI_Win_create/free.
       The "created" payload is {size, disp_unit, flavor, handle}. */
    {
        int wc_idx = find_event(num, "win", "created");
        int wf_idx = find_event(num, "win", "freed");
        expect("window-created event registered", wc_idx >= 0);
        expect("window-freed event registered", wf_idx >= 0);
        if (wc_idx >= 0 && wf_idx >= 0) {
            MPI_T_event_registration wcreg, wfreg;
            MPI_Win win;
            int buf[16];
            (void) MPI_T_event_handle_alloc(wc_idx, NULL, MPI_INFO_NULL, &wcreg);
            (void) MPI_T_event_handle_alloc(wf_idx, NULL, MPI_INFO_NULL, &wfreg);
            (void) MPI_T_event_register_callback(wcreg, MPI_T_CB_REQUIRE_NONE, MPI_INFO_NULL, NULL,
                                                 win_created_cb);
            (void) MPI_T_event_register_callback(wfreg, MPI_T_CB_REQUIRE_NONE, MPI_INFO_NULL, NULL,
                                                 win_freed_cb);
            win_created_fired = win_freed_fired = 0;
            win_created_handle = win_freed_handle = 0;
            MPI_Win_create(buf, sizeof(buf), 4, MPI_INFO_NULL, MPI_COMM_SELF, &win);
            expect("window-created fired on MPI_Win_create", win_created_fired >= 1);
            expect("created payload carries the local size", (int64_t) sizeof(buf) == win_size);
            expect("created payload carries the disp_unit", 4 == win_disp);
            expect("created payload carries the flavor", MPI_WIN_FLAVOR_CREATE == win_flavor);
            expect("created payload carries the MPI_Win handle",
                   win_created_handle == (uint64_t) (uintptr_t) win);
            MPI_Win_free(&win);
            expect("window-freed fired on MPI_Win_free", win_freed_fired >= 1);
            expect("freed handle pairs with the created handle",
                   win_freed_handle == win_created_handle);
            (void) MPI_T_event_handle_free(wcreg, NULL, NULL);
            (void) MPI_T_event_handle_free(wfreg, NULL, NULL);
        }
    }

    /* The in-tree producers use exactly two sources, distinguished by ordering:
       the consolidated "ompi" source is ORDERED (it carries every lifecycle
       event, including the error handler), and "ompi.unordered" -- present only
       where the OS memory-release hooks are supported -- is UNORDERED. */
    {
        int ns = 0, i, found_ordered = -1, found_unordered = -1;
        (void) MPI_T_source_get_num(&ns);
        for (i = 0; i < ns; ++i) {
            char sname[256];
            int slen = sizeof(sname);
            MPI_T_source_order ordering = MPI_T_SOURCE_ORDERED;
            MPI_Count tps = 0, mt = 0;
            if (MPI_SUCCESS
                != MPI_T_source_get_info(i, sname, &slen, NULL, NULL, &ordering, &tps, &mt, NULL)) {
                continue;
            }
            if (0 == strcmp(sname, "ompi")) {
                found_ordered = i;
                expect("the ompi source is ORDERED", MPI_T_SOURCE_ORDERED == ordering);
            } else if (0 == strcmp(sname, "ompi.unordered")) {
                found_unordered = i;
                expect("the ompi.unordered source is UNORDERED",
                       MPI_T_SOURCE_UNORDERED == ordering);
            }
        }
        expect("found the ompi (ordered) source", found_ordered >= 0);
        (void) found_unordered; /* ompi.unordered is platform-dependent */
    }

    MPI_T_finalize();
    MPI_Finalize();

    printf("RESULT: %s (%d failures)\n", 0 == failures ? "PASS" : "FAIL", failures);
    return 0 == failures ? 0 : 1;
}
