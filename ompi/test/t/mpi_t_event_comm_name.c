/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Coverage for the object-bound ompi.mpi.communicator_name_set event.  Unlike
 * the other producer events (which are NO_OBJECT and reach every registration),
 * this event is bound to a specific communicator: a tool allocates a
 * registration handle bound to one MPI_Comm and is notified ONLY when THAT
 * communicator is named.  We bind one registration to each of two communicators
 * and confirm that naming one fires only its own bound callback, that the
 * payload carries that communicator's handle, and that the new name is queryable
 * on the bound communicator (the fixed payload does not carry the name string).
 */

#include "opal_config.h"

#include <mpi.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

static int      failures = 0;
static int      a_fired = 0;
static int      b_fired = 0;
static uint64_t a_handle = 0;

static void expect(const char *what, int cond)
{
    printf("%s: %s\n", cond ? "PASS" : "FAIL", what);
    if (!cond) {
        ++failures;
    }
}

static void cb_a(MPI_T_event_instance ev,
                 MPI_T_event_registration h __opal_attribute_unused__,
                 MPI_T_cb_safety s __opal_attribute_unused__,
                 void *u __opal_attribute_unused__)
{
    ++a_fired;
    (void) MPI_T_event_read(ev, 0, &a_handle);
}

static void cb_b(MPI_T_event_instance ev __opal_attribute_unused__,
                 MPI_T_event_registration h __opal_attribute_unused__,
                 MPI_T_cb_safety s __opal_attribute_unused__,
                 void *u __opal_attribute_unused__)
{
    ++b_fired;
}

int main(int argc, char **argv)
{
    int provided, idx, rlen;
    MPI_Comm comm_a, comm_b;
    MPI_T_event_registration reg_a, reg_b;
    char namebuf[MPI_MAX_OBJECT_NAME];

    if (MPI_SUCCESS != MPI_Init(&argc, &argv)) {
        printf("SKIP: MPI_Init failed (no singleton support here)\n");
        return 77;
    }
    if (MPI_SUCCESS != MPI_T_init_thread(MPI_THREAD_SINGLE, &provided)) {
        printf("SKIP: MPI_T_init_thread failed\n");
        MPI_Finalize();
        return 77;
    }
    if (MPI_SUCCESS != MPI_T_event_get_index("ompi.mpi.communicator_name_set", &idx)) {
        printf("SKIP: ompi.mpi.communicator_name_set event not registered\n");
        MPI_T_finalize();
        MPI_Finalize();
        return 77;
    }

    MPI_Comm_dup(MPI_COMM_SELF, &comm_a);
    MPI_Comm_dup(MPI_COMM_SELF, &comm_b);

    /* Bind one registration to each communicator (obj_handle is the ADDRESS of
       the MPI_Comm handle variable). */
    expect("alloc handle bound to comm_a",
           MPI_SUCCESS == MPI_T_event_handle_alloc(idx, &comm_a, MPI_INFO_NULL, &reg_a));
    expect("alloc handle bound to comm_b",
           MPI_SUCCESS == MPI_T_event_handle_alloc(idx, &comm_b, MPI_INFO_NULL, &reg_b));
    (void) MPI_T_event_register_callback(reg_a, MPI_T_CB_REQUIRE_NONE, MPI_INFO_NULL, NULL, cb_a);
    (void) MPI_T_event_register_callback(reg_b, MPI_T_CB_REQUIRE_NONE, MPI_INFO_NULL, NULL, cb_b);

    /* Name comm_a: only comm_a's bound registration is notified. */
    a_fired = b_fired = 0;
    a_handle = 0;
    MPI_Comm_set_name(comm_a, "alpha");
    expect("naming comm_a fired its bound callback exactly once", 1 == a_fired);
    expect("naming comm_a did NOT fire comm_b's bound callback", 0 == b_fired);
    expect("name_set payload handle is comm_a's MPI_Comm handle",
           a_handle == (uint64_t) (uintptr_t) comm_a);

    /* The new name is queryable on the bound communicator. */
    rlen = 0;
    namebuf[0] = '\0';
    MPI_Comm_get_name(comm_a, namebuf, &rlen);
    expect("MPI_Comm_get_name on the bound communicator returns the new name",
           0 == strcmp(namebuf, "alpha"));

    /* Name comm_b: only comm_b's bound registration is notified. */
    a_fired = b_fired = 0;
    MPI_Comm_set_name(comm_b, "beta");
    expect("naming comm_b fired its bound callback exactly once", 1 == b_fired);
    expect("naming comm_b did NOT fire comm_a's bound callback", 0 == a_fired);

    (void) MPI_T_event_handle_free(reg_a, NULL, NULL);
    (void) MPI_T_event_handle_free(reg_b, NULL, NULL);
    MPI_Comm_free(&comm_a);
    MPI_Comm_free(&comm_b);
    MPI_T_finalize();
    MPI_Finalize();

    printf("RESULT: %s (%d failures)\n", 0 == failures ? "PASS" : "FAIL", failures);
    return 0 == failures ? 0 : 1;
}
