/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 *
 * Singleton regression test for how MPI_T handles variable indices and
 * object-bound handles.
 *
 * Index space (MPI-5.0 sec. 15.3.6, 15.3.7, 15.3.8).  A bad index must
 * be reported as MPI_T_ERR_INVALID_INDEX, not as the catch-all
 * MPI_T_ERR_INVALID, whether the index is out of range (negative or
 * past the count) or in range but naming a variable that has been
 * invalidated -- which happens routinely, since a variable may never be
 * removed from the index space once registered, so the variables of a
 * component that is built but not selected stay visible to
 * MPI_T_*_get_num() while being unusable.  A negative performance
 * variable index used to escape the bounds check entirely.
 *
 * Object binding (MPI-5.0 sec. 15.3.2).  Allocating a handle for an
 * object-bound variable must accept a live object of the bound type and
 * reject a null one.  The validity check used to be handed the address
 * of the caller's handle variable rather than the handle itself, so what
 * it read was whatever happened to sit on the caller's stack.
 *
 * Runs as a 'make check' singleton (singleton MPI_Init needs no
 * launcher).
 */

#include <mpi.h>
#include <stdio.h>

#define NAME_LEN 256

static int failures = 0;

static void expect(const char *what, int cond)
{
    if (cond) {
        printf("PASS: %s\n", what);
    } else {
        ++failures;
        printf("FAIL: %s\n", what);
    }
}

/* Report an MPI_T return code against the one the standard asks for. */
static void expect_rc(const char *what, int got, int want)
{
    char detail[NAME_LEN];
    snprintf(detail, sizeof(detail), "%s (got %d, want %d)", what, got, want);
    expect(detail, got == want);
}

static int pvar_info(int index)
{
    char name[NAME_LEN];
    int name_len = NAME_LEN, desc_len = 0, verbosity, var_class, bind;
    int readonly, continuous, atomic;
    MPI_Datatype datatype;
    MPI_T_enum enumtype;

    return MPI_T_pvar_get_info(index, name, &name_len, &verbosity, &var_class,
                               &datatype, &enumtype, NULL, &desc_len, &bind,
                               &readonly, &continuous, &atomic);
}

static int cvar_info(int index)
{
    char name[NAME_LEN];
    int name_len = NAME_LEN, desc_len = 0, verbosity, bind, scope;
    MPI_Datatype datatype;
    MPI_T_enum enumtype;

    return MPI_T_cvar_get_info(index, name, &name_len, &verbosity, &datatype,
                               &enumtype, NULL, &desc_len, &bind, &scope);
}

static int event_info(int index)
{
    char name[NAME_LEN];
    int name_len = NAME_LEN, desc_len = 0, verbosity, bind, num_elements;
    MPI_Datatype array_of_datatypes[1];
    MPI_Aint array_of_displacements[1];
    MPI_T_enum enumtype;

    /* num_elements is in/out: zero in says "do not describe the payload",
       which keeps the two arrays untouched. */
    num_elements = 0;
    return MPI_T_event_get_info(index, name, &name_len, &verbosity,
                                array_of_datatypes, array_of_displacements,
                                &num_elements, &enumtype, NULL, NULL,
                                &desc_len, &bind);
}

/* Every index below the reported count must be either usable or reported
   as MPI_T_ERR_INVALID_INDEX; nothing else is a legal answer. */
static void check_in_range(const char *kind, int num, int (*info)(int))
{
    char detail[NAME_LEN];
    int usable = 0, invalid = 0, other = 0;

    for (int i = 0; i < num; ++i) {
        int rc = info(i);
        if (MPI_SUCCESS == rc) {
            ++usable;
        } else if (MPI_T_ERR_INVALID_INDEX == rc) {
            ++invalid;
        } else {
            if (0 == other) {
                printf("       first unexpected code for %s index %d: %d\n",
                       kind, i, rc);
            }
            ++other;
        }
    }

    printf("       %s: %d of %d usable, %d invalidated\n", kind, usable, num,
           invalid);
    snprintf(detail, sizeof(detail),
             "every in-range %s index is usable or MPI_T_ERR_INVALID_INDEX",
             kind);
    expect(detail, 0 == other);
}

/* Find a performance variable bound to a communicator, or -1. */
static int find_comm_bound_pvar(int num, char *name_out, int name_out_len)
{
    for (int i = 0; i < num; ++i) {
        char name[NAME_LEN];
        int name_len = NAME_LEN, desc_len = 0, verbosity, var_class, bind;
        int readonly, continuous, atomic;
        MPI_Datatype datatype;
        MPI_T_enum enumtype;

        if (MPI_SUCCESS
                == MPI_T_pvar_get_info(i, name, &name_len, &verbosity, &var_class,
                                       &datatype, &enumtype, NULL, &desc_len,
                                       &bind, &readonly, &continuous, &atomic)
            && MPI_T_BIND_MPI_COMM == bind) {
            snprintf(name_out, name_out_len, "%s", name);
            return i;
        }
    }

    return -1;
}

/* Allocate and immediately release a handle for pvar 'index' bound to
   'obj_handle', and return the allocation's error code. */
static int try_bind(MPI_T_pvar_session session, int index, void *obj_handle)
{
    MPI_T_pvar_handle handle;
    int count = 0;
    int rc = MPI_T_pvar_handle_alloc(session, index, obj_handle, &handle, &count);

    if (MPI_SUCCESS == rc) {
        MPI_T_pvar_handle_free(session, &handle);
    }

    return rc;
}

static void check_binding(int num_pvar)
{
    char name[NAME_LEN];
    MPI_T_pvar_session session;
    MPI_Comm world = MPI_COMM_WORLD, self = MPI_COMM_SELF;
    MPI_Comm null = MPI_COMM_NULL;
    int index = find_comm_bound_pvar(num_pvar, name, sizeof(name));

    if (0 > index) {
        printf("       no communicator-bound performance variable in this "
               "build; skipping the binding checks\n");
        return;
    }

    printf("       using performance variable %d (%s)\n", index, name);

    if (MPI_SUCCESS != MPI_T_pvar_session_create(&session)) {
        expect("MPI_T_pvar_session_create", 0);
        return;
    }

    expect_rc("a handle bound to MPI_COMM_WORLD is accepted",
              try_bind(session, index, &world), MPI_SUCCESS);
    expect_rc("a handle bound to MPI_COMM_SELF is accepted",
              try_bind(session, index, &self), MPI_SUCCESS);
    /* MPI_COMM_NULL is the one invalid communicator that can be probed
       safely: it is a real predefined object, so the validity check has
       something to read.  A communicator that has actually been freed may
       have had its memory released, and reading a class name back out of
       it would be a use-after-free. */
    expect_rc("a handle bound to MPI_COMM_NULL is rejected",
              try_bind(session, index, &null), MPI_T_ERR_INVALID_HANDLE);

    /* A duplicate behaves like any other live communicator. */
    MPI_Comm dup;
    MPI_Comm_dup(MPI_COMM_WORLD, &dup);
    expect_rc("a handle bound to a duplicated communicator is accepted",
              try_bind(session, index, &dup), MPI_SUCCESS);
    MPI_Comm_free(&dup);

    MPI_T_pvar_session_free(&session);
}

int main(int argc, char **argv)
{
    int num_cvar = 0, num_pvar = 0, num_event = 0;
    MPI_T_pvar_session session;
    MPI_T_pvar_handle pvar_handle;
    MPI_T_cvar_handle cvar_handle;
    int count = 0, provided = 0;

    MPI_Init(&argc, &argv);
    MPI_T_init_thread(MPI_THREAD_SINGLE, &provided);

    MPI_T_cvar_get_num(&num_cvar);
    MPI_T_pvar_get_num(&num_pvar);
    MPI_T_event_get_num(&num_event);
    printf("counts: %d control variables, %d performance variables, "
           "%d event types\n", num_cvar, num_pvar, num_event);

    /* An out-of-range index -- below the start of the space or past its
       end -- is MPI_T_ERR_INVALID_INDEX for all three kinds. */
    expect_rc("MPI_T_pvar_get_info(-1)", pvar_info(-1), MPI_T_ERR_INVALID_INDEX);
    expect_rc("MPI_T_pvar_get_info(num_pvar)", pvar_info(num_pvar),
              MPI_T_ERR_INVALID_INDEX);
    expect_rc("MPI_T_cvar_get_info(-1)", cvar_info(-1), MPI_T_ERR_INVALID_INDEX);
    expect_rc("MPI_T_cvar_get_info(num_cvar)", cvar_info(num_cvar),
              MPI_T_ERR_INVALID_INDEX);
    expect_rc("MPI_T_event_get_info(-1)", event_info(-1),
              MPI_T_ERR_INVALID_INDEX);
    expect_rc("MPI_T_event_get_info(num_event)", event_info(num_event),
              MPI_T_ERR_INVALID_INDEX);

    /* Handle allocation reports a bad index the same way get_info does. */
    if (MPI_SUCCESS == MPI_T_pvar_session_create(&session)) {
        expect_rc("MPI_T_pvar_handle_alloc(-1)",
                  MPI_T_pvar_handle_alloc(session, -1, NULL, &pvar_handle, &count),
                  MPI_T_ERR_INVALID_INDEX);
        expect_rc("MPI_T_pvar_handle_alloc(num_pvar)",
                  MPI_T_pvar_handle_alloc(session, num_pvar, NULL, &pvar_handle,
                                          &count),
                  MPI_T_ERR_INVALID_INDEX);
        MPI_T_pvar_session_free(&session);
    } else {
        expect("MPI_T_pvar_session_create", 0);
    }

    expect_rc("MPI_T_cvar_handle_alloc(-1)",
              MPI_T_cvar_handle_alloc(-1, NULL, &cvar_handle, &count),
              MPI_T_ERR_INVALID_INDEX);
    expect_rc("MPI_T_cvar_handle_alloc(num_cvar)",
              MPI_T_cvar_handle_alloc(num_cvar, NULL, &cvar_handle, &count),
              MPI_T_ERR_INVALID_INDEX);

    check_in_range("control variable", num_cvar, cvar_info);
    check_in_range("performance variable", num_pvar, pvar_info);
    check_in_range("event type", num_event, event_info);

    check_binding(num_pvar);

    MPI_T_finalize();
    MPI_Finalize();

    printf("RESULT: %s\n", 0 == failures ? "PASS" : "FAIL");
    return 0 == failures ? 0 : 1;
}
