/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 *
 * Concurrent MPI_T performance variable sessions on one variable.
 *
 * Each handle bound to a performance variable is on that variable's list
 * of bound handles, which every session shares.  Freeing a session frees
 * the handles still in it, which removes them from that list and calls
 * the variable's notify callback.  MPI_T_pvar_session_free() did that
 * without the MPI_T lock, so it could corrupt the list while another
 * thread was allocating or freeing a handle on the same variable (both
 * of which do hold the lock).
 *
 * One thread frees its sessions with the handle still allocated (so the
 * session frees it); the other frees its handles explicitly.  A race here
 * is probabilistic: a failure (a crash, or an assertion in a debug build)
 * proves the bug, a pass is only evidence.
 */

#include "mpi_t_lifecycle.h"

#include <pthread.h>
#include <signal.h>
#include <stdatomic.h>
#include <unistd.h>

#define ITERATIONS 20000

static int pvar_index = -1;
static _Atomic int failed = 0;

static void timed_out(int sig)
{
    static const char msg[] = "FAIL: timed out (deadlock?)\n";

    (void) sig;
    (void) write(STDOUT_FILENO, msg, sizeof(msg) - 1);
    _exit(1);
}

static void *session_frees_handle(void *arg)
{
    MPI_T_pvar_session session;
    MPI_T_pvar_handle handle;
    int count;

    (void) arg;
    for (int i = 0; i < ITERATIONS && !failed; ++i) {
        if (MPI_SUCCESS != MPI_T_pvar_session_create(&session)
            || MPI_SUCCESS != MPI_T_pvar_handle_alloc(session, pvar_index, NULL, &handle, &count)
            || MPI_SUCCESS != MPI_T_pvar_session_free(&session)) {
            printf("FAIL: MPI_T call failed (session frees the handle, iteration %d)\n", i);
            failed = 1;
        }
    }
    return NULL;
}

static void *handle_freed_explicitly(void *arg)
{
    MPI_T_pvar_session session;
    MPI_T_pvar_handle handle;
    int count;

    (void) arg;
    for (int i = 0; i < ITERATIONS && !failed; ++i) {
        if (MPI_SUCCESS != MPI_T_pvar_session_create(&session)
            || MPI_SUCCESS != MPI_T_pvar_handle_alloc(session, pvar_index, NULL, &handle, &count)
            || MPI_SUCCESS != MPI_T_pvar_handle_free(session, &handle)
            || MPI_SUCCESS != MPI_T_pvar_session_free(&session)) {
            printf("FAIL: MPI_T call failed (handle freed explicitly, iteration %d)\n", i);
            failed = 1;
        }
    }
    return NULL;
}

int main(void)
{
    int provided, num, name_len, desc_len, verbosity, var_class, bind;
    int readonly, continuous, atomic;
    MPI_Datatype datatype;
    MPI_T_enum enumtype;
    char name[256];
    pthread_t t1, t2;

    signal(SIGALRM, timed_out);
    alarm(300);

    STEP_OR_SKIP(MPI_T_init_thread(MPI_THREAD_MULTIPLE, &provided), "T_init");
    if (MPI_THREAD_MULTIPLE != provided) {
        printf("SKIP: MPI_T does not provide MPI_THREAD_MULTIPLE\n");
        MPI_T_finalize();
        return 77;
    }

    /* Any performance variable that is not bound to an MPI object and
       that a handle can be allocated for will do.  MPI_T_init_thread()
       registers the variables of every component, so there are some even
       though MPI is not initialized, but some of them cannot be used until
       their provider is initialized. */
    STEP(MPI_T_pvar_get_num(&num), "pvar_get_num");
    for (int i = 0; i < num; ++i) {
        MPI_T_pvar_session session;
        MPI_T_pvar_handle handle;
        int count, usable = 0;

        name_len = sizeof(name);
        desc_len = 0;
        if (MPI_SUCCESS != MPI_T_pvar_get_info(i, name, &name_len, &verbosity, &var_class,
                                               &datatype, &enumtype, NULL, &desc_len, &bind,
                                               &readonly, &continuous, &atomic)
            || MPI_T_BIND_NO_OBJECT != bind) {
            continue;
        }

        STEP(MPI_T_pvar_session_create(&session), "pvar_session_create (probe)");
        if (MPI_SUCCESS == MPI_T_pvar_handle_alloc(session, i, NULL, &handle, &count)) {
            usable = 1;
            STEP(MPI_T_pvar_handle_free(session, &handle), "pvar_handle_free (probe)");
        }
        STEP(MPI_T_pvar_session_free(&session), "pvar_session_free (probe)");

        if (usable) {
            pvar_index = i;
            break;
        }
    }
    if (0 > pvar_index) {
        printf("SKIP: no usable performance variable that is not bound to an MPI object\n");
        MPI_T_finalize();
        return 77;
    }
    printf("using performance variable %s\n", name);
    fflush(stdout);

    if (0 != pthread_create(&t1, NULL, session_frees_handle, NULL)
        || 0 != pthread_create(&t2, NULL, handle_freed_explicitly, NULL)) {
        printf("SKIP: could not create threads\n");
        return 77;
    }
    pthread_join(t1, NULL);
    pthread_join(t2, NULL);

    STEP(MPI_T_finalize(), "T_finalize");

    if (failed) {
        return 1;
    }
    printf("SUCCESS: %d concurrent session/handle cycles on one variable\n", 2 * ITERATIONS);
    return 0;
}
