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
 * MPI_T reads racing component teardown: a tool thread reads every
 * control and performance variable in a loop while the main thread
 * brings an MPI session up and down.
 *
 * Instance initialization closes and unloads every component that is
 * not selected, and closing a component deregisters its control
 * variables (freeing their string values) and invalidates its
 * performance variables.  While MPI_T holds its framework references
 * that happens only the first time a framework is opened in an MPI_T
 * "epoch", so each cycle below is a whole epoch: MPI_T_init_thread(),
 * start the reader, MPI_Session_init(), MPI_Session_finalize(), stop the
 * reader, MPI_T_finalize().
 *
 * The reader tolerates any MPI_T error (a variable can legitimately
 * disappear under it); what it must never see is a crash, a hang, or a
 * read of freed memory (which needs AddressSanitizer or similar to be
 * caught reliably).  A race here is probabilistic: a failure proves a
 * bug, a pass is only evidence.
 */

#include "mpi_t_lifecycle.h"

#include <pthread.h>
#include <signal.h>
#include <stdatomic.h>
#include <string.h>
#include <unistd.h>

#define CYCLES 10
#define MAX_COUNT 64
#define NAME_LEN 256

static _Atomic int stop = 0;
static _Atomic long reads = 0;

static void timed_out(int sig)
{
    static const char msg[] = "FAIL: timed out (deadlock?)\n";

    (void) sig;
    (void) write(STDOUT_FILENO, msg, sizeof(msg) - 1);
    _exit(1);
}

/* Large enough for MAX_COUNT values of any MPI_T datatype */
static long long buf[MAX_COUNT];

/* A buffer for count values of datatype, or NULL.  A string gets a buffer
   of exactly the count that allocating its handle reported, as the MPI
   standard says to size it; a string can be several kilobytes long (e.g.,
   the component path that "make check" sets). */
static void *get_buffer(MPI_Datatype datatype, int count)
{
    if (MPI_CHAR == datatype) {
        return malloc((size_t) count);
    }
    return (count <= MAX_COUNT) ? buf : NULL;
}

static void put_buffer(void *b)
{
    if (b != (void *) buf) {
        free(b);
    }
}

static void read_cvars(void)
{
    int num, count, rc, bind, verbosity, scope, name_len, desc_len;
    MPI_Datatype datatype;
    MPI_T_enum enumtype;
    MPI_T_cvar_handle handle;
    char name[NAME_LEN];

    if (MPI_SUCCESS != MPI_T_cvar_get_num(&num)) {
        return;
    }
    for (int i = 0; i < num && !stop; ++i) {
        name_len = NAME_LEN;
        desc_len = 0;
        rc = MPI_T_cvar_get_info(i, name, &name_len, &verbosity, &datatype, &enumtype, NULL,
                                 &desc_len, &bind, &scope);
        if (MPI_SUCCESS != rc || MPI_T_BIND_NO_OBJECT != bind) {
            continue;
        }
        if (MPI_SUCCESS != MPI_T_cvar_handle_alloc(i, NULL, &handle, &count)) {
            continue;
        }
        void *b = get_buffer(datatype, count);
        if (NULL != b && MPI_SUCCESS == MPI_T_cvar_read(handle, b)) {
            ++reads;
        }
        put_buffer(b);
        MPI_T_cvar_handle_free(&handle);
    }
}

static void read_pvars(void)
{
    int num, count, rc, bind, verbosity, var_class, readonly, continuous, atomic;
    int name_len, desc_len;
    MPI_Datatype datatype;
    MPI_T_enum enumtype;
    MPI_T_pvar_session session;
    MPI_T_pvar_handle handle;
    char name[NAME_LEN];

    if (MPI_SUCCESS != MPI_T_pvar_get_num(&num)) {
        return;
    }
    if (MPI_SUCCESS != MPI_T_pvar_session_create(&session)) {
        return;
    }
    for (int i = 0; i < num && !stop; ++i) {
        name_len = NAME_LEN;
        desc_len = 0;
        rc = MPI_T_pvar_get_info(i, name, &name_len, &verbosity, &var_class, &datatype,
                                 &enumtype, NULL, &desc_len, &bind, &readonly, &continuous,
                                 &atomic);
        if (MPI_SUCCESS != rc || MPI_T_BIND_NO_OBJECT != bind) {
            continue;
        }
        if (MPI_SUCCESS != MPI_T_pvar_handle_alloc(session, i, NULL, &handle, &count)) {
            continue;
        }
        void *b = get_buffer(datatype, count);
        if (NULL != b && MPI_SUCCESS == MPI_T_pvar_read(session, handle, b)) {
            ++reads;
        }
        put_buffer(b);
        MPI_T_pvar_handle_free(session, &handle);
    }
    MPI_T_pvar_session_free(&session);
}

static void *reader(void *arg)
{
    (void) arg;
    while (!stop) {
        read_cvars();
        read_pvars();
    }
    return NULL;
}

int main(void)
{
    int provided;
    MPI_Session s;
    pthread_t thread;

    signal(SIGALRM, timed_out);
    alarm(600);

    pin_tcp_btl();

    /* Probe serially first: environments that cannot run MPI_T or a
       singleton session at all are a skip, not a failure. */
    STEP_OR_SKIP(MPI_T_init_thread(MPI_THREAD_MULTIPLE, &provided), "probe: T_init");
    if (MPI_THREAD_MULTIPLE != provided) {
        printf("SKIP: MPI_T does not provide MPI_THREAD_MULTIPLE\n");
        MPI_T_finalize();
        return 77;
    }
    STEP_OR_SKIP(MPI_Session_init(MPI_INFO_NULL, MPI_ERRORS_RETURN, &s), "probe: Session_init");
    STEP(MPI_Session_finalize(&s), "probe: Session_finalize");
    STEP(MPI_T_finalize(), "probe: T_finalize");

    for (int i = 0; i < CYCLES; ++i) {
        printf("cycle %d\n", i);
        fflush(stdout);

        STEP(MPI_T_init_thread(MPI_THREAD_MULTIPLE, &provided), "T_init");
        stop = 0;
        if (0 != pthread_create(&thread, NULL, reader, NULL)) {
            printf("SKIP: could not create a thread\n");
            return 77;
        }

        STEP(MPI_Session_init(MPI_INFO_NULL, MPI_ERRORS_RETURN, &s), "Session_init");
        STEP(MPI_Session_finalize(&s), "Session_finalize");

        stop = 1;
        pthread_join(thread, NULL);
        STEP(MPI_T_finalize(), "T_finalize");
    }

    printf("SUCCESS: %ld variable reads raced %d session lifecycles\n", (long) reads, CYCLES);
    return 0;
}
