/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Concurrent MPI_T and sessions initialization: one thread loops full
 * MPI_T init/finalize cycles while another loops full MPI session
 * init/finalize cycles, with no ordering between them.
 *
 * A first MPI_T initialization and an instance (world or session)
 * initialization share unlocked under-layers: the OPAL init reference
 * counters, the shared event base reference count, MCA variable
 * registration, and every framework's refcount.  MPI_T entry points
 * therefore serialize against instance init/teardown (see
 * ompi_mpi_instance_lock()); this test hammers exactly that window.
 *
 * A race here is probabilistic: a failure (crash or error return) proves
 * the bug, but a pass is only evidence, not proof.  The iteration counts
 * are a compromise between coverage and 'make check' runtime.
 *
 * MPI_T is driven from one thread here, but the process is
 * multithreaded and that thread is not promised to be the main thread,
 * so MPI_THREAD_SERIALIZED is the honest required level for the tools
 * interface (MPI_THREAD_SINGLE would promise a single-threaded
 * process).  A level above SINGLE also engages the thread-flag ratchet
 * in MPI_T_init_thread(), so this test exercises the ratchet and the
 * init serialization together.
 */

#include "mpi_t_lifecycle.h"

#include <pthread.h>
#include <stdatomic.h>

#define T_CYCLES 500
#define SESSION_CYCLES 20

/* Shared between the two threads.  _Atomic (rather than volatile, which
   provides neither atomicity nor ordering in C11) makes every plain read
   and write of this flag a sequentially-consistent atomic access. */
static _Atomic int failed = 0;

static void *t_thread(void *arg)
{
    int provided;

    (void) arg;
    for (int i = 0; i < T_CYCLES && !failed; ++i) {
        if (MPI_SUCCESS != MPI_T_init_thread(MPI_THREAD_SERIALIZED, &provided)) {
            printf("FAIL: MPI_T_init_thread (cycle %d)\n", i);
            failed = 1;
            break;
        }
        if (MPI_SUCCESS != MPI_T_finalize()) {
            printf("FAIL: MPI_T_finalize (cycle %d)\n", i);
            failed = 1;
            break;
        }
    }
    return NULL;
}

static void *session_thread(void *arg)
{
    (void) arg;
    for (int i = 0; i < SESSION_CYCLES && !failed; ++i) {
        MPI_Session s;
        if (MPI_SUCCESS != MPI_Session_init(MPI_INFO_NULL, MPI_ERRORS_RETURN, &s)) {
            printf("FAIL: MPI_Session_init (cycle %d)\n", i);
            failed = 1;
            break;
        }
        if (MPI_SUCCESS != MPI_Session_finalize(&s)) {
            printf("FAIL: MPI_Session_finalize (cycle %d)\n", i);
            failed = 1;
            break;
        }
    }
    return NULL;
}

int main(void)
{
    pthread_t t1, t2;
    int provided;
    MPI_Session probe;

    pin_tcp_btl();

    /* Probe serially first: environments that cannot run MPI_T or a
       singleton session at all are a skip, not a failure. */
    STEP_OR_SKIP(MPI_T_init_thread(MPI_THREAD_SERIALIZED, &provided), "probe: T_init");
    STEP_OR_SKIP(MPI_Session_init(MPI_INFO_NULL, MPI_ERRORS_RETURN, &probe),
                 "probe: Session_init");
    STEP(MPI_Session_finalize(&probe), "probe: Session_finalize");
    STEP(MPI_T_finalize(), "probe: T_finalize");

    printf("running: %d MPI_T cycles vs %d session cycles, concurrently\n",
           T_CYCLES, SESSION_CYCLES);
    fflush(stdout);

    if (0 != pthread_create(&t1, NULL, t_thread, NULL)
        || 0 != pthread_create(&t2, NULL, session_thread, NULL)) {
        printf("SKIP: could not create threads\n");
        return 77;
    }
    pthread_join(t1, NULL);
    pthread_join(t2, NULL);

    if (failed) {
        return 1;
    }

    /* Both lifecycles must still work serially afterwards. */
    STEP(MPI_T_init_thread(MPI_THREAD_SERIALIZED, &provided), "post: T_init");
    STEP(MPI_Session_init(MPI_INFO_NULL, MPI_ERRORS_RETURN, &probe), "post: Session_init");
    STEP(MPI_Session_finalize(&probe), "post: Session_finalize");
    STEP(MPI_T_finalize(), "post: T_finalize");

    printf("SUCCESS: concurrent MPI_T and session lifecycles\n");
    return 0;
}
