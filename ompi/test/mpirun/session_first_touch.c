/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Several threads reaching for the same untouched peer at once, on a
 * fresh instance each time.
 *
 * Lazy wire-up serializes concurrent first contact in places nothing in
 * the tree exercises: the double check under mca_bml_lock in
 * mca_bml_base_endpoint_create(), ompi_proc_arch_lock around arch
 * seeding, the one-atomic claim that makes it one modex fetch per peer
 * however many threads ask, modex_pending under mca_pml_ob1.lock, the
 * signed progress count.  All of them need threads arriving at one cold
 * peer together, so the threads below line up on a barrier first.
 *
 * A peer is only cold once, which is what the session loop is for.
 * Sessions and no MPI_Init, so finalizing the last instance really is
 * the last one: that runs del_procs and drops the modex flags, and the
 * next iteration starts cold again.  It also puts a contract nobody
 * tests under the same load -- if teardown leaves a flag behind,
 * iteration two reads iteration one's addresses.
 *
 * Run it with pmix_base_collect_data=0, where a peer's connection info
 * is local only because somebody asked and only the asking process's
 * progress finishes the fetch.  Note that asking for threads is itself
 * enough to make the tcp btl want every proc at once on a multi-homed
 * node, which collects the exchange after all; where that happens only
 * the shared memory run is on-demand.  A regression here is as likely
 * to hang as to fail, so launch with --timeout.
 */

#include <mpi.h>

#include <pthread.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Fresh instances, hence fresh first touches.  Instance bring-up is not
   free, so this is the smallest count that gives the race more than one
   chance; argv[1] raises it for a real hunt. */
#define ITERATIONS 4
#define THREADS    4

#define TAG_SAME     601
#define TAG_DISTINCT 701

/* pthread_barrier_t is not on macOS. */
struct gate {
    pthread_mutex_t lock;
    pthread_cond_t cond;
    int waiting;
    int generation;
    int total;
};

struct toucher {
    pthread_t id;
    struct gate *gate;
    MPI_Comm comm;
    int rank;
    int size;
    int thread;
    int iteration;
    bool same_peer;
    bool failed;
};

static void gate_init(struct gate *gate, int total)
{
    pthread_mutex_init(&gate->lock, NULL);
    pthread_cond_init(&gate->cond, NULL);
    gate->waiting = 0;
    gate->generation = 0;
    gate->total = total;
}

static void gate_destroy(struct gate *gate)
{
    pthread_mutex_destroy(&gate->lock);
    pthread_cond_destroy(&gate->cond);
}

static void gate_wait(struct gate *gate)
{
    pthread_mutex_lock(&gate->lock);
    int generation = gate->generation;

    if (++gate->waiting == gate->total) {
        gate->waiting = 0;
        gate->generation++;
        pthread_cond_broadcast(&gate->cond);
    } else {
        while (generation == gate->generation) {
            pthread_cond_wait(&gate->cond, &gate->lock);
        }
    }
    pthread_mutex_unlock(&gate->lock);
}

static int payload(int rank, int thread, int iteration)
{
    return (rank + 1) * 100000 + (thread + 1) * 100 + iteration;
}

static void *touch_peer(void *arg)
{
    struct toucher *me = (struct toucher *) arg;
    /* Every thread onto one peer, or spread over all of them.  Both
       want the same partner on the far side, so the distance is what
       varies and the direction is symmetric. */
    int distance = me->same_peer ? 1 : 1 + (me->thread % (me->size - 1));
    int dst = (me->rank + distance) % me->size;
    int src = (me->rank - distance + me->size) % me->size;
    int tag = (me->same_peer ? TAG_SAME : TAG_DISTINCT) + me->thread;
    int outgoing = payload(me->rank, me->thread, me->iteration);
    int incoming = -1;

    /* The point of the test: without this the sends trickle out and the
       first one warms the peer for everybody behind it. */
    gate_wait(me->gate);

    MPI_Sendrecv(&outgoing, 1, MPI_INT, dst, tag, &incoming, 1, MPI_INT, src, tag, me->comm,
                 MPI_STATUS_IGNORE);

    if (payload(src, me->thread, me->iteration) != incoming) {
        fprintf(stderr, "ERROR: rank %d thread %d: expected %d from rank %d, got %d\n", me->rank,
                me->thread, payload(src, me->thread, me->iteration), src, incoming);
        me->failed = true;
    }
    return NULL;
}

static void run_phase(MPI_Comm comm, int rank, int size, int iteration, bool same_peer)
{
    struct toucher toucher[THREADS];
    struct gate gate;

    gate_init(&gate, THREADS);

    for (int i = 0; i < THREADS; ++i) {
        toucher[i] = (struct toucher){.gate = &gate,
                                      .comm = comm,
                                      .rank = rank,
                                      .size = size,
                                      .thread = i,
                                      .iteration = iteration,
                                      .same_peer = same_peer,
                                      .failed = false};
        if (0 != pthread_create(&toucher[i].id, NULL, touch_peer, &toucher[i])) {
            fprintf(stderr, "ERROR: rank %d: cannot create thread %d\n", rank, i);
            MPI_Abort(comm, 1);
        }
    }

    for (int i = 0; i < THREADS; ++i) {
        pthread_join(toucher[i].id, NULL);
        if (toucher[i].failed) {
            MPI_Abort(comm, 1);
        }
    }
    gate_destroy(&gate);
}

static bool granted_multiple(MPI_Session session)
{
    MPI_Info info = MPI_INFO_NULL;
    char value[MPI_MAX_INFO_VAL];
    int length = (int) sizeof(value);
    int flag = 0;

    MPI_Session_get_info(session, &info);
    MPI_Info_get_string(info, "thread_level", &length, value, &flag);
    MPI_Info_free(&info);

    return flag && (0 == strcmp(value, "MPI_THREAD_MULTIPLE"));
}

static MPI_Session open_session(int iteration)
{
    MPI_Session session = MPI_SESSION_NULL;
    MPI_Info info = MPI_INFO_NULL;

    MPI_Info_create(&info);
    MPI_Info_set(info, "thread_level", "MPI_THREAD_MULTIPLE");

    int rc = MPI_Session_init(info, MPI_ERRORS_RETURN, &session);
    MPI_Info_free(&info);

    if (MPI_SUCCESS != rc) {
        char message[MPI_MAX_ERROR_STRING] = "";
        int length = 0;

        MPI_Error_string(rc, message, &length);
        fprintf(stderr, "ERROR: MPI_Session_init failed on instance %d: %s\n", iteration, message);
        return MPI_SESSION_NULL;
    }

    return session;
}

int main(int argc, char *argv[])
{
    int iterations = (argc > 1) ? atoi(argv[1]) : ITERATIONS;
    int rank = -1;

    for (int iteration = 0; iteration < iterations; ++iteration) {
        MPI_Session session = open_session(iteration);
        MPI_Group group = MPI_GROUP_NULL;
        MPI_Comm comm = MPI_COMM_NULL;
        char stringtag[64];
        int size = -1;

        if (MPI_SESSION_NULL == session) {
            return 1;
        }
        if (0 == iteration && !granted_multiple(session)) {
            printf("SKIP: this build does not grant MPI_THREAD_MULTIPLE\n");
            fflush(stdout);
            MPI_Session_finalize(&session);
            return 0;
        }

        MPI_Group_from_session_pset(session, "mpi://WORLD", &group);
        /* Unique per call, though these are strictly sequential. */
        snprintf(stringtag, sizeof(stringtag), "ompi-session-first-touch-%d", iteration);
        MPI_Comm_create_from_group(group, stringtag, MPI_INFO_NULL, MPI_ERRORS_ARE_FATAL, &comm);
        MPI_Group_free(&group);

        MPI_Comm_rank(comm, &rank);
        MPI_Comm_size(comm, &size);
        if (size < 2) {
            fprintf(stderr, "ERROR: this test requires at least 2 ranks\n");
            MPI_Abort(comm, 1);
        }

        run_phase(comm, rank, size, iteration, true);
        if (size > 2) {
            run_phase(comm, rank, size, iteration, false);
        }

        MPI_Comm_free(&comm);
        MPI_Session_finalize(&session);
    }

    if (0 == rank) {
        printf("session first touch: PASSED (%d instances, %d threads)\n", iterations, THREADS);
        fflush(stdout);
    }

    return 0;
}
