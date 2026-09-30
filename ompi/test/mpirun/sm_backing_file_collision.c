/*
 * Copyright (c) 2026      Amazon.com, Inc. or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 *
 * Regression test: two jobs running at the same time on one node whose
 * OPAL jobids collide must not share btl/sm backing files.
 *
 * opal_pmix_convert_nspace() hashes a job's PMIx namespace (for a job
 * started by mpirun, "prterun-<nodename>-<prterun pid>@1") down to a
 * 16-bit job family, the upper half of the jobid.  Two concurrent jobs on
 * one node can therefore get the same jobid, and btl/sm used to name its
 * backing file from the jobid alone: the second job to initialize opened
 * the first job's segment and reinitialized it, and both jobs crashed or
 * hung.
 *
 * Collisions are rare (about N^2 / 2^17 for N concurrent jobs), so the
 * test forces one.  PRRTE builds the namespace from basename(argv[0]), the
 * node name and the prterun PID.  The driver (sm_backing_file_collision.sh)
 * points OMPI_PRTERUN at this program with SM_COLLISION_SHIM set, and
 * mpirun execs it in place of prterun.  It then execs the real prterun
 * (keeping its PID) under an argv[0] basename chosen so that the second
 * job's namespace hashes to the first job's family.
 *
 * Usage:
 *   sm_backing_file_collision [SECONDS]
 *       Run under mpirun: exchange messages around a ring over MPI_Sendrecv
 *       for SECONDS (default 5), 8 bytes to 128 KiB.  Every word is stamped
 *       with a per-job token, the sender and the iteration, so data from
 *       another job is reported as corruption.  Rank 0 prints
 *       "namespace: <PMIx namespace>" before MPI_Init and "ready" after it.
 *       If SM_COLLISION_GO names a file, the timed run starts only once
 *       that file exists, waiting at most SM_COLLISION_GO_TIMEOUT seconds
 *       (default 60).  Every rank
 *       prints "rank <r> finalized" after MPI_Finalize returns.
 *   sm_backing_file_collision --family NAMESPACE
 *       Print the job family Open MPI derives from NAMESPACE.
 *   SM_COLLISION_SHIM=1 sm_backing_file_collision PRTERUN_ARGS...
 *       As OMPI_PRTERUN: exec $SM_COLLISION_PRTERUN with PRTERUN_ARGS.
 *       With SM_COLLISION_FAMILY and SM_COLLISION_NODENAME set, pick the
 *       argv[0] basename that puts this PID on that family.
 */

#include <mpi.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

enum { MAX_WORDS = 1 << 14 };

/* OPAL_HASH_STR (opal/include/opal/hash_string.h), Jenkins one-at-a-time. */
static uint32_t hash_string(const char *str, size_t len)
{
    uint32_t hash = 0;
    for (size_t i = 0; i < len; i++) {
        hash += (unsigned char) str[i];
        hash += hash << 10;
        hash ^= hash >> 6;
    }
    hash += hash << 3;
    hash ^= hash >> 11;
    return hash + (hash << 15);
}

/* The job family opal_pmix_convert_nspace() derives: the hash of the
 * namespace up to its final '@', folded to 16 bits. */
static unsigned job_family(const char *nspace)
{
    const char *at = strrchr(nspace, '@');
    const uint32_t hash = hash_string(nspace, NULL != at ? (size_t) (at - nspace) : strlen(nspace));
    return (unsigned) (((hash >> 16) & 0xffff) ^ (hash & 0xffff));
}

static int prterun_shim(char **argv)
{
    const char *real = getenv("SM_COLLISION_PRTERUN");
    const char *family = getenv("SM_COLLISION_FAMILY");
    const char *node = getenv("SM_COLLISION_NODENAME");
    if (NULL == real) {
        fprintf(stderr, "sm_backing_file_collision: SM_COLLISION_PRTERUN is not set\n");
        return 1;
    }
    static char name[32] = "prterun";
    if (NULL != family && '\0' != family[0] && NULL != node) {
        const unsigned target = (unsigned) strtoul(family, NULL, 0);
        char nspace[512];
        unsigned long i = 1;
        /* A 16-bit target is hit within ~65536 candidates on average. */
        for (; i < 100000000UL; i++) {
            snprintf(name, sizeof(name), "prterun%lu", i);
            snprintf(nspace, sizeof(nspace), "%s-%s-%u@1", name, node, (unsigned) getpid());
            if (job_family(nspace) == target) {
                break;
            }
        }
        if (100000000UL == i) {
            fprintf(stderr, "sm_backing_file_collision: no basename reaches family %s\n", family);
            return 1;
        }
    }
    /* Keep these out of prterun's environment, which the ranks inherit. */
    unsetenv("SM_COLLISION_SHIM");
    unsetenv("SM_COLLISION_FAMILY");

    /* mpirun execs OMPI_PRTERUN with argv[0] "prterun" and then the prterun
     * arguments.  When mpirun was run by absolute path, argv[0] is that path
     * instead, which prterun treats as an implied --prefix; replacing it
     * with a bare basename drops that, which does not matter on one node. */
    argv[0] = name;
    execv(real, argv);
    perror(real);
    return 1;
}

/* Wait until the file PATH exists, for at most TIMEOUT seconds. */
static int wait_for_go(const char *path, double timeout)
{
    const double deadline = MPI_Wtime() + timeout;
    const struct timespec tick = {0, 10 * 1000 * 1000};
    while (0 != access(path, F_OK)) {
        if (MPI_Wtime() > deadline) {
            return 0;
        }
        nanosleep(&tick, NULL);
    }
    return 1;
}

static uint64_t stamp(uint64_t token, int writer, long long iter, int word)
{
    return token ^ ((uint64_t) writer << 48) ^ ((uint64_t) iter << 16) ^ (uint64_t) word;
}

int main(int argc, char **argv)
{
    const char *shim = getenv("SM_COLLISION_SHIM");
    if (NULL != shim && '\0' != shim[0]) {
        return prterun_shim(argv);
    }
    if (argc > 2 && 0 == strcmp(argv[1], "--family")) {
        printf("0x%04x\n", job_family(argv[2]));
        return 0;
    }
    const double seconds = argc > 1 ? atof(argv[1]) : 5.0;

    /* The namespace comes from the launcher, so report it even if MPI_Init
     * fails; btl/sm opens its backing file during MPI_Init. */
    const char *nspace = getenv("PMIX_NAMESPACE");
    const char *rank_env = getenv("PMIX_RANK");
    if (NULL != rank_env && 0 == strcmp(rank_env, "0")) {
        printf("namespace: %s\n", NULL != nspace ? nspace : "(unknown)");
        fflush(stdout);
    }

    MPI_Init(&argc, &argv);
    int rank, size;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    if (0 == rank) {
        printf("ready\n");
        fflush(stdout);
    }

    /* Hold the timed run until the driver has started the other job too. */
    const char *go_file = getenv("SM_COLLISION_GO");
    int go_ok = 1;
    if (0 == rank && NULL != go_file && '\0' != go_file[0]) {
        const char *timeout_env = getenv("SM_COLLISION_GO_TIMEOUT");
        const double timeout = NULL != timeout_env ? atof(timeout_env) : 60.0;
        go_ok = wait_for_go(go_file, timeout);
        if (!go_ok) {
            fprintf(stderr, "sm_backing_file_collision: no go signal within %g s\n", timeout);
        }
    }
    MPI_Bcast(&go_ok, 1, MPI_INT, 0, MPI_COMM_WORLD);
    if (!go_ok) {
        MPI_Finalize();
        return 3;
    }

    uint64_t token = (uint64_t) getpid();
    MPI_Bcast(&token, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD);

    const int right = (rank + 1) % size, left = (rank + size - 1) % size;
    uint64_t *sbuf = malloc(MAX_WORDS * sizeof(uint64_t));
    uint64_t *rbuf = malloc(MAX_WORDS * sizeof(uint64_t));
    long long iters = 0, bad = 0;
    int go = 1;
    const double start = MPI_Wtime();
    while (go) {
        const int n = 1 + (int) (((unsigned long long) iters * 2654435761ULL) % MAX_WORDS);
        for (int i = 0; i < n; i++) {
            sbuf[i] = stamp(token, rank, iters, i);
        }
        MPI_Sendrecv(sbuf, n, MPI_UINT64_T, right, 0, rbuf, n, MPI_UINT64_T, left, 0,
                     MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        for (int i = 0; i < n; i++) {
            if (rbuf[i] != stamp(token, left, iters, i)) {
                fprintf(stderr,
                        "rank %d: message %lld from rank %d is corrupted at word %d "
                        "(another job's data?)\n",
                        rank, iters, left, i);
                bad++;
                break;
            }
        }
        iters++;
        /* Rank 0 decides when to stop, so all ranks run the same iterations. */
        if (0 == iters % 64) {
            go = (0 == rank) ? (MPI_Wtime() - start < seconds) : 0;
            MPI_Bcast(&go, 1, MPI_INT, 0, MPI_COMM_WORLD);
        }
    }

    long long total_bad = 0;
    MPI_Allreduce(&bad, &total_bad, 1, MPI_LONG_LONG, MPI_SUM, MPI_COMM_WORLD);
    if (0 == rank) {
        printf("p2p: %lld iterations, %lld corrupted\n", iters, total_bad);
    }
    free(sbuf);
    free(rbuf);
    MPI_Finalize();
    /* Tells a hang in MPI_Finalize from one in the launcher's teardown. */
    printf("rank %d finalized\n", rank);
    fflush(stdout);
    return 0 == total_bad ? 0 : 1;
}
