/*
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Mutual exclusion with shmem_set_lock/shmem_clear_lock on a PE count that
 * is not a power of two: every PE performs a non-atomic read-modify-write of
 * a counter on PE 0 under the lock.  PEs must agree on which PE owns the
 * lock's state, which fails for odd PE counts if the owner is derived from
 * a per-process address.
 *
 * NP: 3
 */

#include <stdio.h>
#include <shmem.h>

#define ITERS 300

static long lock = 0;
static long counter = 0;

int main(void)
{
    int me, npes, i, errs = 0;
    long got, want;

    shmem_init();
    me = shmem_my_pe();
    npes = shmem_n_pes();

    if (npes < 2) {
        if (0 == me) {
            printf("SKIP: need 2 PEs, have %d\n", npes);
        }
        shmem_finalize();
        return 77;
    }

    for (i = 0; i < ITERS; ++i) {
        long v;
        shmem_set_lock(&lock);
        v = shmem_long_g(&counter, 0);
        shmem_long_p(&counter, v + 1, 0);
        shmem_clear_lock(&lock);
    }
    shmem_barrier_all();

    got = shmem_long_g(&counter, 0);
    want = (long) npes * ITERS;
    if (got != want) {
        printf("FAIL: PE %d: counter expected %ld, got %ld\n", me, want, got);
        ++errs;
    }

    shmem_finalize();
    return errs ? 1 : 0;
}
