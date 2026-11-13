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
 * Mutual exclusion with shmem_set_lock/shmem_clear_lock: every active PE
 * performs a non-atomic read-modify-write of a counter on PE 0 under the
 * lock.  The phases use 2, 3, 4 and 8 contending PEs.
 *
 * NP: 8
 */

#include <stdio.h>
#include <shmem.h>

#define ITERS 300

static long lock = 0;
static long counter = 0;

int main(void)
{
    static const int active[] = { 2, 3, 4, 8 };
    int me, npes, i, p, k, errs = 0;

    shmem_init();
    me = shmem_my_pe();
    npes = shmem_n_pes();

    if (npes < 8) {
        if (0 == me) {
            printf("SKIP: need 8 PEs, have %d\n", npes);
        }
        shmem_finalize();
        return 77;
    }

    for (p = 0; p < 4; ++p) {
        k = active[p];
        if (0 == me) {
            counter = 0;
        }
        shmem_barrier_all();

        if (me < k) {
            for (i = 0; i < ITERS; ++i) {
                long v;
                shmem_set_lock(&lock);
                v = shmem_long_g(&counter, 0);
                shmem_long_p(&counter, v + 1, 0);
                shmem_clear_lock(&lock);
            }
        }
        shmem_barrier_all();

        {
            long got = shmem_long_g(&counter, 0);
            long want = (long) k * ITERS;
            if (got != want) {
                printf("FAIL: PE %d: %d contenders: counter expected %ld, got %ld\n",
                       me, k, want, got);
                ++errs;
            }
        }
        shmem_barrier_all();
    }

    shmem_finalize();
    return errs ? 1 : 0;
}
