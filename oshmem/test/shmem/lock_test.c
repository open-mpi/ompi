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
 * shmem_test_lock semantics: it returns 0 when the lock was acquired and
 * nonzero when it is held elsewhere; a loop of test_lock calls must
 * acquire the lock within a bounded number of attempts and must provide
 * mutual exclusion.
 *
 * NP: 3
 */

#include <stdio.h>
#include <shmem.h>

#define ITERS 200
#define MAX_ATTEMPTS 100000000L

static long lock = 0;
static long lock2 = 0;
static long counter = 0;

int main(void)
{
    int me, npes, i, r, errs = 0;
    long attempts;

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

    /* Uncontended: test_lock acquires and returns 0, then lock is held. */
    if (0 == me) {
        r = shmem_test_lock(&lock2);
        if (0 != r) {
            printf("FAIL: PE %d: uncontended test_lock expected 0, got %d\n", me, r);
            ++errs;
        }
    }
    shmem_barrier_all();

    /* Held by PE 0: everyone else must see nonzero, repeatedly. */
    if (0 != me) {
        for (i = 0; i < 20; ++i) {
            r = shmem_test_lock(&lock2);
            if (0 == r) {
                printf("FAIL: PE %d: test_lock on held lock expected nonzero, got 0\n", me);
                ++errs;
                shmem_clear_lock(&lock2);
                break;
            }
        }
    }
    shmem_barrier_all();

    if (0 == me) {
        shmem_clear_lock(&lock2);
    }
    shmem_barrier_all();

    /* Released: acquisition by test_lock succeeds again, one PE at a time. */
    for (i = 0; i < npes; ++i) {
        if (me == i) {
            r = shmem_test_lock(&lock2);
            if (0 != r) {
                printf("FAIL: PE %d: test_lock on free lock expected 0, got %d\n", me, r);
                ++errs;
            } else {
                shmem_clear_lock(&lock2);
            }
        }
        shmem_barrier_all();
    }

    /* Contended test_lock loops with a bound, protecting a remote counter. */
    for (i = 0; i < ITERS; ++i) {
        attempts = 0;
        while (0 != shmem_test_lock(&lock)) {
            if (++attempts > MAX_ATTEMPTS) {
                printf("FAIL: PE %d: iteration %d: test_lock did not acquire after %ld attempts\n",
                       me, i, attempts);
                ++errs;
                break;
            }
        }
        if (attempts > MAX_ATTEMPTS) {
            break;
        }
        {
            long v = shmem_long_g(&counter, 0);
            shmem_long_p(&counter, v + 1, 0);
        }
        shmem_clear_lock(&lock);
    }
    shmem_barrier_all();

    {
        long got = shmem_long_g(&counter, 0);
        long want = (long) npes * ITERS;
        if (got != want && 0 == errs) {
            printf("FAIL: PE %d: counter expected %ld, got %ld\n", me, want, got);
            ++errs;
        }
    }

    shmem_finalize();
    return errs ? 1 : 0;
}
