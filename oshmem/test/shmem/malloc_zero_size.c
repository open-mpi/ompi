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
 * Zero-size allocation: shmem_malloc, shmem_calloc, shmem_align,
 * shmem_realloc and shmem_malloc_with_hints must return NULL and perform
 * no collective operation when the size is zero.  shmem_calloc must also
 * return NULL when count * size overflows.
 *
 * NP: 4
 */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <shmem.h>

static int failures = 0;
static int me = -1;

static void expect_null(const char *what, void *p)
{
    if (NULL != p) {
        printf("FAIL: PE %d: %s: expected NULL, got %p\n", me, what, p);
        failures++;
    }
}

/*
 * Allocate a block, check that it is symmetric and usable, then free it.
 * OpenSHMEM guarantees symmetric offsets within the heap, not equal virtual
 * addresses, so symmetry is checked by using the local address of an
 * object to reach its remote counterpart and by comparing the distance
 * between two allocations, not by comparing pointers across PEs.
 */
static void check_normal(int round)
{
    static long delta[1];
    long *buf, *buf2;
    int i, npes = shmem_n_pes();
    int next = (me + 1) % npes;
    int prev = (me + npes - 1) % npes;
    size_t n = 16 + round;

    buf = shmem_malloc((n + 1) * sizeof(long));
    if (NULL == buf) {
        printf("FAIL: PE %d: round %d: shmem_malloc(%lu) returned NULL\n",
               me, round, (unsigned long) ((n + 1) * sizeof(long)));
        failures++;
        /* every PE sees the same result, so this is safe */
        return;
    }
    for (i = 0; i < (int) n; i++) {
        buf[i] = me * 1000 + i;
    }
    buf[n] = -1;
    shmem_barrier_all();

    /* write through my local address into my successor's copy */
    shmem_long_p(&buf[n], me, next);
    shmem_barrier_all();
    if (buf[n] != prev) {
        printf("FAIL: PE %d: round %d: PE %d wrote %ld through its local "
               "address, expected %d\n", me, round, prev, buf[n], prev);
        failures++;
    }

    /* a second block must sit at the same distance from the first on all PEs */
    buf2 = shmem_malloc(8 * sizeof(long));
    delta[0] = (NULL == buf2) ? 0 : (long) ((intptr_t) buf2 - (intptr_t) buf);
    shmem_barrier_all();
    if (NULL == buf2) {
        printf("FAIL: PE %d: round %d: second shmem_malloc returned NULL\n",
               me, round);
        failures++;
    } else {
        for (i = 0; i < npes; i++) {
            long other = shmem_g(&delta[0], i);
            if (other != delta[0]) {
                printf("FAIL: PE %d: round %d: allocation distance on PE %d "
                       "is %ld, local is %ld\n", me, round, i, other,
                       delta[0]);
                failures++;
            }
        }
    }

    /* data check: read a neighbor's contents */
    {
        int peer = next;
        long got = shmem_g(&buf[n - 1], peer);
        long want = peer * 1000 + (long) (n - 1);
        if (got != want) {
            printf("FAIL: PE %d: round %d: data from PE %d expected %ld, "
                   "got %ld\n", me, round, peer, want, got);
            failures++;
        }
    }
    shmem_barrier_all();
    shmem_free(buf2);
    shmem_free(buf);
}

int main(void)
{
    int i, round, npes;
    void *p, *q;

    shmem_init();
    me = shmem_my_pe();
    npes = shmem_n_pes();

    if (npes < 2) {
        if (0 == me) {
            printf("SKIP: needs at least 2 PEs\n");
        }
        shmem_finalize();
        return 77;
    }

    check_normal(0);

    for (round = 1; round <= 5; round++) {
        expect_null("shmem_malloc(0)", shmem_malloc(0));
        expect_null("shmem_calloc(0, 8)", shmem_calloc(0, 8));
        expect_null("shmem_calloc(8, 0)", shmem_calloc(8, 0));
        expect_null("shmem_calloc(0, 0)", shmem_calloc(0, 0));
        expect_null("shmem_align(64, 0)", shmem_align(64, 0));
        expect_null("shmem_realloc(NULL, 0)", shmem_realloc(NULL, 0));
        expect_null("shmem_malloc_with_hints(0, 0)",
                    shmem_malloc_with_hints(0, 0));
        expect_null("shmem_malloc_with_hints(0, ATOMICS_REMOTE)",
                    shmem_malloc_with_hints(0, SHMEM_MALLOC_ATOMICS_REMOTE));
        check_normal(round);
    }

    /* A count * size product that does not fit in a size_t cannot be
     * allocated: no memory, no barrier, and a NULL return.  The last case
     * wraps around to a small non-zero product (8 bytes). */
    expect_null("shmem_calloc(SIZE_MAX, 2)", shmem_calloc(SIZE_MAX, 2));
    expect_null("shmem_calloc(SIZE_MAX / 2 + 1, 2)",
                shmem_calloc(SIZE_MAX / 2 + 1, 2));
    expect_null("shmem_calloc(SIZE_MAX / 8 + 2, 8)",
                shmem_calloc(SIZE_MAX / 8 + 2, 8));
    check_normal(5);

    /* shmem_realloc(NULL, n) acts like shmem_malloc(n) */
    p = shmem_realloc(NULL, 128);
    if (NULL == p) {
        printf("FAIL: PE %d: shmem_realloc(NULL, 128) returned NULL\n", me);
        failures++;
    } else {
        long *l = (long *) p;
        for (i = 0; i < 16; i++) {
            l[i] = i;
        }
        shmem_barrier_all();
        /* shmem_realloc(ptr, 0) frees ptr and returns NULL */
        q = shmem_realloc(p, 0);
        expect_null("shmem_realloc(ptr, 0)", q);
    }
    check_normal(6);

    /* A pointer outside the symmetric heap is not a valid argument; it must
     * be rejected with a NULL return, not abort and not free it. */
    {
        int stack_var = 0;
        void *priv = malloc(64);

        expect_null("shmem_realloc(stack ptr, 0)",
                    shmem_realloc(&stack_var, 0));
        expect_null("shmem_realloc(private ptr, 0)", shmem_realloc(priv, 0));
        free(priv);
    }
    check_normal(6);

    /*
     * No collective side effects: only some PEs make zero-size calls, which
     * must not consume a barrier.  If they did, the PEs would be out of step
     * here and the following allocation would hang or return mismatched
     * addresses.  Only attempted if everything so far is correct, so that a
     * broken implementation fails rather than hangs.
     */
    if (0 == failures) {
        if (0 == me % 2) {
            for (i = 0; i < 3; i++) {
                expect_null("asymmetric shmem_malloc(0)", shmem_malloc(0));
                expect_null("asymmetric shmem_align(16, 0)",
                            shmem_align(16, 0));
                expect_null("asymmetric shmem_malloc_with_hints(0, 0)",
                            shmem_malloc_with_hints(0, 0));
            }
        }
        check_normal(7);
        check_normal(8);
    }

    shmem_barrier_all();
    if (0 == failures) {
        if (0 == me) {
            printf("PASS: malloc_zero_size\n");
        }
    }
    shmem_finalize();
    return (0 == failures) ? 0 : 1;
}
