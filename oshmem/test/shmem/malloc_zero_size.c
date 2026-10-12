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
 * no collective operation when the size is zero.
 *
 * NP: 4
 */

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

/* Allocate a block, check that it is symmetric and usable, then free it. */
static void check_normal(int round)
{
    static long addr[1];
    long *buf;
    int i, npes = shmem_n_pes();
    size_t n = 16 + round;

    buf = shmem_malloc(n * sizeof(long));
    if (NULL == buf) {
        printf("FAIL: PE %d: round %d: shmem_malloc(%lu) returned NULL\n",
               me, round, (unsigned long) (n * sizeof(long)));
        failures++;
        /* every PE sees the same result, so this is safe */
        return;
    }
    for (i = 0; i < (int) n; i++) {
        buf[i] = me * 1000 + i;
    }
    /* addr is static (symmetric): publish my address to PE 0's copy */
    addr[0] = (long) buf;
    shmem_barrier_all();
    for (i = 0; i < npes; i++) {
        long other = shmem_g(&addr[0], i);
        if (other != (long) buf) {
            printf("FAIL: PE %d: round %d: address on PE %d is %p, "
                   "local is %p\n", me, round, i, (void *) other,
                   (void *) buf);
            failures++;
        }
    }
    /* data check: read a neighbor's contents */
    {
        int peer = (me + 1) % npes;
        long got = shmem_g(&buf[n - 1], peer);
        long want = peer * 1000 + (long) (n - 1);
        if (got != want) {
            printf("FAIL: PE %d: round %d: data from PE %d expected %ld, "
                   "got %ld\n", me, round, peer, want, got);
            failures++;
        }
    }
    shmem_barrier_all();
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
