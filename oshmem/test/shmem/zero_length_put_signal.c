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
 * A put-with-signal with nelems == 0 transfers no data but must still
 * deliver the signal update.  Only SHMEM_SIGNAL_ADD is used.
 *
 * NP: 4
 */

#include <stdio.h>
#include <stdint.h>
#include <shmem.h>

#define N 8

static long sym[N];
static uint64_t sig;
static int failures = 0;

#define CHECK(cond, ...)                                    \
    do {                                                    \
        if (!(cond)) {                                      \
            printf("FAIL: PE %d: ", me);                    \
            printf(__VA_ARGS__);                            \
            printf("\n");                                   \
            failures++;                                     \
        }                                                   \
    } while (0)

int main(void)
{
    int me, np, i, next, prev;
    long local[N];
    uint64_t expected = 0;
    shmem_ctx_t ctx;
    int have_ctx;

    shmem_init();
    me = shmem_my_pe();
    np = shmem_n_pes();
    if (np < 2) {
        printf("SKIP: need at least 2 PEs\n");
        shmem_finalize();
        return 77;
    }
    next = (me + 1) % np;
    prev = (me + np - 1) % np;

    for (i = 0; i < N; ++i) {
        sym[i] = 1000 * me + i;
        local[i] = -(i + 1);
    }
    sig = 0;
    have_ctx = (0 == shmem_ctx_create(0, &ctx));
    shmem_barrier_all();

    /* Each call adds a distinct power of two; the sum is checked below. */
    shmem_long_put_signal(sym, local, 0, &sig, 1, SHMEM_SIGNAL_ADD, next);
    expected += 1;
    shmem_putmem_signal(sym, local, 0, &sig, 2, SHMEM_SIGNAL_ADD, next);
    expected += 2;
    shmem_put64_signal(sym, local, 0, &sig, 4, SHMEM_SIGNAL_ADD, next);
    expected += 4;
    shmem_long_put_signal_nbi(sym, local, 0, &sig, 8, SHMEM_SIGNAL_ADD, next);
    expected += 8;
    shmem_putmem_signal_nbi(sym, local, 0, &sig, 16, SHMEM_SIGNAL_ADD, next);
    expected += 16;
    /* NULL data pointers */
    shmem_putmem_signal(NULL, NULL, 0, &sig, 32, SHMEM_SIGNAL_ADD, next);
    expected += 32;
    shmem_long_put_signal_nbi(NULL, NULL, 0, &sig, 64, SHMEM_SIGNAL_ADD, next);
    expected += 64;
    shmem_ctx_long_put_signal(SHMEM_CTX_DEFAULT, sym, local, 0, &sig, 128,
                              SHMEM_SIGNAL_ADD, next);
    expected += 128;
    if (have_ctx) {
        shmem_ctx_putmem_signal(ctx, sym, local, 0, &sig, 256,
                                SHMEM_SIGNAL_ADD, next);
        expected += 256;
        /* shmem_ctx_*_put_signal_nbi is not exercised: it hangs at 2 PEs
         * in the UCX SPML for any nelems, independent of this change. */
        shmem_long_put_signal(sym, local, 0, &sig, 512, SHMEM_SIGNAL_ADD, next);
        expected += 512;
        shmem_ctx_quiet(ctx);
    }
    /* and to self */
    shmem_long_put_signal(sym, local, 0, &sig, 1024, SHMEM_SIGNAL_ADD, me);
    expected += 1024;

    shmem_quiet();
    shmem_barrier_all();

    /* Each PE received the series from prev plus its own self entry. */
    {
        uint64_t got = shmem_signal_fetch(&sig);
        CHECK(got == expected, "signal: expected %llu, got %llu",
              (unsigned long long) expected, (unsigned long long) got);
    }
    for (i = 0; i < N; ++i) {
        CHECK(sym[i] == 1000 * me + i,
              "sym[%d] changed by zero-length put_signal: expected %d, got %ld",
              i, 1000 * me + i, sym[i]);
    }

    /* A normal put_signal and a normal put/get afterwards still work. */
    for (i = 0; i < N; ++i) {
        local[i] = 5000 + 100 * me + i;
    }
    shmem_barrier_all();
    shmem_long_put_signal(sym, local, N, &sig, 4096, SHMEM_SIGNAL_ADD, next);
    shmem_quiet();
    shmem_barrier_all();
    CHECK(shmem_signal_fetch(&sig) == expected + 4096,
          "signal after real put_signal: expected %llu, got %llu",
          (unsigned long long) (expected + 4096),
          (unsigned long long) shmem_signal_fetch(&sig));
    for (i = 0; i < N; ++i) {
        CHECK(sym[i] == 5000 + 100 * prev + i,
              "sym[%d] after put_signal: expected %d, got %ld",
              i, 5000 + 100 * prev + i, sym[i]);
    }

    if (have_ctx) {
        shmem_ctx_destroy(ctx);
    }
    shmem_finalize();
    return failures ? 1 : 0;
}
