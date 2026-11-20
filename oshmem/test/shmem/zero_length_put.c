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
 * A put with nelems == 0 is a no-op: it must not touch the destination,
 * must not abort when the pointers are NULL or not symmetric, and must not
 * disturb later communication.
 *
 * NP: 4
 */

#include <stdio.h>
#include <shmem.h>

#define N 16

static long sym[N];
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

/* All put flavors with nelems == 0 (and nelems == 0 for the iput forms). */
#define ZERO_PUTS(D, S, PE)                                             \
    do {                                                                \
        shmem_putmem((void *) (D), (void *) (S), 0, (PE));                                \
        shmem_put8((void *) (D), (void *) (S), 0, (PE));                                  \
        shmem_put16((void *) (D), (void *) (S), 0, (PE));                                 \
        shmem_put32((void *) (D), (void *) (S), 0, (PE));                                 \
        shmem_put64((void *) (D), (void *) (S), 0, (PE));                                 \
        shmem_put128((void *) (D), (void *) (S), 0, (PE));                                \
        shmem_int_put((void *) (D), (void *) (S), 0, (PE));                               \
        shmem_long_put((void *) (D), (void *) (S), 0, (PE));                              \
        shmem_double_put((void *) (D), (void *) (S), 0, (PE));                            \
        shmem_putmem_nbi((void *) (D), (void *) (S), 0, (PE));                            \
        shmem_put8_nbi((void *) (D), (void *) (S), 0, (PE));                              \
        shmem_put16_nbi((void *) (D), (void *) (S), 0, (PE));                             \
        shmem_put32_nbi((void *) (D), (void *) (S), 0, (PE));                             \
        shmem_put64_nbi((void *) (D), (void *) (S), 0, (PE));                             \
        shmem_put128_nbi((void *) (D), (void *) (S), 0, (PE));                            \
        shmem_int_put_nbi((void *) (D), (void *) (S), 0, (PE));                           \
        shmem_long_put_nbi((void *) (D), (void *) (S), 0, (PE));                          \
        shmem_double_put_nbi((void *) (D), (void *) (S), 0, (PE));                        \
        shmem_iput32((void *) (D), (void *) (S), 1, 1, 0, (PE));                          \
        shmem_iput64((void *) (D), (void *) (S), 2, 3, 0, (PE));                          \
        shmem_iput128((void *) (D), (void *) (S), 1, 1, 0, (PE));                         \
        shmem_int_iput((void *) (D), (void *) (S), 1, 1, 0, (PE));                        \
        shmem_long_iput((void *) (D), (void *) (S), 2, 2, 0, (PE));                       \
        shmem_double_iput((void *) (D), (void *) (S), 1, 1, 0, (PE));                     \
    } while (0)

#define ZERO_CTX_PUTS(C, D, S, PE)                                      \
    do {                                                                \
        shmem_ctx_putmem((C), (void *) (D), (void *) (S), 0, (PE));                       \
        shmem_ctx_put32((C), (void *) (D), (void *) (S), 0, (PE));                        \
        shmem_ctx_put64((C), (void *) (D), (void *) (S), 0, (PE));                        \
        shmem_ctx_put128((C), (void *) (D), (void *) (S), 0, (PE));                       \
        shmem_ctx_int_put((C), (void *) (D), (void *) (S), 0, (PE));                      \
        shmem_ctx_long_put((C), (void *) (D), (void *) (S), 0, (PE));                     \
        shmem_ctx_putmem_nbi((C), (void *) (D), (void *) (S), 0, (PE));                   \
        shmem_ctx_put64_nbi((C), (void *) (D), (void *) (S), 0, (PE));                    \
        shmem_ctx_int_put_nbi((C), (void *) (D), (void *) (S), 0, (PE));                  \
        shmem_ctx_double_put_nbi((C), (void *) (D), (void *) (S), 0, (PE));               \
        shmem_ctx_iput64((C), (void *) (D), (void *) (S), 1, 1, 0, (PE));                 \
        shmem_ctx_int_iput((C), (void *) (D), (void *) (S), 2, 2, 0, (PE));               \
    } while (0)

int main(void)
{
    int me, np, i, k, target, self_pass;
    long local[N];
    long nonsym[N];
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

    for (i = 0; i < N; ++i) {
        sym[i] = 1000 * me + i;
        local[i] = -(i + 1);
        nonsym[i] = -1000 - i;
    }
    have_ctx = (0 == shmem_ctx_create(0, &ctx));
    shmem_barrier_all();

    /* Pass 0 targets this PE, pass 1 a remote PE. */
    for (self_pass = 0; self_pass < 2; ++self_pass) {
        target = self_pass ? (me + 1) % np : me;

        /* symmetric destination, valid and NULL sources */
        ZERO_PUTS(sym, local, target);
        ZERO_PUTS(sym, NULL, target);
        /* NULL and non-symmetric destinations */
        ZERO_PUTS(NULL, local, target);
        ZERO_PUTS(NULL, NULL, target);
        ZERO_PUTS(nonsym, local, target);
        ZERO_PUTS(nonsym, NULL, target);

        ZERO_CTX_PUTS(SHMEM_CTX_DEFAULT, sym, local, target);
        ZERO_CTX_PUTS(SHMEM_CTX_DEFAULT, NULL, NULL, target);
        ZERO_CTX_PUTS(SHMEM_CTX_DEFAULT, nonsym, local, target);
        if (have_ctx) {
            ZERO_CTX_PUTS(ctx, sym, local, target);
            ZERO_CTX_PUTS(ctx, NULL, NULL, target);
            ZERO_CTX_PUTS(ctx, nonsym, local, target);
            shmem_ctx_quiet(ctx);
        }
        shmem_fence();
        shmem_quiet();
    }

    shmem_barrier_all();

    for (i = 0; i < N; ++i) {
        CHECK(sym[i] == 1000 * me + i,
              "sym[%d] changed by zero-length put: expected %ld, got %ld",
              i, (long) (1000 * me + i), sym[i]);
        CHECK(nonsym[i] == -1000 - i,
              "nonsym[%d] changed: expected %d, got %ld", i, -1000 - i, nonsym[i]);
        CHECK(local[i] == -(i + 1),
              "local[%d] changed: expected %d, got %ld", i, -(i + 1), local[i]);
    }

    shmem_barrier_all();

    /* Normal communication still works afterwards: put ring, then check. */
    for (k = 0; k < 2; ++k) {
        for (i = 0; i < N; ++i) {
            local[i] = 7000 + 100 * me + i;
        }
        shmem_fence();
        if (0 == k) {
            shmem_long_put(sym, local, N, (me + 1) % np);
        } else {
            shmem_long_put_nbi(sym, local, N, (me + 1) % np);
        }
        shmem_quiet();
        shmem_barrier_all();
        {
            int prev = (me + np - 1) % np;
            for (i = 0; i < N; ++i) {
                CHECK(sym[i] == 7000 + 100 * prev + i,
                      "round %d sym[%d]: expected %d, got %ld",
                      k, i, 7000 + 100 * prev + i, sym[i]);
            }
        }
        /* a zero-length put in between must still change nothing */
        shmem_long_put(sym, local, 0, (me + 1) % np);
        shmem_quiet();
        shmem_barrier_all();
    }

    if (have_ctx) {
        shmem_ctx_destroy(ctx);
    }
    shmem_finalize();
    return failures ? 1 : 0;
}
