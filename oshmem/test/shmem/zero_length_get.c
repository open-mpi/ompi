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
 * A get with nelems == 0 is a no-op: it must not touch the local
 * destination, must not abort when the pointers are NULL or not symmetric,
 * and must not disturb later communication.
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

/* D is the local destination, S the (nominally symmetric) source. */
#define ZERO_GETS(D, S, PE)                                             \
    do {                                                                \
        shmem_getmem((void *) (D), (void *) (S), 0, (PE));                                \
        shmem_get8((void *) (D), (void *) (S), 0, (PE));                                  \
        shmem_get16((void *) (D), (void *) (S), 0, (PE));                                 \
        shmem_get32((void *) (D), (void *) (S), 0, (PE));                                 \
        shmem_get64((void *) (D), (void *) (S), 0, (PE));                                 \
        shmem_get128((void *) (D), (void *) (S), 0, (PE));                                \
        shmem_int_get((void *) (D), (void *) (S), 0, (PE));                               \
        shmem_long_get((void *) (D), (void *) (S), 0, (PE));                              \
        shmem_double_get((void *) (D), (void *) (S), 0, (PE));                            \
        shmem_getmem_nbi((void *) (D), (void *) (S), 0, (PE));                            \
        shmem_get8_nbi((void *) (D), (void *) (S), 0, (PE));                              \
        shmem_get16_nbi((void *) (D), (void *) (S), 0, (PE));                             \
        shmem_get32_nbi((void *) (D), (void *) (S), 0, (PE));                             \
        shmem_get64_nbi((void *) (D), (void *) (S), 0, (PE));                             \
        shmem_get128_nbi((void *) (D), (void *) (S), 0, (PE));                            \
        shmem_int_get_nbi((void *) (D), (void *) (S), 0, (PE));                           \
        shmem_long_get_nbi((void *) (D), (void *) (S), 0, (PE));                          \
        shmem_double_get_nbi((void *) (D), (void *) (S), 0, (PE));                        \
        shmem_iget32((void *) (D), (void *) (S), 1, 1, 0, (PE));                          \
        shmem_iget64((void *) (D), (void *) (S), 2, 3, 0, (PE));                          \
        shmem_iget128((void *) (D), (void *) (S), 1, 1, 0, (PE));                         \
        shmem_int_iget((void *) (D), (void *) (S), 1, 1, 0, (PE));                        \
        shmem_long_iget((void *) (D), (void *) (S), 2, 2, 0, (PE));                       \
        shmem_double_iget((void *) (D), (void *) (S), 1, 1, 0, (PE));                     \
    } while (0)

#define ZERO_CTX_GETS(C, D, S, PE)                                      \
    do {                                                                \
        shmem_ctx_getmem((C), (void *) (D), (void *) (S), 0, (PE));                       \
        shmem_ctx_get32((C), (void *) (D), (void *) (S), 0, (PE));                        \
        shmem_ctx_get64((C), (void *) (D), (void *) (S), 0, (PE));                        \
        shmem_ctx_get128((C), (void *) (D), (void *) (S), 0, (PE));                       \
        shmem_ctx_int_get((C), (void *) (D), (void *) (S), 0, (PE));                      \
        shmem_ctx_long_get((C), (void *) (D), (void *) (S), 0, (PE));                     \
        shmem_ctx_getmem_nbi((C), (void *) (D), (void *) (S), 0, (PE));                   \
        shmem_ctx_get64_nbi((C), (void *) (D), (void *) (S), 0, (PE));                    \
        shmem_ctx_int_get_nbi((C), (void *) (D), (void *) (S), 0, (PE));                  \
        shmem_ctx_double_get_nbi((C), (void *) (D), (void *) (S), 0, (PE));               \
        shmem_ctx_iget64((C), (void *) (D), (void *) (S), 1, 1, 0, (PE));                 \
        shmem_ctx_int_iget((C), (void *) (D), (void *) (S), 2, 2, 0, (PE));               \
    } while (0)

int main(void)
{
    int me, np, i, target, pass, prev;
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

    for (pass = 0; pass < 2; ++pass) {
        target = pass ? (me + 1) % np : me;

        ZERO_GETS(local, sym, target);
        ZERO_GETS(NULL, sym, target);
        ZERO_GETS(local, NULL, target);
        ZERO_GETS(NULL, NULL, target);
        ZERO_GETS(local, nonsym, target);
        ZERO_GETS(NULL, nonsym, target);

        ZERO_CTX_GETS(SHMEM_CTX_DEFAULT, local, sym, target);
        ZERO_CTX_GETS(SHMEM_CTX_DEFAULT, NULL, NULL, target);
        ZERO_CTX_GETS(SHMEM_CTX_DEFAULT, local, nonsym, target);
        if (have_ctx) {
            ZERO_CTX_GETS(ctx, local, sym, target);
            ZERO_CTX_GETS(ctx, NULL, NULL, target);
            ZERO_CTX_GETS(ctx, local, nonsym, target);
            shmem_ctx_quiet(ctx);
        }
        shmem_quiet();
    }
    shmem_barrier_all();

    for (i = 0; i < N; ++i) {
        CHECK(local[i] == -(i + 1),
              "local[%d] changed by zero-length get: expected %d, got %ld",
              i, -(i + 1), local[i]);
        CHECK(nonsym[i] == -1000 - i,
              "nonsym[%d] changed: expected %d, got %ld", i, -1000 - i, nonsym[i]);
        CHECK(sym[i] == 1000 * me + i,
              "sym[%d] changed: expected %ld, got %ld", i, (long) (1000 * me + i), sym[i]);
    }

    /* Normal gets still work afterwards. */
    prev = (me + np - 1) % np;
    shmem_long_get(local, sym, N, prev);
    for (i = 0; i < N; ++i) {
        CHECK(local[i] == 1000 * prev + i,
              "get sym[%d] from PE %d: expected %d, got %ld",
              i, prev, 1000 * prev + i, local[i]);
        local[i] = 0;
    }
    shmem_long_get_nbi(local, sym, N, prev);
    shmem_long_get(local, sym, 0, prev);
    shmem_quiet();
    for (i = 0; i < N; ++i) {
        CHECK(local[i] == 1000 * prev + i,
              "get_nbi sym[%d] from PE %d: expected %d, got %ld",
              i, prev, 1000 * prev + i, local[i]);
    }
    shmem_barrier_all();

    if (have_ctx) {
        shmem_ctx_destroy(ctx);
    }
    shmem_finalize();
    return failures ? 1 : 0;
}
