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
 * NP: 8
 *
 * Every PE other than the target performs many SHMEM_SIGNAL_ADD
 * put-with-signal operations to a single target PE, using the default
 * context and a private context, blocking and nonblocking.  The final
 * signal value must equal the sum of all added values and every data slot
 * must hold its sender's payload.
 */

#include <stdio.h>
#include <stdint.h>
#include <time.h>
#include <shmem.h>

#define ITERS 50
#define NVARIANTS 4
#define TIMEOUT_SEC 30

static uint64_t sigs[NVARIANTS];
static int data[NVARIANTS][64];

static const char *names[NVARIANTS] = {
    "put_signal", "put_signal_nbi", "ctx_put_signal", "ctx_put_signal_nbi"
};

int main(void)
{
    int me, np, target = 0, v, i, pe, failures = 0;
    shmem_ctx_t ctx;
    int have_ctx;

    shmem_init();
    me = shmem_my_pe();
    np = shmem_n_pes();

    if (np < 2) {
        shmem_finalize();
        return 77;
    }

    have_ctx = (0 == shmem_ctx_create(0, &ctx));
    if (!have_ctx) {
        ctx = SHMEM_CTX_DEFAULT;
    }

    for (v = 0; v < NVARIANTS; ++v) {
        sigs[v] = 0;
    }
    shmem_barrier_all();

    for (v = 0; v < NVARIANTS; ++v) {
        int payload = 1000 * (v + 1) + me;

        if (me != target) {
            for (i = 0; i < ITERS; ++i) {
                /* Each sending PE adds me+1 per iteration. */
                switch (v) {
                case 0:
                    shmem_int_put_signal(&data[v][me], &payload, 1, &sigs[v],
                                         me + 1, SHMEM_SIGNAL_ADD, target);
                    break;
                case 1:
                    shmem_int_put_signal_nbi(&data[v][me], &payload, 1,
                                             &sigs[v], me + 1,
                                             SHMEM_SIGNAL_ADD, target);
                    break;
                case 2:
                    shmem_ctx_int_put_signal(ctx, &data[v][me], &payload, 1,
                                             &sigs[v], me + 1,
                                             SHMEM_SIGNAL_ADD, target);
                    break;
                default:
                    shmem_ctx_int_put_signal_nbi(ctx, &data[v][me], &payload,
                                                 1, &sigs[v], me + 1,
                                                 SHMEM_SIGNAL_ADD, target);
                    break;
                }
            }
            shmem_ctx_quiet((2 == v || 3 == v) ? ctx : SHMEM_CTX_DEFAULT);
        } else {
            time_t start = time(NULL);
            uint64_t expected = (uint64_t) ITERS * (np - 1) * (np + 2) / 2;
            uint64_t got = 0;

            /* Poll the signal word with atomic fetches. */
            while (time(NULL) - start < TIMEOUT_SEC) {
                got = shmem_uint64_atomic_fetch(&sigs[v], me);
                if (got >= expected) {
                    break;
                }
            }
            if (got != expected) {
                printf("FAIL: PE %d %s: signal expected %llu, got %llu\n", me,
                       names[v], (unsigned long long) expected,
                       (unsigned long long) got);
                failures++;
            }
            for (pe = 0; pe < np; ++pe) {
                int want = (pe == target) ? 0 : 1000 * (v + 1) + pe;
                if (data[v][pe] != want) {
                    printf("FAIL: PE %d %s: data[%d] expected %d, got %d\n",
                           me, names[v], pe, want, data[v][pe]);
                    failures++;
                }
            }
        }
        shmem_barrier_all();
    }

    shmem_barrier_all();
    if (have_ctx) {
        shmem_ctx_destroy(ctx);
    }
    shmem_finalize();
    return failures ? 1 : 0;
}
