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
 * NP: 4
 *
 * Every PE other than PE 0 sends ROUNDS blocks of data to PE 0, each paired
 * with a SHMEM_SIGNAL_ADD of 1 on a per-sender signal word.  PE 0 waits
 * for the signal count of round r and then verifies the data of round r,
 * which must already be fully delivered.  Covers the default context and a
 * private context, blocking and nonblocking.
 */

#include <stdio.h>
#include <stdint.h>
#include <time.h>
#include <shmem.h>

#define ROUNDS 50
#define LEN 16
#define NVARIANTS 4
#define TIMEOUT_SEC 30
#define MAXPES 64

static uint64_t sigs[NVARIANTS][MAXPES];
static int rbuf[NVARIANTS][MAXPES][ROUNDS][LEN];
static int sbuf[ROUNDS][LEN];

static const char *names[NVARIANTS] = {
    "put_signal", "put_signal_nbi", "ctx_put_signal", "ctx_put_signal_nbi"
};

static int value(int v, int sender, int receiver, int round, int k)
{
    return ((v * 31 + sender) * 17 + receiver) * 1009 + round * 37 + k;
}

int main(void)
{
    int me, np, v, r, k, s, failures = 0;
    shmem_ctx_t ctx;
    int have_ctx;

    shmem_init();
    me = shmem_my_pe();
    np = shmem_n_pes();

    if (np < 2 || np > MAXPES) {
        shmem_finalize();
        return 77;
    }

    have_ctx = (0 == shmem_ctx_create(0, &ctx));
    if (!have_ctx) {
        ctx = SHMEM_CTX_DEFAULT;
    }
    shmem_barrier_all();

    for (v = 0; v < NVARIANTS; ++v) {
        shmem_ctx_t c = (v >= 2) ? ctx : SHMEM_CTX_DEFAULT;

        for (r = 0; r < ROUNDS; ++r) {
            if (me != 0) {
                int dst = 0;
                for (k = 0; k < LEN; ++k) {
                    sbuf[r][k] = value(v, me, dst, r, k);
                }
                if (0 == (v & 1)) {
                    shmem_ctx_int_put_signal(c, rbuf[v][me][r], sbuf[r], LEN,
                                             &sigs[v][me], 1,
                                             SHMEM_SIGNAL_ADD, dst);
                } else {
                    shmem_ctx_int_put_signal_nbi(c, rbuf[v][me][r], sbuf[r],
                                                 LEN, &sigs[v][me], 1,
                                                 SHMEM_SIGNAL_ADD, dst);
                    /* sbuf is rewritten for the next destination */
                    shmem_ctx_quiet(c);
                }
            }
        }

        /* Receive: wait for each sender's round r, then check the data. */
        {
            time_t start = time(NULL);
            for (s = 1; me == 0 && s < np; ++s) {
                for (r = 0; r < ROUNDS; ++r) {
                    uint64_t got = 0;
                    while (time(NULL) - start < TIMEOUT_SEC) {
                        got = shmem_uint64_atomic_fetch(&sigs[v][s], me);
                        if (got >= (uint64_t) r + 1) {
                            break;
                        }
                    }
                    if (got < (uint64_t) r + 1) {
                        printf("FAIL: PE %d %s: sender %d round %d signal "
                               "expected >= %d, got %llu\n", me, names[v], s,
                               r, r + 1, (unsigned long long) got);
                        failures++;
                        break;
                    }
                    for (k = 0; k < LEN; ++k) {
                        int want = value(v, s, me, r, k);
                        if (rbuf[v][s][r][k] != want) {
                            printf("FAIL: PE %d %s: sender %d round %d "
                                   "data[%d] expected %d, got %d\n", me,
                                   names[v], s, r, k, want,
                                   rbuf[v][s][r][k]);
                            failures++;
                            break;
                        }
                    }
                }
                if (shmem_uint64_atomic_fetch(&sigs[v][s], me) != ROUNDS &&
                    0 == failures) {
                    printf("FAIL: PE %d %s: sender %d final signal expected "
                           "%d, got %llu\n", me, names[v], s, ROUNDS,
                           (unsigned long long)
                           shmem_uint64_atomic_fetch(&sigs[v][s], me));
                    failures++;
                }
            }
        }
        shmem_ctx_quiet(c);
        shmem_barrier_all();
    }

    if (have_ctx) {
        shmem_ctx_destroy(ctx);
    }
    shmem_finalize();
    return failures ? 1 : 0;
}
