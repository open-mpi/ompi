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
 * Contexts created with shmem_ctx_create must return their UCX remote keys
 * when OpenSHMEM is finalized.  UCX reports a leak in its log (stdout by
 * default) when its library is torn down at process exit ("object ...
 * was not returned to mpool ucp_rkeys"), later than anything the process
 * itself can observe.  The test therefore runs itself again as a child
 * with stdout and stderr connected to a pipe, and the parent, which stays
 * outside OpenSHMEM, fails if the child's output contains that message or
 * if the child fails.  Only that
 * message is detected; the child also uses its contexts, so wrong results
 * fail as well.
 *
 * NP: 4
 */

#include <shmem.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

#define CYCLES 200
#define NCTX   6

#define CHILD_ENV "CTX_RKEY_LEAK_CHILD"

static int failures = 0;

#define CHECK(cond, ...)                                          \
    do {                                                          \
        if (!(cond)) {                                            \
            fprintf(stderr, "FAIL: PE %d: ", me);                 \
            fprintf(stderr, __VA_ARGS__);                         \
            fprintf(stderr, "\n");                                \
            failures++;                                           \
        }                                                         \
    } while (0)

static int child_main(void)
{
    long *counter;
    long expect;
    shmem_ctx_t keep[NCTX];
    long options[NCTX] = {0, SHMEM_CTX_PRIVATE, SHMEM_CTX_SERIALIZED, 0,
                          SHMEM_CTX_PRIVATE, SHMEM_CTX_SERIALIZED};
    int me, np, i, rc;

    shmem_init();
    me = shmem_my_pe();
    np = shmem_n_pes();

    counter = shmem_calloc(1, sizeof(*counter));
    CHECK(NULL != counter, "shmem_calloc failed");
    shmem_barrier_all();

    /* Churn: contexts that are created, used and destroyed. */
    for (i = 0; i < CYCLES; i++) {
        shmem_ctx_t ctx;

        rc = shmem_ctx_create(0, &ctx);
        CHECK(0 == rc, "shmem_ctx_create failed: %d", rc);
        if (0 != rc) {
            break;
        }
        shmem_ctx_long_atomic_add(ctx, counter, 1, (me + 1) % np);
        shmem_ctx_quiet(ctx);
        shmem_ctx_destroy(ctx);
    }

    /* Several live contexts with different options; every other one is
     * left undestroyed so that finalize has to release it. */
    for (i = 0; i < NCTX; i++) {
        rc = shmem_ctx_create(options[i], &keep[i]);
        CHECK(0 == rc, "shmem_ctx_create(options %ld) failed: %d",
              options[i], rc);
        if (0 != rc) {
            keep[i] = SHMEM_CTX_INVALID;
            continue;
        }
        shmem_ctx_long_atomic_add(keep[i], counter, 1, (me + 1) % np);
        shmem_ctx_quiet(keep[i]);
    }
    for (i = 0; i < NCTX; i += 2) {
        if (SHMEM_CTX_INVALID != keep[i]) {
            shmem_ctx_destroy(keep[i]);
        }
    }

    shmem_barrier_all();
    expect = CYCLES + NCTX;
    CHECK(*counter == expect, "counter %ld, expected %ld", *counter, expect);

    shmem_free(counter);
    shmem_finalize();

    return failures ? 1 : 0;
}

int main(int argc, char **argv)
{
    int fds[2], status = 0, failed = 0;
    size_t len = 0, cap = 4096;
    char *buf;
    ssize_t n;
    pid_t pid;

    (void) argc;

    if (NULL != getenv(CHILD_ENV)) {
        return child_main();
    }

    if (pipe(fds) != 0) {
        printf("SKIP: cannot create a pipe\n");
        return 77;
    }
    setenv(CHILD_ENV, "1", 1);
    pid = fork();
    if (pid < 0) {
        printf("SKIP: cannot fork\n");
        return 77;
    }
    if (0 == pid) {
        close(fds[0]);
        dup2(fds[1], STDOUT_FILENO);
        dup2(fds[1], STDERR_FILENO);
        close(fds[1]);
        execv(argv[0], argv);
        _exit(127);
    }

    close(fds[1]);
    buf = malloc(cap);
    while (NULL != buf && (n = read(fds[0], buf + len, cap - len - 1)) != 0) {
        if (n < 0) {
            break;
        }
        len += (size_t) n;
        if (cap - len < 1024) {
            cap *= 2;
            buf = realloc(buf, cap);
        }
    }
    close(fds[0]);
    if (NULL == buf) {
        return 1;
    }
    buf[len] = '\0';
    fputs(buf, stdout);
    fflush(stdout);

    waitpid(pid, &status, 0);
    if (!WIFEXITED(status) || 0 != WEXITSTATUS(status)) {
        fprintf(stderr, "FAIL: test process failed, status 0x%x\n", status);
        failed = 1;
    }
    if (NULL != strstr(buf, "not returned to mpool")) {
        fprintf(stderr, "FAIL: UCX reported leaked objects at finalize\n");
        failed = 1;
    }
    free(buf);

    return failed;
}
