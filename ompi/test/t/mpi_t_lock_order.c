/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 *
 * White-box test of the lock order between the instance lock
 * (ompi_mpi_instance_lock()) and the MPI_T lock (mca_base_tool_lock()).
 * Runs as a 'make check' program; it calls no MPI function.
 *
 * The instance lock comes before the MPI_T lock: a thread that holds the
 * MPI_T lock must not then take the instance lock, unless it already
 * holds the instance lock (which is recursive).  Taking them in the
 * other order on two threads deadlocks.  A debug build asserts the order
 * when the instance lock is taken.  This test checks that the allowed
 * orders pass the assertion, and that the forbidden order fails it (in a
 * child process, so the abort does not end the test).  The assertion
 * exists only in a debug build, so a release build skips the test.
 */

#include "ompi_config.h"

#include <signal.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include "ompi/instance/instance.h"
#include "opal/mca/base/mca_base_var.h"

#if OPAL_ENABLE_DEBUG

/* Exit status of a child whose assertion fired */
#define ASSERTION_FIRED 42

/* Seconds before the watchdog declares a deadlock */
#define TIMEOUT 60

static void on_abort(int sig)
{
    (void) sig;
    _exit(ASSERTION_FIRED);
}

int main(void)
{
    int failures = 0;
    int status;
    pid_t pid;

    alarm(TIMEOUT);

    /* The allowed order, including taking the instance lock again while
       holding both locks.  An assertion failure here aborts the test. */
    ompi_mpi_instance_lock();
    mca_base_tool_lock();
    ompi_mpi_instance_lock();
    ompi_mpi_instance_unlock();
    mca_base_tool_unlock();
    ompi_mpi_instance_unlock();
    printf("PASS: instance lock, then MPI_T lock, then instance lock again\n");

    /* The forbidden order.  The child's assertion message on stderr is
       expected. */
    fflush(stdout);
    fflush(stderr);
    pid = fork();
    if (pid < 0) {
        perror("fork");
        return 1;
    }
    if (0 == pid) {
        signal(SIGABRT, on_abort);
        mca_base_tool_lock();
        ompi_mpi_instance_lock();
        _exit(0);
    }
    if (pid != waitpid(pid, &status, 0)) {
        perror("waitpid");
        return 1;
    }
    if (WIFEXITED(status) && ASSERTION_FIRED == WEXITSTATUS(status)) {
        printf("PASS: MPI_T lock, then instance lock fails the assertion\n");
    } else if (WIFEXITED(status)) {
        printf("FAIL: MPI_T lock, then instance lock: child exited with status %d, "
               "expected the assertion to fire\n",
               WEXITSTATUS(status));
        ++failures;
    } else if (WIFSIGNALED(status)) {
        printf("FAIL: MPI_T lock, then instance lock: child killed by signal %d, "
               "expected the assertion to fire\n",
               WTERMSIG(status));
        ++failures;
    } else {
        printf("FAIL: MPI_T lock, then instance lock: unexpected child status 0x%x\n", status);
        ++failures;
    }

    printf("%s: %d failure(s)\n", failures ? "FAIL" : "PASS", failures);
    return failures ? 1 : 0;
}

#else /* OPAL_ENABLE_DEBUG */

int main(void)
{
    printf("SKIP: the lock-order assertion exists only in a debug build\n");
    return 77;
}

#endif /* OPAL_ENABLE_DEBUG */
