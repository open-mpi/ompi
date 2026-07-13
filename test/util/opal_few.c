/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Unit tests for opal_few().
 *
 * opal_few() forks/execs argv[0] (via execvp) and waits for it to exit.
 * On success it returns OPAL_SUCCESS and fills *status with the raw
 * waitpid(2) status word; the caller inspects it with the standard
 * WIF* / WEXITSTATUS macros.
 */

#include "opal_config.h"

#include "support.h"

#include "opal/util/few.h"
#include "opal/constants.h"
#include "opal/runtime/opal.h"

#include <sys/wait.h>
#include <stdlib.h>

/* ------------------------------------------------------------------ */
/* helpers                                                             */
/* ------------------------------------------------------------------ */

/*
 * These tests invoke "true" and "false" by name (no leading path) so that
 * opal_few()'s execvp() resolves them via PATH.  Passing a name without a
 * slash is more portable than a hard-coded path: macOS has no /bin/true, and
 * not every environment places these utilities under /usr/bin either.
 */

/*
 * test_true_exits_zero
 *
 * Invoke true; expect:
 *  - opal_few returns OPAL_SUCCESS  (child launched and exited)
 *  - WIFEXITED(status) is true     (child exited normally, not signalled)
 *  - WEXITSTATUS(status) == 0      (child reported success)
 */
static void test_true_exits_zero(void)
{
    int status = -1;
    int rc;
    char prog[] = "true";
    char *argv[] = {prog, NULL};

    rc = opal_few(argv, &status);
    test_verify("opal_few(true) returns OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("opal_few(true): WIFEXITED", WIFEXITED(status));
    test_verify("opal_few(true): WEXITSTATUS == 0", 0 == WEXITSTATUS(status));
}

/*
 * test_false_exits_nonzero
 *
 * Invoke false; expect:
 *  - opal_few returns OPAL_SUCCESS  (child launched and exited)
 *  - WIFEXITED(status) is true     (normal exit, not signalled)
 *  - WEXITSTATUS(status) != 0      (child reported failure)
 */
static void test_false_exits_nonzero(void)
{
    int status = -1;
    int rc;
    char prog[] = "false";
    char *argv[] = {prog, NULL};

    rc = opal_few(argv, &status);
    test_verify("opal_few(false) returns OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("opal_few(false): WIFEXITED", WIFEXITED(status));
    test_verify("opal_few(false): WEXITSTATUS != 0", 0 != WEXITSTATUS(status));
}

/*
 * test_status_word_is_waitpid_form
 *
 * Confirm that opal_few fills the raw waitpid status word, not just
 * the exit code.  After a normal exit WIFSIGNALED must be false.
 */
static void test_status_word_is_waitpid_form(void)
{
    int status = -1;
    char prog[] = "true";
    char *argv[] = {prog, NULL};

    opal_few(argv, &status);
    test_verify("opal_few status: WIFSIGNALED is false for normal exit",
                !WIFSIGNALED(status));
}

/* ------------------------------------------------------------------ */
/* main                                                                */
/* ------------------------------------------------------------------ */

int main(int argc, char *argv[])
{
    int r;

    test_init("opal_few");

    opal_init_util(&argc, &argv);

    test_true_exits_zero();
    test_false_exits_nonzero();
    test_status_word_is_waitpid_form();

    r = test_finalize();
    opal_finalize_util();
    return r;
}
