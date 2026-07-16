/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Unit test for the ompi/errhandler layer (errhandler.c,
 * errhandler_invoke.c, errcode.c, errcode-internal.c).  Exercised
 * through the public MPI error-handler and error-code API after a full
 * singleton MPI_Init.  Custom handlers use MPI_*_call_errhandler so the
 * invoke path runs without actually aborting.
 *
 * Note: the library is compiled with -DNDEBUG, so assert() is a no-op
 * here -- all verification must go through test_verify().
 */

#include "ompi_config.h"

#include <string.h>

#include "support.h"

#include "mpi.h"

static int comm_eh_calls = 0;
static int comm_eh_last_code = 0;

static void comm_errhandler(MPI_Comm *comm, int *code, ...)
{
    (void) comm;
    ++comm_eh_calls;
    comm_eh_last_code = *code;
}

static void test_comm_errhandler(void);
static void test_predefined_errhandlers(void);
static void test_win_errhandler(void);
static void test_error_codes(void);

int main(int argc, char *argv[])
{
    test_init("ompi errhandler");

    int rc = MPI_Init(&argc, &argv);
    test_verify("MPI_Init succeeds", MPI_SUCCESS == rc);

    test_comm_errhandler();
    test_predefined_errhandlers();
    test_win_errhandler();
    test_error_codes();

    int r = test_finalize();
    MPI_Finalize();
    return r;
}

/* ------------------------------------------------------------------ */

static void test_comm_errhandler(void)
{
    MPI_Errhandler eh = MPI_ERRHANDLER_NULL;
    int rc = MPI_Comm_create_errhandler(comm_errhandler, &eh);
    test_verify("Comm_create_errhandler succeeds", MPI_SUCCESS == rc);
    test_verify("created errhandler is non-NULL", MPI_ERRHANDLER_NULL != eh);

    MPI_Comm comm = MPI_COMM_NULL;
    MPI_Comm_dup(MPI_COMM_WORLD, &comm);

    rc = MPI_Comm_set_errhandler(comm, eh);
    test_verify("Comm_set_errhandler succeeds", MPI_SUCCESS == rc);

    MPI_Errhandler got = MPI_ERRHANDLER_NULL;
    rc = MPI_Comm_get_errhandler(comm, &got);
    test_verify("Comm_get_errhandler succeeds", MPI_SUCCESS == rc);
    test_verify("get_errhandler returns the one we set", got == eh);
    MPI_Errhandler_free(&got); /* get_errhandler increments the refcount */

    comm_eh_calls = 0;
    comm_eh_last_code = 0;
    rc = MPI_Comm_call_errhandler(comm, MPI_ERR_OTHER);
    test_verify("Comm_call_errhandler succeeds", MPI_SUCCESS == rc);
    test_verify("custom errhandler was invoked", 1 == comm_eh_calls);
    test_verify("errhandler saw the right error code", MPI_ERR_OTHER == comm_eh_last_code);

    MPI_Comm_free(&comm);
    rc = MPI_Errhandler_free(&eh);
    test_verify("Errhandler_free succeeds", MPI_SUCCESS == rc);
    test_verify("Errhandler_free NULLs the handle", MPI_ERRHANDLER_NULL == eh);
}

/* ------------------------------------------------------------------ */

static void test_predefined_errhandlers(void)
{
    test_verify("MPI_ERRORS_RETURN is non-NULL", MPI_ERRHANDLER_NULL != MPI_ERRORS_RETURN);
    test_verify("MPI_ERRORS_ARE_FATAL is non-NULL",
                MPI_ERRHANDLER_NULL != MPI_ERRORS_ARE_FATAL);

    MPI_Comm comm = MPI_COMM_NULL;
    MPI_Comm_dup(MPI_COMM_WORLD, &comm);

    int rc = MPI_Comm_set_errhandler(comm, MPI_ERRORS_RETURN);
    test_verify("setting MPI_ERRORS_RETURN succeeds", MPI_SUCCESS == rc);

    MPI_Errhandler got = MPI_ERRHANDLER_NULL;
    MPI_Comm_get_errhandler(comm, &got);
    test_verify("get returns MPI_ERRORS_RETURN", got == MPI_ERRORS_RETURN);
    MPI_Errhandler_free(&got);

    /* invoking MPI_ERRORS_RETURN must not abort */
    rc = MPI_Comm_call_errhandler(comm, MPI_ERR_OTHER);
    test_verify("calling MPI_ERRORS_RETURN returns cleanly", MPI_SUCCESS == rc);

    MPI_Comm_free(&comm);
}

/* ------------------------------------------------------------------ */

static int win_eh_calls = 0;

static void win_errhandler(MPI_Win *win, int *code, ...)
{
    (void) win; (void) code;
    ++win_eh_calls;
}

static void test_win_errhandler(void)
{
    static int buffer[8];
    MPI_Win win = MPI_WIN_NULL;
    int rc = MPI_Win_create(buffer, sizeof(buffer), sizeof(int),
                            MPI_INFO_NULL, MPI_COMM_SELF, &win);
    test_verify("Win_create succeeds", MPI_SUCCESS == rc);
    if (MPI_SUCCESS != rc || MPI_WIN_NULL == win) {
        return;
    }

    MPI_Errhandler eh = MPI_ERRHANDLER_NULL;
    rc = MPI_Win_create_errhandler(win_errhandler, &eh);
    test_verify("Win_create_errhandler succeeds", MPI_SUCCESS == rc);

    rc = MPI_Win_set_errhandler(win, eh);
    test_verify("Win_set_errhandler succeeds", MPI_SUCCESS == rc);

    MPI_Errhandler got = MPI_ERRHANDLER_NULL;
    rc = MPI_Win_get_errhandler(win, &got);
    test_verify("Win_get_errhandler succeeds", MPI_SUCCESS == rc);
    test_verify("Win_get_errhandler returns the one we set", got == eh);
    if (MPI_ERRHANDLER_NULL != got) {
        MPI_Errhandler_free(&got);
    }

    win_eh_calls = 0;
    rc = MPI_Win_call_errhandler(win, MPI_ERR_OTHER);
    test_verify("Win_call_errhandler succeeds", MPI_SUCCESS == rc);
    test_verify("custom window errhandler was invoked", 1 == win_eh_calls);

    MPI_Win_free(&win);
    MPI_Errhandler_free(&eh);
}

/* ------------------------------------------------------------------ */

static void test_error_codes(void)
{
    /* error class of a predefined code is the code itself */
    int eclass = -1;
    int rc = MPI_Error_class(MPI_ERR_TRUNCATE, &eclass);
    test_verify("Error_class succeeds", MPI_SUCCESS == rc);
    test_verify("class of MPI_ERR_TRUNCATE is MPI_ERR_TRUNCATE",
                MPI_ERR_TRUNCATE == eclass);

    char str[MPI_MAX_ERROR_STRING];
    int len = -1;
    rc = MPI_Error_string(MPI_SUCCESS, str, &len);
    test_verify("Error_string(MPI_SUCCESS) succeeds", MPI_SUCCESS == rc);
    test_verify("Error_string returns a non-empty string", len > 0);

    /* user-defined error class/code/string */
    int newclass = -1;
    rc = MPI_Add_error_class(&newclass);
    test_verify("Add_error_class succeeds", MPI_SUCCESS == rc);
    test_verify("new class is beyond MPI_ERR_LASTCODE", newclass > MPI_ERR_LASTCODE);

    int newcode = -1;
    rc = MPI_Add_error_code(newclass, &newcode);
    test_verify("Add_error_code succeeds", MPI_SUCCESS == rc);

    rc = MPI_Add_error_string(newcode, "my custom error");
    test_verify("Add_error_string succeeds", MPI_SUCCESS == rc);

    memset(str, 0, sizeof(str));
    len = -1;
    MPI_Error_string(newcode, str, &len);
    test_verify("custom error string round-trips",
                0 == strcmp(str, "my custom error"));

    int c2 = -1;
    MPI_Error_class(newcode, &c2);
    test_verify("class of the new code is the new class", newclass == c2);
}
