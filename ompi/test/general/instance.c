/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Unit test for the ompi/instance layer (instance.c) via the MPI Sessions
 * API, which is fully exercisable single-process: session init/finalize,
 * process-set queries, group-from-pset, communicator-from-group, and
 * session error handlers.
 *
 * Note: the library is compiled with -DNDEBUG, so assert() is a no-op
 * here -- all verification must go through test_verify().
 */

#include "ompi_config.h"

#include <string.h>

#include "support.h"

#include "mpi.h"

static int session_eh_calls = 0;

static void session_errhandler(MPI_Session *session, int *code, ...)
{
    (void) session; (void) code;
    ++session_eh_calls;
}

static void test_session_lifecycle(void);
static void test_psets(void);
static void test_group_and_comm(void);
static void test_session_errhandler(void);

int main(int argc, char *argv[])
{
    (void) argc; (void) argv;
    test_init("ompi instance (sessions)");

    test_session_lifecycle();
    test_psets();
    test_group_and_comm();
    test_session_errhandler();

    return test_finalize();
}

/* ------------------------------------------------------------------ */

static void test_session_lifecycle(void)
{
    MPI_Session session = MPI_SESSION_NULL;
    int rc = MPI_Session_init(MPI_INFO_NULL, MPI_ERRORS_RETURN, &session);
    test_verify("Session_init succeeds", MPI_SUCCESS == rc);
    test_verify("session is not NULL", MPI_SESSION_NULL != session);

    MPI_Info info = MPI_INFO_NULL;
    rc = MPI_Session_get_info(session, &info);
    test_verify("Session_get_info succeeds", MPI_SUCCESS == rc && MPI_INFO_NULL != info);
    if (MPI_INFO_NULL != info) {
        MPI_Info_free(&info);
    }

    rc = MPI_Session_finalize(&session);
    test_verify("Session_finalize succeeds", MPI_SUCCESS == rc);
    test_verify("Session_finalize NULLs the handle", MPI_SESSION_NULL == session);
}

/* ------------------------------------------------------------------ */

static void test_psets(void)
{
    MPI_Session session = MPI_SESSION_NULL;
    MPI_Session_init(MPI_INFO_NULL, MPI_ERRORS_RETURN, &session);

    int npsets = -1;
    int rc = MPI_Session_get_num_psets(session, MPI_INFO_NULL, &npsets);
    test_verify("Session_get_num_psets succeeds", MPI_SUCCESS == rc);
    test_verify("at least the two predefined psets exist", npsets >= 2);

    bool found_world = false, found_self = false;
    for (int i = 0; i < npsets; ++i) {
        char name[MPI_MAX_PSET_NAME_LEN];
        /* pset_len is INOUT: input is the buffer capacity. */
        int len = (int) sizeof(name);
        rc = MPI_Session_get_nth_pset(session, MPI_INFO_NULL, i, &len, name);
        test_verify("Session_get_nth_pset succeeds", MPI_SUCCESS == rc);
        if (0 == strcmp(name, "mpi://WORLD")) {
            found_world = true;
        }
        if (0 == strcmp(name, "mpi://SELF")) {
            found_self = true;
        }
    }
    test_verify("mpi://WORLD pset is present", found_world);
    test_verify("mpi://SELF pset is present", found_self);

    MPI_Info pinfo = MPI_INFO_NULL;
    rc = MPI_Session_get_pset_info(session, "mpi://SELF", &pinfo);
    test_verify("Session_get_pset_info succeeds", MPI_SUCCESS == rc && MPI_INFO_NULL != pinfo);
    if (MPI_INFO_NULL != pinfo) {
        /* mpi_size is a required key on a pset info */
        int flag = 0;
        char val[MPI_MAX_INFO_VAL];
        MPI_Info_get(pinfo, "mpi_size", MPI_MAX_INFO_VAL - 1, val, &flag);
        test_verify("pset info has mpi_size", 1 == flag);
        MPI_Info_free(&pinfo);
    }

    MPI_Session_finalize(&session);
}

/* ------------------------------------------------------------------ */

static void test_group_and_comm(void)
{
    MPI_Session session = MPI_SESSION_NULL;
    MPI_Session_init(MPI_INFO_NULL, MPI_ERRORS_RETURN, &session);

    MPI_Group group = MPI_GROUP_NULL;
    int rc = MPI_Group_from_session_pset(session, "mpi://SELF", &group);
    test_verify("Group_from_session_pset succeeds", MPI_SUCCESS == rc);
    test_verify("pset group is not NULL", MPI_GROUP_NULL != group);

    int gsize = -1;
    MPI_Group_size(group, &gsize);
    test_verify("mpi://SELF group has size 1", 1 == gsize);

    /* NOTE (np>1): MPI_Comm_create_from_group() performs a runtime
     * (PMIx-server-backed) CID allocation over the pset and fails with
     * "PMIx server unreachable" in a launcher-less singleton.  It is
     * therefore deferred to the np>1 effort and not asserted here. */

    MPI_Group_free(&group);
    MPI_Session_finalize(&session);
}

/* ------------------------------------------------------------------ */

static void test_session_errhandler(void)
{
    MPI_Session session = MPI_SESSION_NULL;
    MPI_Session_init(MPI_INFO_NULL, MPI_ERRORS_RETURN, &session);

    MPI_Errhandler eh = MPI_ERRHANDLER_NULL;
    int rc = MPI_Session_create_errhandler(session_errhandler, &eh);
    test_verify("Session_create_errhandler succeeds", MPI_SUCCESS == rc);

    rc = MPI_Session_set_errhandler(session, eh);
    test_verify("Session_set_errhandler succeeds", MPI_SUCCESS == rc);

    MPI_Errhandler got = MPI_ERRHANDLER_NULL;
    rc = MPI_Session_get_errhandler(session, &got);
    test_verify("Session_get_errhandler succeeds", MPI_SUCCESS == rc && got == eh);
    if (MPI_ERRHANDLER_NULL != got) {
        MPI_Errhandler_free(&got);
    }

    session_eh_calls = 0;
    rc = MPI_Session_call_errhandler(session, MPI_ERR_OTHER);
    test_verify("Session_call_errhandler succeeds", MPI_SUCCESS == rc);
    test_verify("session errhandler was invoked", 1 == session_eh_calls);

    MPI_Errhandler_free(&eh);
    MPI_Session_finalize(&session);
}
