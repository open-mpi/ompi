/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * MPI_T vs. the sessions model.  Unlike MPI_Init(), sessions may be
 * created and destroyed repeatedly in one process, so a single executable
 * can walk every interleaving -- including the sessions analog of the
 * T-outlives-MPI teardown hazard (a session's finalize runs opal_finalize()
 * while MPI_T still holds the frameworks, so the true component closes are
 * deferred to MPI_T_finalize()), overlapping sessions, nested MPI_T around
 * sessions, and interleaved full cycles of each flavor.
 *
 * Repeated session cycles under one continuously-held MPI_T reference live
 * in mpi_t_repeated_sessions.c; see the comment there.
 *
 * Sub-cases run in sequence; the step log names the killer if one crashes.
 */

#include "mpi_t_lifecycle.h"

#include <string.h>

static int t_brackets_session(void)
{
    int provided;
    MPI_Session s;

    STEP(MPI_T_init_thread(MPI_THREAD_SINGLE, &provided), "c1: T_init");
    STEP(MPI_Session_init(MPI_INFO_NULL, MPI_ERRORS_RETURN, &s), "c1: Session_init");
    STEP(MPI_Session_finalize(&s), "c1: Session_finalize");
    STEP(MPI_T_finalize(), "c1: T_finalize");
    return 0;
}

static int t_multiple_brackets_session(void)
{
    int provided;
    MPI_Session s;

    /* Once crashed on macOS (SIGSEGV in ompi_comm_destruct() at the
       session's finalize): MPI_T_init_thread() used to write its thread
       level into the World Model's globals, flipping the process onto
       THREAD_MULTIPLE code paths that the session never chose.  MPI_T's
       level is its own (MPI 5.0 sec. 15.3.4). */
    STEP(MPI_T_init_thread(MPI_THREAD_MULTIPLE, &provided), "c8: T_init (MULTIPLE)");
    if (MPI_THREAD_MULTIPLE != provided) {
        printf("FAIL: c8: T provided %d != MPI_THREAD_MULTIPLE\n", provided);
        return 1;
    }
    STEP(MPI_Session_init(MPI_INFO_NULL, MPI_ERRORS_RETURN, &s), "c8: Session_init");
    STEP(MPI_Session_finalize(&s), "c8: Session_finalize");
    STEP(MPI_T_finalize(), "c8: T_finalize");
    return 0;
}

static int check_session_level(MPI_Session s, const char *want, const char *what)
{
    MPI_Info out;
    char val[64];
    int len = sizeof(val), flag;

    STEP(MPI_Session_get_info(s, &out), what);
    STEP(MPI_Info_get_string(out, "thread_level", &len, val, &flag), what);
    STEP(MPI_Info_free(&out), what);
    if (!flag || 0 != strcmp(val, want)) {
        printf("FAIL: %s: granted \"%s\", want %s\n", what, flag ? val : "(unset)", want);
        return 1;
    }
    return 0;
}

static int mixed_level_downgrade(void)
{
    int rc;
    MPI_Session s1, s2;
    MPI_Info info;

    /* A second scope requesting MPI_THREAD_MULTIPLE while a lower-level
       scope is active cannot be safely upgraded mid-process (components
       already selected were configured at the lower level, and the
       process-wide thread flags cannot change without racing their
       unlocked hot-path readers).  MPI 5.0 sec. 11.6.2 explicitly allows
       the earlier scope to influence the granted level; Open MPI grants
       MPI_THREAD_SERIALIZED in that case, reported through the session's
       "thread_level" info key.  At a quiescent point (no active scope), a
       MULTIPLE request is granted in full. */
    STEP(MPI_Session_init(MPI_INFO_NULL, MPI_ERRORS_RETURN, &s1), "c9: Session_init #1 (default=SINGLE)");
    STEP(MPI_Info_create(&info), "c9: Info_create");
    STEP(MPI_Info_set(info, "thread_level", "MPI_THREAD_MULTIPLE"), "c9: Info_set");
    STEP(MPI_Session_init(info, MPI_ERRORS_RETURN, &s2), "c9: Session_init #2 (MULTIPLE, contended)");
    STEP(MPI_Info_free(&info), "c9: Info_free");
    if (0 != (rc = check_session_level(s2, "MPI_THREAD_SERIALIZED", "c9: contended grant"))) return rc;
    STEP(MPI_Session_finalize(&s2), "c9: Session_finalize #2");
    STEP(MPI_Session_finalize(&s1), "c9: Session_finalize #1");

    STEP(MPI_Info_create(&info), "c9: Info_create (quiescent)");
    STEP(MPI_Info_set(info, "thread_level", "MPI_THREAD_MULTIPLE"), "c9: Info_set (quiescent)");
    STEP(MPI_Session_init(info, MPI_ERRORS_RETURN, &s1), "c9: Session_init (MULTIPLE, quiescent)");
    STEP(MPI_Info_free(&info), "c9: Info_free (quiescent)");
    if (0 != (rc = check_session_level(s1, "MPI_THREAD_MULTIPLE", "c9: quiescent grant"))) return rc;
    STEP(MPI_Session_finalize(&s1), "c9: Session_finalize (quiescent)");
    return 0;
}

static int t_inside_session(void)
{
    int provided;
    MPI_Session s;

    STEP(MPI_Session_init(MPI_INFO_NULL, MPI_ERRORS_RETURN, &s), "c2: Session_init");
    STEP(MPI_T_init_thread(MPI_THREAD_SINGLE, &provided), "c2: T_init");
    STEP(MPI_T_finalize(), "c2: T_finalize");
    STEP(MPI_Session_finalize(&s), "c2: Session_finalize");
    return 0;
}

static int t_outlives_session(void)
{
    int provided;
    MPI_Session s;

    /* The hazard case: Session_finalize() drops the last full-OPAL
       reference from the MPI side, so the deferred component closes run
       at T_finalize(). */
    STEP(MPI_Session_init(MPI_INFO_NULL, MPI_ERRORS_RETURN, &s), "c3: Session_init");
    STEP(MPI_T_init_thread(MPI_THREAD_SINGLE, &provided), "c3: T_init");
    STEP(MPI_Session_finalize(&s), "c3: Session_finalize (T still open)");
    STEP(MPI_T_finalize(), "c3: T_finalize (deferred closes)");
    return 0;
}

static int overlapping_sessions_under_t(void)
{
    int provided;
    MPI_Session s1, s2;

    STEP(MPI_T_init_thread(MPI_THREAD_SINGLE, &provided), "c4: T_init");
    STEP(MPI_Session_init(MPI_INFO_NULL, MPI_ERRORS_RETURN, &s1), "c4: Session_init #1");
    STEP(MPI_Session_init(MPI_INFO_NULL, MPI_ERRORS_RETURN, &s2), "c4: Session_init #2");
    STEP(MPI_Session_finalize(&s1), "c4: Session_finalize #1");
    STEP(MPI_Session_finalize(&s2), "c4: Session_finalize #2");
    STEP(MPI_T_finalize(), "c4: T_finalize");
    return 0;
}

static int nested_t_around_session(void)
{
    int provided;
    MPI_Session s;

    STEP(MPI_T_init_thread(MPI_THREAD_SINGLE, &provided), "c6: T_init (outer)");
    STEP(MPI_T_init_thread(MPI_THREAD_SINGLE, &provided), "c6: T_init (inner)");
    STEP(MPI_Session_init(MPI_INFO_NULL, MPI_ERRORS_RETURN, &s), "c6: Session_init");
    STEP(MPI_Session_finalize(&s), "c6: Session_finalize");
    STEP(MPI_T_finalize(), "c6: T_finalize (inner)");
    STEP(MPI_T_finalize(), "c6: T_finalize (outer)");
    return 0;
}

static int interleaved_cycles(void)
{
    int provided;
    MPI_Session s;

    /* Full T cycle, then full session cycle, then both interleaved: the
       process's OPAL layers go through several complete teardowns. */
    STEP(MPI_T_init_thread(MPI_THREAD_SINGLE, &provided), "c7: T_init (solo cycle)");
    STEP(MPI_T_finalize(), "c7: T_finalize (solo cycle)");
    STEP(MPI_Session_init(MPI_INFO_NULL, MPI_ERRORS_RETURN, &s), "c7: Session_init (solo cycle)");
    STEP(MPI_Session_finalize(&s), "c7: Session_finalize (solo cycle)");
    STEP(MPI_T_init_thread(MPI_THREAD_SINGLE, &provided), "c7: T_init");
    STEP(MPI_Session_init(MPI_INFO_NULL, MPI_ERRORS_RETURN, &s), "c7: Session_init");
    STEP(MPI_Session_finalize(&s), "c7: Session_finalize");
    STEP(MPI_T_finalize(), "c7: T_finalize");
    return 0;
}

int main(void)
{
    int provided, rc;
    MPI_Session probe;

    pin_tcp_btl();

    /* Can this environment do sessions and MPI_T at all?  (No singleton
       support, etc., is a skip, not a failure.) */
    STEP_OR_SKIP(MPI_T_init_thread(MPI_THREAD_SINGLE, &provided), "probe: T_init");
    STEP_OR_SKIP(MPI_Session_init(MPI_INFO_NULL, MPI_ERRORS_RETURN, &probe), "probe: Session_init");
    STEP(MPI_Session_finalize(&probe), "probe: Session_finalize");
    STEP(MPI_T_finalize(), "probe: T_finalize");

    if (0 != (rc = t_brackets_session())) return rc;
    if (0 != (rc = t_inside_session())) return rc;
    if (0 != (rc = t_outlives_session())) return rc;
    if (0 != (rc = overlapping_sessions_under_t())) return rc;
    if (0 != (rc = nested_t_around_session())) return rc;
    if (0 != (rc = interleaved_cycles())) return rc;
    if (0 != (rc = t_multiple_brackets_session())) return rc;
    if (0 != (rc = mixed_level_downgrade())) return rc;

    printf("SUCCESS: all MPI_T x sessions interleavings\n");
    return 0;
}
