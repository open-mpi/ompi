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
 * Singleton regression test: a performance variable whose provider frees
 * its data in MPI_Finalize() must not be usable afterwards, even though
 * the MPI tool information interface (MPI_T) is still initialized.
 *
 * MPI_T is reference counted independently of MPI, so a tool may legally
 * keep using it after MPI_Finalize() (MPI-5.0 sec. 15.3.1).  While MPI_T
 * holds its references, MPI_Finalize() does not close any framework, so
 * the components' MCA variable groups are not deregistered and their
 * performance variables are not invalidated by the framework.  A provider
 * that frees the data behind its performance variables earlier than that
 * -- the monitoring component frees its counters when the PML, OSC, and
 * COLL monitoring components finalize -- must invalidate those variables
 * itself.  Before it did, starting or stopping the monitoring "flush"
 * variable after MPI_Finalize() called its notify callback, and stopping
 * it dumped the freed counters.
 *
 * The flush variable is not bound to an MPI object, so using it after
 * MPI_Finalize() is legal; a conforming implementation must either
 * service the call or report the variable as no longer valid.
 *
 * Finally, a second MPI_T epoch registers the monitoring variables again,
 * which makes them valid again; using them then must not touch the freed
 * counters either.
 */

#include "mpi_t_lifecycle.h"

#include <signal.h>
#include <string.h>
#include <unistd.h>

static void timed_out(int sig)
{
    static const char msg[] = "FAIL: timed out\n";

    (void) sig;
    (void) write(STDOUT_FILENO, msg, sizeof(msg) - 1);
    _exit(1);
}

int main(int argc, char **argv)
{
    int provided, index, count, rc, failures = 0;
    MPI_T_pvar_session session;
    MPI_T_pvar_handle handle, stopped_handle;
    char filename[1024];
    const char *tmpdir = getenv("TMPDIR");

    signal(SIGALRM, timed_out);
    alarm(300);

    /* Monitoring can only be turned on through the environment in a
       singleton; MCA reads it during initialization. */
    setenv("OMPI_MCA_pml_monitoring_enable", "1", 1);

    STEP_OR_SKIP(MPI_T_init_thread(MPI_THREAD_SINGLE, &provided), "T_init");
    STEP_OR_SKIP(MPI_Init(&argc, &argv), "MPI_Init");

    if (MPI_SUCCESS != MPI_T_pvar_get_index("pml_monitoring_flush", MPI_T_PVAR_CLASS_GENERIC,
                                            &index)) {
        /* Monitoring is a common component and may not be part of this
           build at all. */
        printf("SKIP: the monitoring component is not available\n");
        MPI_Finalize();
        MPI_T_finalize();
        return 77;
    }

    STEP(MPI_T_pvar_session_create(&session), "pvar_session_create");
    STEP(MPI_T_pvar_handle_alloc(session, index, NULL, &handle, &count), "pvar_handle_alloc");
    STEP(MPI_T_pvar_start(session, handle), "pvar_start");
    /* A second handle, left stopped, to try starting it afterwards */
    STEP(MPI_T_pvar_handle_alloc(session, index, NULL, &stopped_handle, &count),
         "pvar_handle_alloc (stopped)");

    STEP(MPI_Finalize(), "MPI_Finalize");

    /* Writing the flush variable sets the file name; stopping it dumps
       the monitoring counters to that file.  If the variable is still
       valid, the dump reads the counters that MPI_Finalize() freed. */
    snprintf(filename, sizeof(filename), "%s/mpi_t_pvar_after_finalize.%d",
             (NULL != tmpdir) ? tmpdir : "/tmp", (int) getpid());

    printf("step: pvar_write after MPI_Finalize\n");
    fflush(stdout);
    rc = MPI_T_pvar_write(session, handle, filename);
    if (MPI_SUCCESS == rc) {
        printf("FAIL: pvar_write succeeded after MPI_Finalize freed the variable's data\n");
        ++failures;
    } else if (MPI_T_ERR_INVALID_HANDLE != rc) {
        printf("FAIL: pvar_write returned %d, expected MPI_T_ERR_INVALID_HANDLE\n", rc);
        ++failures;
    }

    printf("step: pvar_stop after MPI_Finalize\n");
    fflush(stdout);
    rc = MPI_T_pvar_stop(session, handle);
    if (MPI_SUCCESS == rc) {
        printf("FAIL: pvar_stop succeeded after MPI_Finalize freed the variable's data\n");
        ++failures;
    } else if (MPI_T_ERR_INVALID_HANDLE != rc) {
        printf("FAIL: pvar_stop returned %d, expected MPI_T_ERR_INVALID_HANDLE\n", rc);
        ++failures;
    }

    /* Starting a handle calls the variable's notify callback, which would
       use the freed data, too. */
    printf("step: pvar_start after MPI_Finalize\n");
    fflush(stdout);
    rc = MPI_T_pvar_start(session, stopped_handle);
    if (MPI_SUCCESS == rc) {
        printf("FAIL: pvar_start succeeded after MPI_Finalize freed the variable's data\n");
        ++failures;
    } else if (MPI_T_ERR_INVALID_HANDLE != rc) {
        printf("FAIL: pvar_start returned %d, expected MPI_T_ERR_INVALID_HANDLE\n", rc);
        ++failures;
    }

    /* Remove the dump file, in case the stop above wrote one. */
    {
        char dumpfile[1100];
        snprintf(dumpfile, sizeof(dumpfile), "%s.0.prof", filename);
        (void) unlink(dumpfile);
    }

    STEP(MPI_T_pvar_handle_free(session, &handle), "pvar_handle_free");
    STEP(MPI_T_pvar_handle_free(session, &stopped_handle), "pvar_handle_free (stopped)");
    STEP(MPI_T_pvar_session_free(&session), "pvar_session_free");
    STEP(MPI_T_finalize(), "T_finalize");

    /* A new MPI_T epoch registers the monitoring component's variables
       again (the environment still enables it), so they are valid again
       even though MPI is finalized and the monitoring data is gone.  Using
       them may succeed or fail, but must not touch the freed data. */
    STEP(MPI_T_init_thread(MPI_THREAD_SINGLE, &provided), "T_init (second epoch)");
    STEP(MPI_T_pvar_get_index("pml_monitoring_flush", MPI_T_PVAR_CLASS_GENERIC, &index),
         "pvar_get_index (second epoch)");
    STEP(MPI_T_pvar_session_create(&session), "pvar_session_create (second epoch)");
    STEP(MPI_T_pvar_handle_alloc(session, index, NULL, &handle, &count),
         "pvar_handle_alloc (second epoch)");
    printf("step: pvar_start/write/stop (second epoch)\n");
    fflush(stdout);
    (void) MPI_T_pvar_start(session, handle);
    (void) MPI_T_pvar_write(session, handle, filename);
    (void) MPI_T_pvar_stop(session, handle);
    {
        char dumpfile[1100];
        snprintf(dumpfile, sizeof(dumpfile), "%s.0.prof", filename);
        (void) unlink(dumpfile);
    }
    STEP(MPI_T_pvar_handle_free(session, &handle), "pvar_handle_free (second epoch)");
    STEP(MPI_T_pvar_session_free(&session), "pvar_session_free (second epoch)");
    STEP(MPI_T_finalize(), "T_finalize (second epoch)");

    if (0 != failures) {
        return 1;
    }

    printf("SUCCESS: the monitoring performance variables are invalid after MPI_Finalize\n");
    return 0;
}
