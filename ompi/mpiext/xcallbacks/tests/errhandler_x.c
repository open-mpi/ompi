/*
 * Copyright (c) 2026      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

/*
 * Test program for MPIX_{Comm,Win,File,Session}_create_errhandler_x
 * extended callback APIs.
 *
 * Verifies:
 * 1. Error handler creation with extra_state for each object type
 * 2. extra_state is passed to the callback when error handler is invoked
 * 3. Destructor is invoked exactly once when error handler is freed
 * 4. Error handlers work correctly when attached to their respective objects
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "mpi.h"
#include "mpi-ext.h"

/* Extra state structure to track callback invocations */
typedef struct {
    int magic;              /* Signature to verify correct state pointer */
    int call_count;         /* Number of times error handler was invoked */
    int destructor_called;  /* Flag set when destructor runs */
} extra_state_t;

#define STATE_MAGIC 0xDEADBEEF

/* Comm error handler callback */
static void comm_errhandler_x(MPI_Comm comm, int error_code, void *extra_state)
{
    extra_state_t *state = (extra_state_t *)extra_state;

    /* Verify we got the right state pointer */
    assert(state != NULL);
    assert(state->magic == STATE_MAGIC);

    /* Increment call counter */
    state->call_count++;

    /* Suppress unused parameter warnings */
    (void)comm;
    (void)error_code;
}

/* Win error handler callback */
static void win_errhandler_x(MPI_Win win, int error_code, void *extra_state)
{
    extra_state_t *state = (extra_state_t *)extra_state;

    /* Verify we got the right state pointer */
    assert(state != NULL);
    assert(state->magic == STATE_MAGIC);

    /* Increment call counter */
    state->call_count++;

    /* Suppress unused parameter warnings */
    (void)win;
    (void)error_code;
}

/* File error handler callback */
static void file_errhandler_x(MPI_File file, int error_code, void *extra_state)
{
    extra_state_t *state = (extra_state_t *)extra_state;

    /* Verify we got the right state pointer */
    assert(state != NULL);
    assert(state->magic == STATE_MAGIC);

    /* Increment call counter */
    state->call_count++;

    /* Suppress unused parameter warnings */
    (void)file;
    (void)error_code;
}

/* Session error handler callback */
static void session_errhandler_x(MPI_Session session, int error_code, void *extra_state)
{
    extra_state_t *state = (extra_state_t *)extra_state;

    /* Verify we got the right state pointer */
    assert(state != NULL);
    assert(state->magic == STATE_MAGIC);

    /* Increment call counter */
    state->call_count++;

    /* Suppress unused parameter warnings */
    (void)session;
    (void)error_code;
}

/* Destructor callback - shared for all error handler types */
static void my_destructor(void *extra_state)
{
    extra_state_t *state = (extra_state_t *)extra_state;

    /* Verify we got the right state pointer */
    assert(state != NULL);
    assert(state->magic == STATE_MAGIC);

    /* Mark destructor as called */
    state->destructor_called = 1;
}

/* Test MPIX_Comm_create_errhandler_x */
static int test_comm_errhandler_x(int rank)
{
    MPI_Comm comm = MPI_COMM_NULL;
    MPI_Errhandler eh = MPI_ERRHANDLER_NULL;
    extra_state_t state;
    int errors = 0;

    /* Initialize extra state */
    state.magic = STATE_MAGIC;
    state.call_count = 0;
    state.destructor_called = 0;

    /* Create error handler with extended callback */
    MPIX_Comm_create_errhandler_x(comm_errhandler_x, my_destructor, &state, &eh);
    if (MPI_ERRHANDLER_NULL == eh) {
        fprintf(stderr, "Rank %d: MPIX_Comm_create_errhandler_x failed\n", rank);
        return 1;
    }

    /* Duplicate comm and attach the error handler */
    MPI_Comm_dup(MPI_COMM_WORLD, &comm);
    MPI_Comm_set_errhandler(comm, eh);

    /* Manually invoke the error handler */
    state.call_count = 0;
    MPI_Comm_call_errhandler(comm, MPI_ERR_OTHER);

    /* Verify the callback was invoked */
    if (state.call_count != 1) {
        fprintf(stderr, "Rank %d: Comm error handler callback not invoked "
                "(call_count = %d, expected 1)\n", rank, state.call_count);
        errors++;
    }

    /* Verify destructor has NOT been called yet */
    if (state.destructor_called) {
        fprintf(stderr, "Rank %d: Comm destructor called prematurely\n", rank);
        errors++;
    }

    /* Clean up */
    MPI_Comm_free(&comm);
    MPI_Errhandler_free(&eh);

    /* Verify destructor was called exactly once */
    if (state.destructor_called != 1) {
        fprintf(stderr, "Rank %d: Comm destructor not called after free "
                "(destructor_called = %d)\n", rank, state.destructor_called);
        errors++;
    }

    return errors;
}

/* Test MPIX_Win_create_errhandler_x */
static int test_win_errhandler_x(int rank)
{
    MPI_Win win;
    MPI_Errhandler eh = MPI_ERRHANDLER_NULL;
    extra_state_t state;
    int *baseptr;
    int errors = 0;

    /* Initialize extra state */
    state.magic = STATE_MAGIC;
    state.call_count = 0;
    state.destructor_called = 0;

    /* Create error handler with extended callback */
    MPIX_Win_create_errhandler_x(win_errhandler_x, my_destructor, &state, &eh);
    if (MPI_ERRHANDLER_NULL == eh) {
        fprintf(stderr, "Rank %d: MPIX_Win_create_errhandler_x failed\n", rank);
        return 1;
    }

    /* Create a window */
    MPI_Alloc_mem(sizeof(int), MPI_INFO_NULL, &baseptr);
    *baseptr = rank;
    MPI_Win_create(baseptr, sizeof(int), sizeof(int), MPI_INFO_NULL,
                   MPI_COMM_WORLD, &win);
    MPI_Win_set_errhandler(win, eh);

    /* Manually invoke the error handler */
    state.call_count = 0;
    MPI_Win_call_errhandler(win, MPI_ERR_OTHER);

    /* Verify the callback was invoked */
    if (state.call_count != 1) {
        fprintf(stderr, "Rank %d: Win error handler callback not invoked "
                "(call_count = %d, expected 1)\n", rank, state.call_count);
        errors++;
    }

    /* Verify destructor has NOT been called yet */
    if (state.destructor_called) {
        fprintf(stderr, "Rank %d: Win destructor called prematurely\n", rank);
        errors++;
    }

    /* Clean up */
    MPI_Win_free(&win);
    MPI_Free_mem(baseptr);
    MPI_Errhandler_free(&eh);

    /* Verify destructor was called exactly once */
    if (state.destructor_called != 1) {
        fprintf(stderr, "Rank %d: Win destructor not called after free "
                "(destructor_called = %d)\n", rank, state.destructor_called);
        errors++;
    }

    return errors;
}

/* Test MPIX_File_create_errhandler_x */
static int test_file_errhandler_x(int rank)
{
    MPI_File fh;
    MPI_Errhandler eh = MPI_ERRHANDLER_NULL;
    extra_state_t state;
    char filename[256];
    int errors = 0;

    /* Initialize extra state */
    state.magic = STATE_MAGIC;
    state.call_count = 0;
    state.destructor_called = 0;

    /* Create error handler with extended callback */
    MPIX_File_create_errhandler_x(file_errhandler_x, my_destructor, &state, &eh);
    if (MPI_ERRHANDLER_NULL == eh) {
        fprintf(stderr, "Rank %d: MPIX_File_create_errhandler_x failed\n", rank);
        return 1;
    }

    /* Create a temporary file */
    snprintf(filename, sizeof(filename), "test_file_errhandler_%d.tmp", rank);
    MPI_File_open(MPI_COMM_SELF, filename,
                  MPI_MODE_CREATE | MPI_MODE_RDWR | MPI_MODE_DELETE_ON_CLOSE,
                  MPI_INFO_NULL, &fh);
    MPI_File_set_errhandler(fh, eh);

    /* Manually invoke the error handler */
    state.call_count = 0;
    MPI_File_call_errhandler(fh, MPI_ERR_OTHER);

    /* Verify the callback was invoked */
    if (state.call_count != 1) {
        fprintf(stderr, "Rank %d: File error handler callback not invoked "
                "(call_count = %d, expected 1)\n", rank, state.call_count);
        errors++;
    }

    /* Verify destructor has NOT been called yet */
    if (state.destructor_called) {
        fprintf(stderr, "Rank %d: File destructor called prematurely\n", rank);
        errors++;
    }

    /* Clean up */
    MPI_File_close(&fh);
    MPI_Errhandler_free(&eh);

    /* Verify destructor was called exactly once */
    if (state.destructor_called != 1) {
        fprintf(stderr, "Rank %d: File destructor not called after free "
                "(destructor_called = %d)\n", rank, state.destructor_called);
        errors++;
    }

    return errors;
}

/* Test MPIX_Session_create_errhandler_x */
static int test_session_errhandler_x(int rank)
{
    MPI_Session session;
    MPI_Errhandler eh = MPI_ERRHANDLER_NULL;
    extra_state_t state;
    int errors = 0;

    /* Initialize extra state */
    state.magic = STATE_MAGIC;
    state.call_count = 0;
    state.destructor_called = 0;

    /* Create error handler with extended callback */
    MPIX_Session_create_errhandler_x(session_errhandler_x, my_destructor, &state, &eh);
    if (MPI_ERRHANDLER_NULL == eh) {
        fprintf(stderr, "Rank %d: MPIX_Session_create_errhandler_x failed\n", rank);
        return 1;
    }

    /* Create a session */
    MPI_Session_init(MPI_INFO_NULL, MPI_ERRORS_RETURN, &session);
    MPI_Session_set_errhandler(session, eh);

    /* Manually invoke the error handler */
    state.call_count = 0;
    MPI_Session_call_errhandler(session, MPI_ERR_OTHER);

    /* Verify the callback was invoked */
    if (state.call_count != 1) {
        fprintf(stderr, "Rank %d: Session error handler callback not invoked "
                "(call_count = %d, expected 1)\n", rank, state.call_count);
        errors++;
    }

    /* Verify destructor has NOT been called yet */
    if (state.destructor_called) {
        fprintf(stderr, "Rank %d: Session destructor called prematurely\n", rank);
        errors++;
    }

    /* Clean up */
    MPI_Session_finalize(&session);
    MPI_Errhandler_free(&eh);

    /* Verify destructor was called exactly once */
    if (state.destructor_called != 1) {
        fprintf(stderr, "Rank %d: Session destructor not called after free "
                "(destructor_called = %d)\n", rank, state.destructor_called);
        errors++;
    }

    return errors;
}

int main(int argc, char **argv)
{
    int rank, errors = 0;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    /* Test each error handler type */
    errors += test_comm_errhandler_x(rank);
    errors += test_win_errhandler_x(rank);
    errors += test_file_errhandler_x(rank);
    errors += test_session_errhandler_x(rank);

    MPI_Finalize();

    if (errors == 0 && rank == 0) {
        printf("MPIX errhandler_x tests: PASSED\n");
    }

    return (errors == 0) ? 0 : 1;
}
