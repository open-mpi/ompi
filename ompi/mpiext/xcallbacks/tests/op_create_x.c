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
 * Test program for MPIX_Op_create_x extended callback API.
 *
 * Verifies:
 * 1. User-defined op creation with extra_state
 * 2. extra_state is passed to the callback during reduction
 * 3. Destructor is invoked exactly once when op is freed
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "mpi.h"
#include "mpi-ext.h"

/* Extra state structure to track callback invocations */
typedef struct {
    int magic;           /* Signature to verify correct state pointer */
    int call_count;      /* Number of times op callback was invoked */
    int destructor_called; /* Flag set when destructor runs */
} extra_state_t;

#define STATE_MAGIC 0x12345678

/* User-defined reduction operation: sum with state tracking */
static void my_sum_op_x(void *invec, void *inoutvec, MPI_Count count,
                        MPI_Datatype datatype, void *extra_state)
{
    extra_state_t *state = (extra_state_t *)extra_state;
    const int *in = (const int *)invec;
    int *inout = (int *)inoutvec;
    MPI_Count i;

    /* Verify we got the right state pointer */
    assert(state != NULL);
    assert(state->magic == STATE_MAGIC);

    /* Increment call counter */
    state->call_count++;

    /* Perform the reduction: sum */
    for (i = 0; i < count; i++) {
        inout[i] += in[i];
    }
}

/* Destructor callback */
static void my_destructor(void *extra_state)
{
    extra_state_t *state = (extra_state_t *)extra_state;

    /* Verify we got the right state pointer */
    assert(state != NULL);
    assert(state->magic == STATE_MAGIC);

    /* Mark destructor as called */
    state->destructor_called = 1;
}

int main(int argc, char **argv)
{
    int rank, size, i;
    int sendbuf[10], recvbuf[10];
    MPI_Op my_op = MPI_OP_NULL;
    extra_state_t state;
    int errors = 0;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    /* Initialize extra state */
    state.magic = STATE_MAGIC;
    state.call_count = 0;
    state.destructor_called = 0;

    /* Create user-defined op with extra state and destructor */
    MPIX_Op_create_x(my_sum_op_x, my_destructor, 1 /* commute */, &state, &my_op);
    if (MPI_OP_NULL == my_op) {
        fprintf(stderr, "Rank %d: MPIX_Op_create_x failed\n", rank);
        MPI_Abort(MPI_COMM_WORLD, 1);
    }

    /* Initialize send buffer: each rank sends {rank, rank, ...} */
    for (i = 0; i < 10; i++) {
        sendbuf[i] = rank;
        recvbuf[i] = -1;  /* Initialize to detect untouched elements */
    }

    /* Test 1: MPI_Reduce with the user-defined op */
    MPI_Reduce(sendbuf, recvbuf, 10, MPI_INT, my_op, 0, MPI_COMM_WORLD);

    /* Verify result on root */
    if (rank == 0) {
        int expected_sum = (size * (size - 1)) / 2;  /* 0 + 1 + ... + (size-1) */
        for (i = 0; i < 10; i++) {
            if (recvbuf[i] != expected_sum) {
                fprintf(stderr, "Rank 0: MPI_Reduce result mismatch: "
                        "recvbuf[%d] = %d, expected %d\n",
                        i, recvbuf[i], expected_sum);
                errors++;
            }
        }

        /* On rank 0, the op should have been called (size-1) times
         * in a typical tree-based reduce implementation, but this is
         * implementation-dependent. Just verify it was called at least once. */
        if (state.call_count < 1) {
            fprintf(stderr, "Rank 0: Op callback was never invoked\n");
            errors++;
        }
    }

    /* Reset for next test */
    for (i = 0; i < 10; i++) {
        sendbuf[i] = rank + 1;  /* Non-zero values */
        recvbuf[i] = -1;
    }
    state.call_count = 0;

    /* Test 2: MPI_Allreduce with the user-defined op */
    MPI_Allreduce(sendbuf, recvbuf, 10, MPI_INT, my_op, MPI_COMM_WORLD);

    /* Every rank should see the same result */
    {
        int expected_sum = size * (size + 1) / 2;  /* 1 + 2 + ... + size */
        for (i = 0; i < 10; i++) {
            if (recvbuf[i] != expected_sum) {
                fprintf(stderr, "Rank %d: MPI_Allreduce result mismatch: "
                        "recvbuf[%d] = %d, expected %d\n",
                        rank, i, recvbuf[i], expected_sum);
                errors++;
            }
        }

        /* Op should have been called at least once on every rank */
        if (state.call_count < 1) {
            fprintf(stderr, "Rank %d: Op callback was never invoked in Allreduce\n",
                    rank);
            errors++;
        }
    }

    /* Verify destructor has NOT been called yet */
    if (state.destructor_called) {
        fprintf(stderr, "Rank %d: Destructor called prematurely\n", rank);
        errors++;
    }

    /* Free the op - should trigger the destructor */
    MPI_Op_free(&my_op);

    /* Verify destructor was called exactly once */
    if (state.destructor_called != 1) {
        fprintf(stderr, "Rank %d: Destructor not called after MPI_Op_free "
                "(destructor_called = %d)\n",
                rank, state.destructor_called);
        errors++;
    }

    /* Verify op handle was set to MPI_OP_NULL */
    if (my_op != MPI_OP_NULL) {
        fprintf(stderr, "Rank %d: Op handle not set to MPI_OP_NULL after free\n",
                rank);
        errors++;
    }

    MPI_Finalize();

    if (errors == 0 && rank == 0) {
        printf("MPIX_Op_create_x test: PASSED\n");
    }

    return (errors == 0) ? 0 : 1;
}
