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
 * Test program for MPIX_Type_create_keyval_x extended callback API.
 *
 * Verifies:
 * 1. Keyval creation with copy/delete callbacks and destructor
 * 2. extra_state is passed to copy callback during MPI_Type_dup
 * 3. extra_state is passed to delete callback during type free
 * 4. Destructor is invoked exactly once when keyval is freed
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "mpi.h"
#include "mpi-ext.h"

/* Extra state structure to track callback invocations */
typedef struct {
    int magic;               /* Signature to verify correct state pointer */
    int copy_count;          /* Number of times copy callback was invoked */
    int delete_count;        /* Number of times delete callback was invoked */
    int destructor_called;   /* Flag set when destructor runs */
} extra_state_t;

#define STATE_MAGIC 0xDEADBEEF

/* Copy callback - invoked during MPI_Type_dup */
static int my_copy_fn(MPI_Datatype oldtype, int keyval, void *extra_state,
                      void *attribute_val_in, void *attribute_val_out,
                      int *flag)
{
    extra_state_t *state = (extra_state_t *)extra_state;
    int *val_in = (int *)attribute_val_in;
    int **val_out = (int **)attribute_val_out;

    /* Verify we got the right state pointer */
    assert(state != NULL);
    assert(state->magic == STATE_MAGIC);

    /* Increment copy counter */
    state->copy_count++;

    /* Allocate new attribute value and copy the data */
    *val_out = (int *)malloc(sizeof(int));
    assert(*val_out != NULL);
    **val_out = *val_in;

    *flag = 1;  /* Attribute should be copied */
    return MPI_SUCCESS;
}

/* Delete callback - invoked during attribute deletion or type free */
static int my_delete_fn(MPI_Datatype type, int keyval, void *attribute_val,
                        void *extra_state)
{
    extra_state_t *state = (extra_state_t *)extra_state;
    int *val = (int *)attribute_val;

    /* Verify we got the right state pointer */
    assert(state != NULL);
    assert(state->magic == STATE_MAGIC);

    /* Increment delete counter */
    state->delete_count++;

    /* Free the attribute value */
    free(val);

    return MPI_SUCCESS;
}

/* Destructor callback - invoked when keyval is freed */
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
    int rank, errors = 0;
    int keyval = MPI_KEYVAL_INVALID;
    extra_state_t state;
    MPI_Datatype base_type = MPI_DATATYPE_NULL;
    MPI_Datatype dup_type = MPI_DATATYPE_NULL;
    int *attr_val = NULL;
    int *retrieved_val = NULL;
    int flag;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    /* Initialize extra state */
    state.magic = STATE_MAGIC;
    state.copy_count = 0;
    state.delete_count = 0;
    state.destructor_called = 0;

    /* Create keyval with copy/delete callbacks and destructor */
    MPIX_Type_create_keyval_x(my_copy_fn, my_delete_fn, &keyval,
                               &state, my_destructor);
    if (MPI_KEYVAL_INVALID == keyval) {
        fprintf(stderr, "Rank %d: MPIX_Type_create_keyval_x failed\n", rank);
        MPI_Abort(MPI_COMM_WORLD, 1);
    }

    /* Create a base datatype (contiguous type) */
    MPI_Type_contiguous(10, MPI_INT, &base_type);
    MPI_Type_commit(&base_type);

    /* Allocate and set attribute on base_type */
    attr_val = (int *)malloc(sizeof(int));
    assert(attr_val != NULL);
    *attr_val = 99;

    MPI_Type_set_attr(base_type, keyval, attr_val);

    /* Verify attribute was set */
    flag = 0;
    MPI_Type_get_attr(base_type, keyval, &retrieved_val, &flag);
    if (!flag || retrieved_val == NULL || *retrieved_val != 99) {
        fprintf(stderr, "Rank %d: MPI_Type_get_attr failed to retrieve attribute\n",
                rank);
        errors++;
    }

    /* Test 1: Duplicate type - should trigger copy callback */
    MPI_Type_dup(base_type, &dup_type);

    /* Verify copy callback was invoked */
    if (state.copy_count != 1) {
        fprintf(stderr, "Rank %d: Copy callback not invoked during MPI_Type_dup "
                "(copy_count = %d, expected 1)\n",
                rank, state.copy_count);
        errors++;
    }

    /* Verify attribute was copied to new type */
    flag = 0;
    retrieved_val = NULL;
    MPI_Type_get_attr(dup_type, keyval, &retrieved_val, &flag);
    if (!flag || retrieved_val == NULL || *retrieved_val != 99) {
        fprintf(stderr, "Rank %d: Attribute not copied to duplicated type\n",
                rank);
        errors++;
    }

    /* Verify delete callback has NOT been called yet */
    if (state.delete_count != 0) {
        fprintf(stderr, "Rank %d: Delete callback called prematurely "
                "(delete_count = %d)\n",
                rank, state.delete_count);
        errors++;
    }

    /* Test 2: Free duplicated type - should trigger delete callback */
    MPI_Type_free(&dup_type);

    /* Verify delete callback was invoked once */
    if (state.delete_count != 1) {
        fprintf(stderr, "Rank %d: Delete callback not invoked correctly "
                "(delete_count = %d, expected 1)\n",
                rank, state.delete_count);
        errors++;
    }

    /* Verify destructor has NOT been called yet */
    if (state.destructor_called) {
        fprintf(stderr, "Rank %d: Destructor called prematurely\n", rank);
        errors++;
    }

    /* Delete attribute from base_type - should trigger delete callback again */
    MPI_Type_delete_attr(base_type, keyval);

    /* Verify delete callback was invoked a second time */
    if (state.delete_count != 2) {
        fprintf(stderr, "Rank %d: Delete callback not invoked on attribute delete "
                "(delete_count = %d, expected 2)\n",
                rank, state.delete_count);
        errors++;
    }

    /* Free base type */
    MPI_Type_free(&base_type);

    /* Free the keyval - should trigger the destructor */
    MPI_Type_free_keyval(&keyval);

    /* Verify destructor was called exactly once */
    if (state.destructor_called != 1) {
        fprintf(stderr, "Rank %d: Destructor not called after MPI_Type_free_keyval "
                "(destructor_called = %d)\n",
                rank, state.destructor_called);
        errors++;
    }

    /* Verify keyval was set to MPI_KEYVAL_INVALID */
    if (keyval != MPI_KEYVAL_INVALID) {
        fprintf(stderr, "Rank %d: Keyval not set to MPI_KEYVAL_INVALID after free\n",
                rank);
        errors++;
    }

    MPI_Finalize();

    if (errors == 0 && rank == 0) {
        printf("MPIX_Type_create_keyval_x test: PASSED\n");
    }

    return (errors == 0) ? 0 : 1;
}
