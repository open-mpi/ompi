/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Singleton (no launcher required) regression test for the cat_index
 * validation performed by MPI_T_category_get_events and
 * MPI_T_category_get_num_events.
 *
 * Both functions are specified with cat_index in the range
 * [0, num_cat-1] (MPI-5.0 sec. 15.3.9).  An out-of-range index must be
 * reported as MPI_T_ERR_INVALID_INDEX, consistent with the sibling
 * MPI_T_category_get_{cvars,pvars,categories} query functions.  This
 * test asserts that contract for cat_index = -1 and cat_index = num_cat,
 * and that a valid index still returns MPI_SUCCESS (writing nothing
 * while Open MPI registers no MPI_T events).
 */

#include <mpi.h>
#include <stdio.h>

static int failures = 0;

static void check(const char *what, int got, int expected)
{
    if (got != expected) {
        ++failures;
        printf("FAIL: %s returned %d, expected %d\n", what, got, expected);
    } else {
        printf("PASS: %s returned %d\n", what, got);
    }
}

int main(void)
{
    int provided, num_cat = 0, num_events = 0;
    int indices[8];

    MPI_T_init_thread(MPI_THREAD_SINGLE, &provided);
    MPI_T_category_get_num(&num_cat);
    printf("num_cat = %d\n", num_cat);

    /* cat_index = -1 is always out of range, even when num_cat == 0. */
    check("MPI_T_category_get_events(-1)",
          MPI_T_category_get_events(-1, 8, indices),
          MPI_T_ERR_INVALID_INDEX);
    check("MPI_T_category_get_num_events(-1)",
          MPI_T_category_get_num_events(-1, &num_events),
          MPI_T_ERR_INVALID_INDEX);

    /* cat_index = num_cat is one past the last valid index. */
    check("MPI_T_category_get_events(num_cat)",
          MPI_T_category_get_events(num_cat, 8, indices),
          MPI_T_ERR_INVALID_INDEX);
    check("MPI_T_category_get_num_events(num_cat)",
          MPI_T_category_get_num_events(num_cat, &num_events),
          MPI_T_ERR_INVALID_INDEX);

    /* A valid index must still succeed (no events registered yet). */
    if (num_cat > 0) {
        check("MPI_T_category_get_events(0)",
              MPI_T_category_get_events(0, 8, indices),
              MPI_SUCCESS);

        num_events = -1;
        check("MPI_T_category_get_num_events(0)",
              MPI_T_category_get_num_events(0, &num_events),
              MPI_SUCCESS);
        if (0 != num_events) {
            ++failures;
            printf("FAIL: MPI_T_category_get_num_events(0) wrote num_events=%d, "
                   "expected 0\n", num_events);
        }
    }

    MPI_T_finalize();

    if (0 == failures) {
        printf("All MPI_T category event index checks passed.\n");
        return 0;
    }
    printf("%d MPI_T category event index check(s) failed.\n", failures);
    return 1;
}
