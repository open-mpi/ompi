/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Singleton regression test for the MPI_T events foundation (Phase 0):
 *
 *  - The MPI-4.0 MPI_T error classes MPI_T_ERR_NOT_SUPPORTED and
 *    MPI_T_ERR_NOT_ACCESSIBLE exist, are distinct from each other and
 *    from the neighbouring predefined classes, lie within
 *    MPI_ERR_LASTCODE, and are registered with the error machinery
 *    (MPI_Error_string resolves each to a distinct, non-empty string).
 *
 *  - opal_count_t backs MPI_Count without changing its width: the
 *    MPI_COUNT datatype size equals sizeof(MPI_Count).  (The
 *    same-width invariant itself is enforced at build time by a
 *    _Static_assert in ompi_mpi_init.c; this checks the runtime view.)
 *
 * Runs as a 'make check' singleton (singleton MPI_Init needs no
 * launcher).
 */

#include <mpi.h>
#include <stdio.h>
#include <string.h>

static int failures = 0;

static void expect(const char *what, int cond)
{
    if (cond) {
        printf("PASS: %s\n", what);
    } else {
        ++failures;
        printf("FAIL: %s\n", what);
    }
}

int main(int argc, char **argv)
{
    MPI_Init(&argc, &argv);

    /* The two new classes must be distinct and within range. */
    expect("MPI_T_ERR_NOT_SUPPORTED != MPI_T_ERR_NOT_ACCESSIBLE",
           MPI_T_ERR_NOT_SUPPORTED != MPI_T_ERR_NOT_ACCESSIBLE);
    expect("MPI_T_ERR_NOT_SUPPORTED != MPI_T_ERR_INVALID_NAME",
           MPI_T_ERR_NOT_SUPPORTED != MPI_T_ERR_INVALID_NAME);
    expect("MPI_T_ERR_NOT_ACCESSIBLE != MPI_T_ERR_INVALID_NAME",
           MPI_T_ERR_NOT_ACCESSIBLE != MPI_T_ERR_INVALID_NAME);
    expect("MPI_T_ERR_NOT_SUPPORTED <= MPI_ERR_LASTCODE",
           MPI_T_ERR_NOT_SUPPORTED <= MPI_ERR_LASTCODE);
    expect("MPI_T_ERR_NOT_ACCESSIBLE <= MPI_ERR_LASTCODE",
           MPI_T_ERR_NOT_ACCESSIBLE <= MPI_ERR_LASTCODE);

    /* Both classes must be registered: MPI_Error_string resolves each
       to a non-empty string, and the two strings differ (so a copy
       /paste registration error is caught). */
    char s1[MPI_MAX_ERROR_STRING], s2[MPI_MAX_ERROR_STRING];
    int l1 = 0, l2 = 0;
    MPI_Error_string(MPI_T_ERR_NOT_SUPPORTED, s1, &l1);
    MPI_Error_string(MPI_T_ERR_NOT_ACCESSIBLE, s2, &l2);
    printf("string(NOT_SUPPORTED)  = \"%s\"\n", s1);
    printf("string(NOT_ACCESSIBLE) = \"%s\"\n", s2);
    expect("MPI_Error_string(NOT_SUPPORTED) is non-empty", l1 > 0);
    expect("MPI_Error_string(NOT_ACCESSIBLE) is non-empty", l2 > 0);
    expect("the two error strings differ", 0 != strcmp(s1, s2));

    /* MPI_Count width is unchanged (opal_count_t is a no-op for it). */
    int tsize = 0;
    MPI_Type_size(MPI_COUNT, &tsize);
    printf("MPI_Type_size(MPI_COUNT) = %d ; sizeof(MPI_Count) = %d\n",
           tsize, (int) sizeof(MPI_Count));
    expect("MPI_Type_size(MPI_COUNT) == sizeof(MPI_Count)",
           tsize == (int) sizeof(MPI_Count));

    MPI_Finalize();

    printf("RESULT: %s\n", 0 == failures ? "PASS" : "FAIL");
    return 0 == failures ? 0 : 1;
}
