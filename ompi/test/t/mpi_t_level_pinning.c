/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Thread-level reports are pinned per scope (MPI 5.0 secs. 11.6.2 and
 * 15.3.4): MPI_T's level belongs to the tools interface alone and is
 * fixed for its init epoch; MPI_QUERY_THREAD returns what the original
 * MPI_INIT_THREAD returned, forever.  An earlier scope's level may
 * influence what a later init is *granted*, but never changes what an
 * existing scope already reported.
 *
 * Ordering here: MPI_T at MPI_THREAD_MULTIPLE first, then the world at
 * MPI_THREAD_SINGLE.  The strong MPI_T level must not bleed into the
 * world's reported level, and nested MPI_T inits must keep reporting the
 * epoch's pinned level regardless of what they request.
 */

#include "mpi_t_lifecycle.h"

#define CHECK_LEVEL(what, got, want)                                    \
    do {                                                                \
        if ((want) != (got)) {                                          \
            printf("FAIL: %s: got %d, want %d\n", (what), (got), (want)); \
            return 1;                                                   \
        }                                                               \
    } while (0)

int main(void)
{
    int provided, query;

    pin_tcp_btl();

    STEP_OR_SKIP(MPI_T_init_thread(MPI_THREAD_MULTIPLE, &provided), "T_init (MULTIPLE)");
    CHECK_LEVEL("MPI_T provided", provided, MPI_THREAD_MULTIPLE);

    STEP_OR_SKIP(MPI_Init_thread(NULL, NULL, MPI_THREAD_SINGLE, &provided),
                 "MPI_Init_thread (SINGLE, singleton)");
    CHECK_LEVEL("world provided", provided, MPI_THREAD_SINGLE);

    STEP(MPI_Query_thread(&query), "Query_thread");
    CHECK_LEVEL("Query_thread (pinned to world's original)", query, MPI_THREAD_SINGLE);

    /* Nested MPI_T init: epoch level is pinned; the SINGLE request here
       must neither be granted nor leak anywhere. */
    STEP(MPI_T_init_thread(MPI_THREAD_SINGLE, &provided), "T_init (nested, SINGLE)");
    CHECK_LEVEL("MPI_T provided (nested, epoch-pinned)", provided, MPI_THREAD_MULTIPLE);
    STEP(MPI_T_finalize(), "T_finalize (nested)");

    STEP(MPI_Query_thread(&query), "Query_thread (after nested T_init)");
    CHECK_LEVEL("Query_thread (still pinned)", query, MPI_THREAD_SINGLE);

    STEP(MPI_Finalize(), "MPI_Finalize");
    STEP(MPI_T_finalize(), "T_finalize (last)");

    printf("SUCCESS: thread-level reports pinned per scope\n");
    return 0;
}
