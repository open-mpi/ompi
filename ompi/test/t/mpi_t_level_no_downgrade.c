/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * The downgrade direction of the thread-level aliasing bug: a tool
 * calling MPI_T_init_thread(MPI_THREAD_SINGLE) inside a running
 * MPI_THREAD_MULTIPLE application.  MPI_T's level used to be written
 * straight into the World Model's globals, so this silently turned off
 * opal_using_threads()-gated locking and ob1's threaded request paths in
 * a genuinely multithreaded program, and corrupted what
 * MPI_QUERY_THREAD reported (MPI 5.0 sec. 11.6.2 pins it to the value
 * returned by the original MPI_INIT_THREAD).
 *
 * A singleton cannot observe elided locking directly, but it can observe
 * the report: after the MPI_T init, MPI_QUERY_THREAD must still return
 * the world's original level, and MPI_Is_thread_main() must still know
 * the main thread.
 */

#include "mpi_t_lifecycle.h"

int main(void)
{
    int provided, world_level, query, flag;

    pin_tcp_btl();

    STEP_OR_SKIP(MPI_Init_thread(NULL, NULL, MPI_THREAD_MULTIPLE, &world_level),
                 "MPI_Init_thread (MULTIPLE, singleton)");

    STEP(MPI_T_init_thread(MPI_THREAD_SINGLE, &provided), "T_init (SINGLE, inside MPI)");
    if (MPI_THREAD_SINGLE != provided) {
        printf("FAIL: MPI_T provided %d, want MPI_THREAD_SINGLE\n", provided);
        return 1;
    }

    STEP(MPI_Query_thread(&query), "Query_thread");
    if (query != world_level) {
        printf("FAIL: Query_thread %d changed from original %d after "
               "MPI_T_init_thread(SINGLE)\n", query, world_level);
        return 1;
    }

    STEP(MPI_Is_thread_main(&flag), "Is_thread_main");
    if (!flag) {
        printf("FAIL: main thread no longer recognized as main\n");
        return 1;
    }

    STEP(MPI_T_finalize(), "T_finalize");
    STEP(MPI_Finalize(), "MPI_Finalize");

    printf("SUCCESS: no thread-level downgrade from MPI_T\n");
    return 0;
}
