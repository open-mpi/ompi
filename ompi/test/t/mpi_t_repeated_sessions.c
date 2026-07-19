/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Repeated session cycles under one continuously-held MPI_T reference:
 *
 *     T_init -> (Session_init -> Session_finalize) x3 -> T_finalize
 *
 * This ordering once failed at the second MPI_Session_init(): MPI_T's
 * framework registration holds every framework's refcount, so the first
 * session's finalize cannot truly close the bml framework -- and
 * mca_bml_base_init() therefore skips r2's component init on the second
 * session (that is where btls_added is reset and BTL selection re-runs),
 * while the per-session pml finalize had already emptied r2's module
 * array.  With the "btls_added" guard stale-true over an empty array,
 * add_procs attached zero BTLs and Session_init failed with
 * MPI_ERR_INTERN ("BTLs attempted: (null)").  Fixed by resetting the
 * guard in mca_bml_r2_finalize(), where the state it guards is freed.
 */

#include "mpi_t_lifecycle.h"

int main(void)
{
    int provided;
    MPI_Session s;

    pin_tcp_btl();

    STEP_OR_SKIP(MPI_T_init_thread(MPI_THREAD_SINGLE, &provided), "T_init");
    for (int i = 0; i < 3; ++i) {
        char what[64];
        snprintf(what, sizeof(what), "Session_init (cycle %d, T held)", i);
        if (0 == i) {
            STEP_OR_SKIP(MPI_Session_init(MPI_INFO_NULL, MPI_ERRORS_RETURN, &s), what);
        } else {
            STEP(MPI_Session_init(MPI_INFO_NULL, MPI_ERRORS_RETURN, &s), what);
        }
        snprintf(what, sizeof(what), "Session_finalize (cycle %d, T held)", i);
        STEP(MPI_Session_finalize(&s), what);
    }
    STEP(MPI_T_finalize(), "T_finalize");

    printf("SUCCESS: repeated session cycles under a held MPI_T reference\n");
    return 0;
}
