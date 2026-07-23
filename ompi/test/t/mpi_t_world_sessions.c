/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * All three lifecycles in one process: MPI_T bracketing the world model,
 * with a session opened and closed while MPI is up, and a second session
 * outliving MPI_Finalize().  The world model and the sessions model are
 * allowed to coexist (MPI-4.0 sec. 11); MPI_T is reference counted
 * independently of both.  The final MPI_T_finalize() performs the deferred
 * component closes after every other lifecycle has fully wound down.
 */

#include "mpi_t_lifecycle.h"

int main(void)
{
    int provided;
    MPI_Session s1, s2;

    pin_tcp_btl();

    STEP_OR_SKIP(MPI_T_init_thread(MPI_THREAD_SINGLE, &provided), "T_init");
    STEP_OR_SKIP(MPI_Init(NULL, NULL), "MPI_Init (singleton)");
    STEP_OR_SKIP(MPI_Session_init(MPI_INFO_NULL, MPI_ERRORS_RETURN, &s1),
                 "Session_init #1 (inside MPI)");
    STEP(MPI_Session_finalize(&s1), "Session_finalize #1 (inside MPI)");
    STEP(MPI_Session_init(MPI_INFO_NULL, MPI_ERRORS_RETURN, &s2),
         "Session_init #2 (inside MPI)");
    STEP(MPI_Finalize(), "MPI_Finalize (session #2 still open)");
    STEP(MPI_Session_finalize(&s2), "Session_finalize #2 (after MPI_Finalize)");
    STEP(MPI_T_finalize(), "T_finalize (deferred closes)");

    printf("SUCCESS: MPI_T bracketing world + sessions coexistence\n");
    return 0;
}
