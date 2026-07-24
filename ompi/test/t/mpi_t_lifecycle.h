/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Shared scaffolding for the MPI_T lifecycle-ordering tests in this
 * directory.  Not installed; test-local only.
 */

#ifndef MPI_T_LIFECYCLE_H
#define MPI_T_LIFECYCLE_H

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>

/* The teardown failures these tests guard against need a component that
   still has events registered on OPAL's shared event base when its
   deferred close runs; the tcp btl is that component.  Pin the pml (the
   btl variable only bites under a BTL-based pml) and the btl so the
   coverage cannot silently evaporate on a build where another pml wins
   selection.  Best-effort: on a host with no usable TCP interface the
   tcp btl yields no modules and registers no events.

   Clearing btl_tcp_if_exclude lets loopback count as a tcp interface,
   so the tcp btl yields >= 2 modules even on a single-NIC host.  That
   matters: at MPI_THREAD_MULTIPLE, a multi-module tcp btl sets
   MCA_BTL_FLAGS_SINGLE_ADD_PROCS, which makes ob1 require comm world
   and sends a sessions-only process through ompi_comm_init_mpi3() --
   the path that once tore down world/self marked PML_ADDED without a
   matching pml add_comm() (SIGSEGV at MPI_Session_finalize()).  Without
   this, that regression is only reachable on multi-NIC hosts. */
static inline void pin_tcp_btl(void)
{
    setenv("OMPI_MCA_pml", "ob1", 1);
    setenv("OMPI_MCA_btl", "tcp,self", 1);
    setenv("OMPI_MCA_btl_tcp_if_exclude", "", 1);
}

/* Every step announces itself and flushes before running, so that if a
   step crashes the process, the last line of output names the killer. */
#define STEP(expr, what)                                        \
    do {                                                        \
        printf("step: %s\n", (what));                           \
        fflush(stdout);                                         \
        if (MPI_SUCCESS != (expr)) {                            \
            printf("FAIL: %s returned non-success\n", (what));  \
            return 1;                                           \
        }                                                       \
    } while (0)

/* Same, but a failure is a SKIP (exit 77): used for the first init call
   of a flavor, where "this environment cannot do that at all" (e.g. no
   singleton support) should not count as a test failure. */
#define STEP_OR_SKIP(expr, what)                                \
    do {                                                        \
        printf("step: %s\n", (what));                           \
        fflush(stdout);                                         \
        if (MPI_SUCCESS != (expr)) {                            \
            printf("SKIP: %s failed; cannot test here\n", (what)); \
            return 77;                                          \
        }                                                       \
    } while (0)

#endif /* MPI_T_LIFECYCLE_H */
