/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Singleton (no launcher required) regression test for an MPI_T reference
 * that outlives MPI itself.
 *
 * MPI_T is reference counted independently of MPI, so a tool may legally
 * bracket MPI entirely (MPI-5.0 sec. 15.3.1):
 *
 *     MPI_T_init_thread() -> MPI_Init() -> MPI_Finalize() -> MPI_T_finalize()
 *
 * MPI_T_init_thread() registers every MCA framework, bumping each framework's
 * refcount.  With the ordering above, MPI_Finalize()'s framework closes are
 * therefore mere decrements: the *true* closes -- component_close(), where
 * components delete the events they registered on OPAL's shared event base --
 * are deferred into MPI_T_finalize(), after MPI_Finalize() has already run
 * opal_finalize().  MPI_T must hold a reference on the shared event base so
 * that it stays alive until those deferred closes have run; this test is the
 * regression guard for that reference.
 *
 * This failed once: opal_event_finalize() began freeing the shared base at
 * opal_finalize() (fixing an fd leak across MPI session cycles) while MPI_T
 * held no reference on it.  The tcp btl's deferred component_close() then
 * decided ownership of its base by comparing against the (by then NULL)
 * opal_sync_event_base global, concluded the shared base was its own, freed
 * it a second time, and this sequence segfaulted inside event_base_free().
 *
 * The ordering is the entire point of the test, so it deliberately calls no
 * MPI_T routine other than init/finalize.
 */

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv)
{
    int provided;

    /* The failure this guards against needs a component that still has events
       registered on the shared event base when its deferred close runs; the
       tcp btl is that component (it registers its listening-socket events on
       the shared base and deletes them at component_close()).  If it is never
       selected, this test exercises nothing and would keep passing even if
       the bug came back, so force it (with self, which is what carries a
       singleton's traffic).  Overwrite rather than defer to the environment:
       a CI runner that pins OMPI_MCA_btl to something else would otherwise
       silently turn this test into a no-op.

       The btl variable only bites under a BTL-based PML, so pin ob1 as well:
       if ucx or a cm/mtl PML won selection, btl=tcp,self would be ignored and
       we would be back to a vacuous test.

       This is best-effort, not a guarantee: on a host with no usable TCP
       interface the tcp btl yields no modules, registers no events, and
       nothing can make it do so. */
    setenv("OMPI_MCA_pml", "ob1", 1);
    setenv("OMPI_MCA_btl", "tcp,self", 1);

    /* MPI_T first: this reference has to outlive the MPI_Finalize() below. */
    if (MPI_SUCCESS != MPI_T_init_thread(MPI_THREAD_SINGLE, &provided)) {
        printf("SKIP: MPI_T_init_thread() failed\n");
        return 77;
    }

    if (MPI_SUCCESS != MPI_Init(&argc, &argv)) {
        printf("SKIP: MPI_Init() failed (no singleton support here)\n");
        return 77;
    }

    MPI_Finalize();

    /* The last MPI_T reference, dropped after MPI_Finalize() already finalized
       OPAL.  This is the call that closes the info components. */
    MPI_T_finalize();

    printf("SUCCESS: MPI_T_finalize() after MPI_Finalize() did not crash\n");

    return 0;
}
