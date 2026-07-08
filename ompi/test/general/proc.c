/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Unit test for the ompi/proc layer (proc.c).  The proc subsystem is
 * entirely runtime-dependent (it is populated from the PMIx modex during
 * MPI_Init), so this test uses a full singleton MPI_Init and then calls
 * the internal ompi_proc_* accessors directly.
 *
 * Memory semantics (see proc.h):
 *   - ompi_proc_world()/get_allocated(): proc refcounts NOT incremented;
 *     caller frees only the returned array.
 *   - ompi_proc_self()/all(): proc refcounts incremented; caller must
 *     OBJ_RELEASE each entry and free the array.
 *   - ompi_proc_local()/find(): no refcount change.
 *
 * Note: the library is compiled with -DNDEBUG, so assert() is a no-op
 * here -- all verification must go through test_verify().
 */

#include "ompi_config.h"

#include <stdlib.h>

#include "support.h"

#include "opal/class/opal_object.h"

#include "ompi/constants.h"
#include "ompi/proc/proc.h"

#include "mpi.h"

int main(int argc, char *argv[])
{
    test_init("ompi proc");

    int rc = MPI_Init(&argc, &argv);
    test_verify("MPI_Init succeeds", MPI_SUCCESS == rc);

    ompi_proc_t *local = ompi_proc_local();
    test_verify("ompi_proc_local() is non-NULL", NULL != local);

    test_verify("ompi_proc_world_size() is 1 (singleton)", 1 == ompi_proc_world_size());

    /* world: array owned by caller, procs not retained */
    size_t sz = 0;
    ompi_proc_t **world = ompi_proc_world(&sz);
    test_verify("ompi_proc_world() returns a list", NULL != world);
    test_verify("world size is 1", 1 == sz);
    test_verify("world[0] is the local proc", NULL != world && world[0] == local);
    free(world);

    /* allocated: array owned by caller, procs not retained */
    sz = 0;
    ompi_proc_t **alloc = ompi_proc_get_allocated(&sz);
    test_verify("ompi_proc_get_allocated() returns a list", NULL != alloc);
    test_verify("allocated size is >= 1", sz >= 1);
    free(alloc);

    /* self: procs retained, caller releases */
    sz = 0;
    ompi_proc_t **self = ompi_proc_self(&sz);
    test_verify("ompi_proc_self() returns a list", NULL != self);
    test_verify("self size is 1", 1 == sz);
    test_verify("self[0] is the local proc", NULL != self && self[0] == local);
    if (NULL != self) {
        for (size_t i = 0; i < sz; ++i) {
            OBJ_RELEASE(self[i]);
        }
        free(self);
    }

    /* all: procs retained, caller releases */
    sz = 0;
    ompi_proc_t **all = ompi_proc_all(&sz);
    test_verify("ompi_proc_all() returns a list", NULL != all);
    test_verify("all size is 1 (singleton)", 1 == sz);
    if (NULL != all) {
        for (size_t i = 0; i < sz; ++i) {
            OBJ_RELEASE(all[i]);
        }
        free(all);
    }

    /* find by name returns the local proc */
    if (NULL != local) {
        ompi_proc_t *found = (ompi_proc_t *) ompi_proc_find(&local->super.proc_name);
        test_verify("ompi_proc_find() locates the local proc by name", found == local);
    }

    rc = ompi_proc_refresh();
    test_verify("ompi_proc_refresh() succeeds", OMPI_SUCCESS == rc);

    int r = test_finalize();
    MPI_Finalize();
    return r;
}
