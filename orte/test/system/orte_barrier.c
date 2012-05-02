/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of applications
 */

#include <stdio.h>

#include "orte/runtime/runtime.h"
#include "orte/mca/grpcomm/grpcomm.h"

int main(int argc, char* argv[])
{
    orte_grpcomm_collective_t *coll;

    if (ORTE_SUCCESS != orte_init(&argc, &argv, ORTE_PROC_NON_MPI)) {
        fprintf(stderr, "Failed orte_init\n");
        exit(1);
    }
    
    coll = OBJ_NEW(orte_grpcomm_collective_t);
    coll->id = orte_process_info.peer_modex;
    orte_grpcomm.barrier(coll);

    coll->id = orte_process_info.peer_fini_barrier;
    orte_grpcomm.barrier(coll);
    OBJ_RELEASE(coll);

    if (ORTE_SUCCESS != orte_finalize()) {
        fprintf(stderr, "Failed orte_finalize\n");
        exit(1);
    }
    return 0;
}
