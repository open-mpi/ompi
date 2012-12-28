/* -*- C -*-
 *
 * $HEADER$
 *
 */
#include <stdio.h>
#include <unistd.h>

#include "opal/dss/dss.h"
#include "opal/mca/event/event.h"
#include "opal/util/output.h"

#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_wait.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/db/db.h"

int main(int argc, char* argv[])
{
    int rc;
    char hostname[512];
    pid_t pid;
    orte_proc_t *proc;

    if (0 > (rc = orte_init(&argc, &argv, ORTE_PROC_NON_MPI))) {
        fprintf(stderr, "orte_db: couldn't init orte - error code %d\n", rc);
        return rc;
    }
    
    gethostname(hostname, 512);
    pid = getpid();
    
    proc = OBJ_NEW(orte_proc_t);
    if (ORTE_SUCCESS != (rc = orte_db.store(ORTE_PROC_MY_NAME, "test-insert", proc, ORTE_PROC))) {
        ORTE_ERROR_LOG(rc);
    }
    if (ORTE_SUCCESS != (rc = orte_db.fetch(ORTE_PROC_MY_NAME, "test-insert", (void**)&proc, ORTE_PROC))) {
        ORTE_ERROR_LOG(rc);
    }
    if (ORTE_SUCCESS != (rc = orte_db.store(ORTE_PROC_MY_NAME, "test-insert2", proc, ORTE_PROC))) {
        ORTE_ERROR_LOG(rc);
    }
    if (ORTE_SUCCESS != (rc = orte_db.fetch(ORTE_PROC_MY_NAME, "test-insert2", (void**)&proc, ORTE_PROC))) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_RELEASE(proc);

    orte_finalize();
    return 0;
}
