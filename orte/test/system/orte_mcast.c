/* -*- C -*-
 *
 * $HEADER$
 *
 */
#include <stdio.h>
#include <unistd.h>

#include "opal/dss/dss.h"
#include "opal/event/event.h"

#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_wait.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rmcast/rmcast.h"
#include "orte/mca/grpcomm/grpcomm.h"

static void cbfunc(int channel, opal_buffer_t *buf, void *cbdata);

int main(int argc, char* argv[])
{
    int rc, i;
    char hostname[512];
    pid_t pid;
    opal_buffer_t buf;
    int32_t i32=1;
    
    if (0 > (rc = orte_init(ORTE_PROC_NON_MPI))) {
        fprintf(stderr, "orte_nodename: couldn't init orte - error code %d\n", rc);
        return rc;
    }

    gethostname(hostname, 512);
    pid = getpid();

    printf("orte_nodename: Node %s Name %s Pid %ld\n",
           hostname, ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (long)pid);

    
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    
    if (0 == ORTE_PROC_MY_NAME->vpid) {
        orte_grpcomm.barrier();
        fprintf(stderr, "%d: past barrier\n", (int)ORTE_PROC_MY_NAME->vpid);
        
        if (ORTE_SUCCESS != (rc = orte_rmcast.recv_nb(ORTE_RMCAST_APP_PUBLIC_ADDR, cbfunc, NULL))) {
            ORTE_ERROR_LOG(rc);
        }
        opal_dss.pack(&buf, &i32, 1, OPAL_INT32);
            if (ORTE_SUCCESS != (rc = orte_rmcast.send(ORTE_RMCAST_APP_PUBLIC_ADDR, &buf))) {
                ORTE_ERROR_LOG(rc);
                goto blast;
            }
    } else {
        if (ORTE_SUCCESS != (rc = orte_rmcast.recv_nb(ORTE_RMCAST_APP_PUBLIC_ADDR, cbfunc, NULL))) {
            ORTE_ERROR_LOG(rc);
        }
        orte_grpcomm.barrier();
        fprintf(stderr, "%d: past barrier\n", (int)ORTE_PROC_MY_NAME->vpid);
        
    }
    opal_event_dispatch();
    
blast:
    OBJ_DESTRUCT(&buf);
    
    orte_finalize();
    return 0;
}

static void cbfunc(int channel, opal_buffer_t *buf, void *cbdata)
{
    fprintf(stderr, "GOT MESSAGE\n");
    fflush(stderr);
    orte_finalize();
    exit(0);
}
