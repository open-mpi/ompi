/* -*- C -*-
 */

#include "orte_config.h"


#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <signal.h>

#include "opal/runtime/opal.h"
#include "opal/mca/pmix/pmix.h"
#include "orte/runtime/runtime.h"
#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"

int main(int argc, char* argv[])
{

    int i, rc;
    double pi;
    pid_t pid;
    const char *hostname;

    if (0 > (rc = orte_init(&argc, &argv, ORTE_PROC_NON_MPI))) {
        fprintf(stderr, "orte_abort: couldn't init orte - error code %d\n", rc);
        return rc;
    }
    pid = getpid();
    hostname = opal_gethostname();

    if (1 < argc) {
        rc = strtol(argv[1], NULL, 10);
    } else {
        rc = 3;
    }

    printf("orte_abort: Name %s Host: %s Pid %ld\n", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
              hostname, (long)pid);
    fflush(stdout);

    if (orte_process_info.my_name.vpid == (orte_process_info.num_procs-1)) {
        printf("ulfm[%ld]: exiting\n", (long)pid);
        exit(0);
    }

    printf("ulfm[%ld]: entering fence\n", (long)pid);
    /* everyone else enters barrier - this should complete */
    opal_pmix.fence(NULL, 0);
    return 0;
}
