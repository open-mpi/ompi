/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of MPI applications
 */

#include <stdio.h>
#include <unistd.h>

#include "opal/class/opal_list.h"
#include "opal/util/opal_environ.h"

#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/runtime.h"
#include "orte/mca/ess/ess.h"

int main(int argc, char* argv[])
{
    int rc, i, restart=-1;
    char hostname[512], *rstrt;
    pid_t pid;
    
    if (0 > (rc = orte_init(&argc, &argv, ORTE_PROC_NON_MPI))) {
        fprintf(stderr, "orte_nodename: couldn't init orte - error code %d\n", rc);
        return rc;
    }

    if (NULL != (rstrt = getenv("OMPI_MCA_orte_num_restarts"))) {
        restart = strtol(rstrt, NULL, 10);
    }

    gethostname(hostname, 512);
    pid = getpid();

    printf("orte_nodename: Node %s Name %s Pid %ld Restarts: %d\n",
           hostname, ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (long)pid, restart);

    for (i=0; NULL != environ[i]; i++) {
        if (0 == strncmp(environ[i], "OMPI_MCA", strlen("OMPI_MCA"))) {
            printf("\t%s\n", environ[i]);
        }
    }

    orte_finalize();
    return 0;
}
