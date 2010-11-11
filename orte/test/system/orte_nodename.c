/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of MPI applications
 */

#include <stdio.h>
#include <unistd.h>

#include "opal/class/opal_list.h"
#include "opal/mca/sysinfo/sysinfo_types.h"
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
    char *keys[] = {
        OPAL_SYSINFO_CPU_TYPE,
        OPAL_SYSINFO_CPU_MODEL,
        OPAL_SYSINFO_NUM_CPUS,
        OPAL_SYSINFO_MEM_SIZE,
        NULL        
    };
    opal_list_t values;
    opal_list_item_t *item;
    opal_sysinfo_value_t *sys;
    
    if (0 > (rc = orte_init(&argc, &argv, ORTE_PROC_NON_MPI))) {
        fprintf(stderr, "orte_nodename: couldn't init orte - error code %d\n", rc);
        return rc;
    }

    if (NULL != (rstrt = getenv("OMPI_MCA_orte_num_restarts"))) {
        restart = strtol(rstrt, NULL, 10);
    }

    gethostname(hostname, 512);
    pid = getpid();

    OBJ_CONSTRUCT(&values, opal_list_t);
    orte_ess.query_sys_info(NULL, keys, &values);
    
    printf("orte_nodename: Node %s Name %s Pid %ld Restarts: %d Num info %d\n",
           hostname, ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (long)pid,
           restart, (int)opal_list_get_size(&values));
    while (NULL != (item = opal_list_remove_first(&values))) {
        sys = (opal_sysinfo_value_t*)item;
        if (OPAL_STRING == sys->type) {
            printf("\t%s: %s\n", sys->key, sys->data.str);
        } else {
            printf("\t%s: %d\n", sys->key, (int)sys->data.i64);

        }
    }

    for (i=0; NULL != environ[i]; i++) {
        if (0 == strncmp(environ[i], "OMPI_MCA", strlen("OMPI_MCA"))) {
            printf("\t%s\n", environ[i]);
        }
    }

    orte_finalize();
    return 0;
}
