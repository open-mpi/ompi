/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of MPI applications
 */

#include <stdio.h>

#include "orte/util/proc_info.h"

int main(int argc, char* argv[])
{
    int rc;
    char hostname[512];
    
    if (0 > (rc = orte_init())) {
        fprintf(stderr, "orte_nodename: couldn't init orte - error code %d\n", rc);
        return rc;
    }

    gethostname(hostname, 512);
    printf("orte_nodename: Node %s Name [%lu,%lu,%lu]\n", hostname, ORTE_NAME_ARGS(orte_process_info.my_name));

    orte_finalize();
    return 0;
}
