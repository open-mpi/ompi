/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of MPI applications
 */

#include <stdio.h>

#include "orte/util/proc_info.h"
#include "orte/runtime/runtime.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/oob/base/base.h"

#define MY_TAG 12345

int main(int argc, char* argv[])
{
    int rc;
    char hostname[512];
    pid_t pid;
    struct iovec msg;
    
    if (0 > (rc = orte_init(ORTE_NON_INFRASTRUCTURE, ORTE_USE_BARRIER))) {
        fprintf(stderr, "orte_nodename: couldn't init orte - error code %d\n", rc);
        return rc;
    }

    gethostname(hostname, 512);
    pid = getpid();

    /* wait for message from our parent */
    if (0 > orte_rml.recv(ORTE_NAME_WILDCARD, &msg, 1, MY_TAG, MCA_OOB_ALLOC)) {
        printf("error at line %d\n", __LINE__);
    }
    
    printf("CHILD  [%lu,%lu,%lu] Node %s Pid %ld\n", ORTE_NAME_ARGS(orte_process_info.my_name), hostname, (long)pid);

    orte_finalize();
    return 0;
}
