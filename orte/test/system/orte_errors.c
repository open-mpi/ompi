/* -*- C -*-
 *
 * $HEADER$
 *
 * Check error messages
 */

#include <stdio.h>
#include <unistd.h>

#include "orte/runtime/runtime.h"
#include "orte/mca/errmgr/errmgr.h"

int main(int argc, char* argv[])
{

    int rc, i;

    putenv("OMPI_MCA_orte_report_silent_errors=1");

    if (0 > (rc = orte_init(&argc, &argv, ORTE_PROC_NON_MPI))) {
        fprintf(stderr, "orte_abort: couldn't init orte - error code %d\n", rc);
        return rc;
    }

    for (i=0; ORTE_ERR_MAX < i; i--) {
        fprintf(stderr, "%d: %s\n", -1*i, 
                (NULL == ORTE_ERROR_NAME(i)) ? "NULL" : ORTE_ERROR_NAME(i));
    }

    orte_finalize();
    return 0;
}
