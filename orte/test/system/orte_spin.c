/* -*- C -*-
 *
 * $HEADER$
 *
 * A program that just spins - provides mechanism for testing user-driven
 * abnormal program termination
 */
#include "orte_config.h"
#include "orte/constants.h"

#include <stdio.h>

#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/runtime.h"

int main(int argc, char* argv[])
{
    if (ORTE_SUCCESS != orte_init(&argc, &argv, ORTE_PROC_NON_MPI)) {
        fprintf(stderr, "ORTE_INIT FAILED\n");
        exit(1);
    }
    opal_output(0, "%s RUNNING", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    opal_event.dispatch();

    orte_finalize();

    return 0;
}
