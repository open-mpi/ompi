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

#include "opal/mca/event/event.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/runtime.h"

int main(int argc, char* argv[])
{
    int i;
    float pi;

    if (ORTE_SUCCESS != orte_init(&argc, &argv, ORTE_PROC_NON_MPI)) {
        fprintf(stderr, "ORTE_INIT FAILED\n");
        exit(1);
    }
    opal_output(0, "%s RUNNING", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    i = 0;
    while (1) {
        i++;
        pi = i / 3.14159256;
        if (i == 9995) {
            i=0;
        }
    }

    orte_finalize();

    return 0;
}
