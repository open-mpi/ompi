/* -*- C -*-
 *
 * $HEADER$
 *
 * A program that just spins - provides mechanism for testing user-driven
 * abnormal program termination
 */
#include "opal_config.h"

#include <stdio.h>

#include "opal/runtime/opal_progress.h"

#include "orte/runtime/runtime.h"

int main(int argc, char* argv[])
{

    int i;
    double pi;

    orte_init(&argc, &argv, ORTE_PROC_NON_MPI);

    i = 0;
    while (1) {
        i++;
        pi = i / 3.14159256;
        if (i > 100) {
            /* need to progress so we can
             * wake up if our daemon goes
             * away!
             */
            opal_progress();
            /* reset the counter so we loop */
            i = 0;
        }
    }

    orte_finalize();

    return 0;
}
