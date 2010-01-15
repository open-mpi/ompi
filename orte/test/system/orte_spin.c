/* -*- C -*-
 *
 * $HEADER$
 *
 * A program that just spins - provides mechanism for testing user-driven
 * abnormal program termination
 */

#include <stdio.h>

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
        if (i > 100) i = 0;
    }

    orte_finalize();

    return 0;
}
