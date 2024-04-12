/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of applications
 */

#include <stdio.h>
#include "prte/constants.h"
#include "prte/runtime/runtime.h"

int main(int argc, char* argv[])
{
    if (PRTE_SUCCESS != prte_init(&argc, &argv, PRTE_PROC_NON_MPI)) {
        fprintf(stderr, "Failed prte_init\n");
        exit(1);
    }

    if (PRTE_SUCCESS != prte_finalize()) {
        fprintf(stderr, "Failed prte_finalize\n");
        exit(1);
    }
    return 0;
}
