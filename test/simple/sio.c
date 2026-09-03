/* -*- C -*-
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 *
 * The most basic of MPI applications
 */

#include "mpi.h"
#include <stdio.h>

int main(int argc, char *argv[])
{
    int i;

    MPI_Init(&argc, &argv);

    for (i = 0; i < 100; i++) {
        printf("some output from mpitest to test the xml problem: %d\n", i);
    }

    MPI_Finalize();
    return 0;
}
