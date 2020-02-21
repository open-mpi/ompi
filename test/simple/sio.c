/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of MPI applications
 */

#include <stdio.h>
#include "mpi.h"

int main(int argc, char* argv[])
{
    int i;

    MPI_Init(&argc, &argv);

    for (i=0; i < 100; i++) {
        printf("some output from mpitest to test the xml problem: %d\n", i);
    }

    MPI_Finalize();
    return 0;
}
