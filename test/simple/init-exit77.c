/* SPDX-License-Identifier: BSD-3-Clause-Open-MPI */
#include "mpi.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv)
{
    int debugme = 1;

    MPI_Init(&argc, &argv);
    printf("init...\n");
    fflush(0);
    MPI_Finalize();
    exit(77);
}
