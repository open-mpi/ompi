/*
 * Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Sample "example" MPI extension application for Open MPI
 */

#include <stdio.h>
#include "mpi.h"
#include "mpi-ext.h"

int main(int argc, char* argv[])
{
    int rank, size;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    printf("Hello, world, I am %d of %d\n", rank, size);
    OMPI_Progress(3, MPI_COMM_WORLD);

    MPI_Finalize();
    return 0;
}
