/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of MPI applications
 */

#include <stdio.h>
#include "mpi.h"

#include "orte/util/output.h"

int main(int argc, char* argv[])
{
    int rank, size;
    int stream;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    stream = orte_output_open(NULL, "HELLO", "OUTPUT", NULL);
    orte_output(stream, "(stream) Hello, World, I am %d of %d\n", rank, size);

    printf("(printf) Hello, World, I am %d of %d\n", rank, size);
    
    MPI_Finalize();
    return 0;
}
