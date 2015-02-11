/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of MPI applications
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include "mpi.h"

int main(int argc, char* argv[])
{
    int rank, size;
    char *rk;

    /* get the MPI rank from the environ */
    if (NULL == (rk = getenv("OMPI_COMM_WORLD_RANK"))) {
        fprintf(stderr, "FAILED TO GET RANK\n");
        exit(1);
    }
    if (1 < argc) {
        /* rank 0 exits first */
        if (0 == strcmp(rk, "0")) {
            exit(1);
        } else {
            sleep(1);
        }
    } else {
        if (0 == strcmp(rk, "0")) {
            sleep(1);
            exit(1);
        }        
    }
    
    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    printf("Hello, World, I am %d of %d\n", rank, size);

    MPI_Finalize();
    return 0;
}
