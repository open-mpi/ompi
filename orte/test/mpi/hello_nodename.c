/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of MPI applications
 */

#define _GNU_SOURCE
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

#include "mpi.h"

int main(int argc, char* argv[])
{
    int rank, size;
    char hostname[512];
    void *appnum;
    void *univ_size;
    char *appstr, *unistr;
    int flag;
    char *envar;

    envar = getenv("OMPI_UNIVERSE_SIZE");

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_get_attr(MPI_COMM_WORLD, MPI_APPNUM, &appnum, &flag);
    if (NULL == appnum) {
        asprintf(&appstr, "UNDEFINED");
    } else {
        asprintf(&appstr, "%d", *(int*)appnum);
    }
    MPI_Comm_get_attr(MPI_COMM_WORLD, MPI_UNIVERSE_SIZE, &univ_size, &flag);
    if (NULL == univ_size) {
        asprintf(&unistr, "UNDEFINED");
    } else {
        asprintf(&unistr, "%d", *(int*)univ_size);
    }
    
    gethostname(hostname, 512);
    printf("Hello, World, I am %d of %d on host %s from app number %s universe size %s universe envar %s\n",
           rank, size, hostname, appstr, unistr, (NULL == envar) ? "NULL" : envar);

    MPI_Finalize();
    return 0;
}
