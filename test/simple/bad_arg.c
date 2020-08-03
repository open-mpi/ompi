/*
 * Copyright (c) 2020      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <stdio.h>
#include <unistd.h>
#include "mpi.h"

void errhandler(MPI_Comm* comm, int* err) {
    int rank, len = 0;
    char errstr[MPI_MAX_ERROR_STRING] = {0};
    char cname[MPI_MAX_OBJECT_NAME] = {0};
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Error_string(*err, errstr, &len);
    MPI_Comm_get_name(*comm, cname, &len);
    fprintf(stderr, "Rank %02d: The error handler for %s has been invoked with error %d: %s\n\n", rank, cname, *err, errstr);
}



int main(int argc, char *argv[])
{
    int rank, size, i = 5;
    MPI_Errhandler errh;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    MPI_Comm_create_errhandler(errhandler, &errh);
    MPI_Comm_set_errhandler(MPI_COMM_WORLD, errh);
    MPI_Comm_set_errhandler(MPI_COMM_SELF, errh);

    if(0 == rank) {
        fprintf(stderr,
"This test will have rank 1 pass an invalid argument to a local MPI call.\n"
"An error handler has been set, so:\n"
"  * A high quality implementation should refrain from calling MPI_ERRORS_ARE_FATAL.\n"
"  * An MPI-4 compliant implementation should trigger the MPI_COMM_SELF error handler.\n"
"  * An MPI-3 compliant implementation should trigger the MPI_COMM_WORLD error handler.\n\n");
    }

    sleep(1);

    if(1 == rank) {
        MPI_Type_set_name(MPI_DATATYPE_NULL, "bad type argument");
    }
    else {
        MPI_Type_set_name(MPI_INT, "good type argument");
    }
    /* The reported error was not 'important' so we expect MPI to remain
     * operative */

    /* progress for some time to see if some error handler gets called */
    while(i-- > 0) {
        MPI_Sendrecv_replace(&size, 1, MPI_INT, 0, 0, 0, 0, MPI_COMM_SELF, MPI_STATUS_IGNORE);
        sleep(1);
        if(0 == rank) fprintf(stderr, "\rWaiting for %2ds", i);
    }
    if(0 == rank) fprintf(stderr, "\n");


    MPI_Barrier(MPI_COMM_WORLD);
    fprintf(stderr, "Rank %02d: I have survived till the end of the test.\n", rank);

    MPI_Finalize();
    return 0;
}

