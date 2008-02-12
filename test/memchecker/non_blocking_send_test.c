/*
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include <stdio.h>
#include "mpi.h"

int main( int argc, char *argv[] )
{
    int rank, size;
    int send_value,recv_value;
	
    MPI_Status status;
    MPI_Request send_request;
    MPI_Request recv_request;

    MPI_Init( &argc, &argv );

    MPI_Comm_rank( MPI_COMM_WORLD, &rank );
    MPI_Comm_size( MPI_COMM_WORLD, &size );

    send_value = 10;
    recv_value = 0;
	
    if (size != 2) {
        fprintf (stderr, "Error: Need 2 processes\n");
        MPI_Finalize ();
    }
	
    MPI_Isend (&send_value, 1, MPI_INT,
               (rank + 1) % size, 4711, MPI_COMM_WORLD, &send_request);
    MPI_Irecv (&recv_value, 1, MPI_INT,
               (rank + size - 1) % size, 4711, MPI_COMM_WORLD, &recv_request);

    /*
     * Reading or writing of the send buffer before
     * non blocking send is finished will cause an error.
     */
    printf("\nsent: %d\n",send_value); 
    send_value = 12;

    MPI_Wait (&send_request, &status);
    MPI_Wait (&recv_request, &status);

    /*
     * Using of send buffer after non blocking send
     * is finished will be ok.
     */
    printf("\nsent: %d\n",send_value); 
    send_value = 16;
	  
    MPI_Finalize ();
    return 0;
}

