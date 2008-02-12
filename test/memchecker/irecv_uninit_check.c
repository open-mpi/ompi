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
    int send_value[10],recv_value[10];
    int i;
    MPI_Status status;
    MPI_Request send_request;
    MPI_Request recv_request;

    MPI_Init( &argc, &argv );
    
    MPI_Comm_rank( MPI_COMM_WORLD, &rank );
    MPI_Comm_size( MPI_COMM_WORLD, &size );
    
    for(i=0;i<10;i++)
    {
        send_value[i] = i;
        printf("\n%d:%d",i,send_value[i]);
    }

    /*
     * The last value of receive buffer is
     * uninitialized.
     */

    if (size != 2) {
        fprintf (stderr, "Error: Need 2 processes\n");
        MPI_Finalize ();
    }

    /*
     * Now we only send 9 ints, but expect to receive 10.
     * Perfectly valid, but the receiver (in strict mode?) should not
     * depend on the data.
     */
    MPI_Isend (&send_value, 9, MPI_INT,
			   (rank + 1) % size, 4711, MPI_COMM_WORLD, &send_request);
    MPI_Irecv (&recv_value, 10, MPI_INT,
			   (rank + size - 1) % size, 4711, MPI_COMM_WORLD, &recv_request);
	
    MPI_Wait (&send_request, &status);
    MPI_Wait (&recv_request, &status);

    /*
     * This should return an error in any mode.
     */
    printf("\nError: buf[9]:%d",recv_value[9]);

    MPI_Finalize ();
    return 0;
}
