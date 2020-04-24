#include <stdio.h>
#include "mpi.h"


int main( int argc, char *argv[] )
{
    MPI_Status status;
    MPI_Comm comm,scomm;
    int rank, size, color, errs=0;
    MPI_Init( 0, 0 );
    MPI_Comm_rank( MPI_COMM_WORLD, &rank );
    color = rank % 2;
    printf("%d Calling split\n", rank);
    MPI_Comm_split( MPI_COMM_WORLD, color, rank, &scomm );
    printf("%d Calling Intercomm_create\n", rank);
    MPI_Intercomm_create( scomm, 0, MPI_COMM_WORLD, 1-color, 1, &comm);
    printf("%d Completet\n", rank);
    MPI_Comm_rank( comm, &rank );
    MPI_Comm_remote_size( comm, &size );
    MPI_Comm_free(&scomm);
    MPI_Comm_free(&comm);
    MPI_Finalize();
    return errs;
}

