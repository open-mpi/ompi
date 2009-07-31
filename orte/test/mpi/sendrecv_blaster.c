/*
 */

#include <stdio.h>
#include <stdlib.h>
#include "mpi.h"
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <sys/time.h>

int main(int argc, char *argv[])
{ 
    MPI_Status     status;               /* MPI status                          */
    int            mpierr;               /* MPI function return code            */
    int            rank;                 /* Process rank within MPI_COMM_WORLD  */
    int            tag0=41;              /* MPI message tag                     */
    
    int            n_bytes=1024*1024; 
    int            n_loops=2; 
    unsigned char* send_buff;
    unsigned char* recv_buff;
    
    int            i, j, count;
    
    float fraction, randval;
    struct timeval tp;
    
    if ( argc > 2 )
    {
        n_loops = atoi(argv[2]);
        n_loops = n_loops < 1 ? 10 : n_loops;
    }
    if ( argc > 1 )
    {
        n_bytes = atoi(argv[1]);
        n_bytes = n_bytes < 1 ? 32768 : n_bytes;
    }
    
    send_buff = (unsigned char *) valloc(n_bytes);
    recv_buff = (unsigned char *) valloc(n_bytes);
    
    /* seed the random number generator */
    gettimeofday (&tp, NULL);
    srand (tp.tv_usec);

    for ( i=0; i<n_bytes; i++ )
    {
        send_buff[i] = i%128;
    }
    
    mpierr = MPI_Init(&argc, &argv);
    if (mpierr != MPI_SUCCESS)
    {
        fprintf(stderr, "MPI Error %d (MPI_Init)\n",mpierr);
        fflush(stderr);
        MPI_Abort(MPI_COMM_WORLD, -1);
    }
    
    MPI_Errhandler_set(MPI_COMM_WORLD, MPI_ERRORS_RETURN);
    
    mpierr = MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    if (mpierr != MPI_SUCCESS || rank < 0)
    {
        fprintf(stderr, "MPI Error %d (MPI_Comm_rank)\n",mpierr);
        fflush(stderr);
        MPI_Abort(MPI_COMM_WORLD, -1);
    }
    
    i=0;
    j=0;
    while (i < 10000)
    {
        randval = rand();
        fraction = randval/RAND_MAX;
        count = fraction * n_bytes;
        mpierr = MPI_Sendrecv(send_buff, count, MPI_CHAR, rank, tag0,
                              recv_buff, n_bytes, MPI_CHAR, rank, tag0, MPI_COMM_WORLD, &status);
        if (mpierr != MPI_SUCCESS)
        {
            fprintf(stderr,"MPI Error %d (MPI_Sendrecv) %s [%d,%d]\n",mpierr,rank,rank,i);
            fflush(stderr);
            MPI_Abort(MPI_COMM_WORLD, -1);
        }
        if (i == 1000) {
            j++;
            fprintf(stderr, "Rank %d has completed %dk iterations\n", rank, j);
            i = 0;
        }
        i++;
    }
    
    fprintf(stderr, "Rank %d completed test\n", rank);
    MPI_Finalize();
}
