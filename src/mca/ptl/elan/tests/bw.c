#include "mpi.h"
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>

#define MYBUFSIZE (4*1024*1024)
#define MAX_REQ_NUM 1024

MPI_Request request[MAX_REQ_NUM];
MPI_Status  stat[MAX_REQ_NUM];

int
main (int argc,
      char *argv[])
{
    int         myid, numprocs, i;
    int         size, loop, page_size;
    double      t_start = 0.0, t_end = 0.0, t = 0.0;
    char        s_buf[MYBUFSIZE];
    char        r_buf[MYBUFSIZE];

    MPI_Init (&argc, &argv);
    MPI_Comm_size (MPI_COMM_WORLD, &numprocs);
    MPI_Comm_rank (MPI_COMM_WORLD, &myid);

    if (argc < 3) {
        fprintf (stderr, "Usage: bw loop msg_size\n");
        MPI_Finalize ();
        return 0;
    }
    size = atoi (argv[2]);
    loop = atoi (argv[1]);
    page_size = getpagesize ();

    for (i = 0; i < size; i++) {
        s_buf[i] = 'a';
        r_buf[i] = 'b';
    }

    MPI_Barrier (MPI_COMM_WORLD);

    if (myid == 0) {
        t_start = MPI_Wtime ();
        for (i = 0; i < loop; i++) {
            MPI_Isend (s_buf, size, MPI_CHAR, 1, 100, MPI_COMM_WORLD,
                       request + i);
        }
        MPI_Waitall (loop, request, stat);
        MPI_Recv (r_buf, 4, MPI_CHAR, 1, 101, MPI_COMM_WORLD, &stat[0]);
        t_end = MPI_Wtime ();
        t = t_end - t_start;
    } else {
        for (i = 0; i < loop; i++) {
            MPI_Irecv (r_buf, size, MPI_CHAR, 0, 100, MPI_COMM_WORLD,
                       request + i);
        }
        MPI_Waitall (loop, request, stat);
        MPI_Send (s_buf, 4, MPI_CHAR, 0, 101, MPI_COMM_WORLD);
    }

    if (myid == 0) {
        double      tmp;
        tmp = ((size * 1.0) / 1.0e6) * loop;
        fprintf (stdout, "%8d  %8.2f\n", size, tmp / t);
        fflush (stdout);
    }

    MPI_Barrier (MPI_COMM_WORLD);
    MPI_Finalize ();
    return 0;
}
