#include "mpi.h"
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <sys/time.h>
#include "test_util.h"

#define MYBUFSIZE (1<<22)
#define MAX_REQ_NUM 1024

MPI_Request request[MAX_REQ_NUM];
MPI_Status  tmp_stat[MAX_REQ_NUM];

int         skip = 0;

int
main (int argc,
      char *argv[])
{
    int         myid, numprocs, i, j;
    int         size=0, loop, page_size;
    struct timeval t_start, t_end;
    char        *s_buf, *r_buf;

    s_buf = (char *) malloc (sizeof(char)*MYBUFSIZE);
    r_buf = (char *) malloc (sizeof(char)*MYBUFSIZE);

    /* Get some environmental variables set for Open MPI, OOB */
    env_init_for_elan();

    if (argc < 3) {
        fprintf (stderr, "Usage: %s loop size \n", argv[0]);
        MPI_Finalize ();
        return 0;
    }
    MPI_Init (&argc, &argv);
    MPI_Comm_size (MPI_COMM_WORLD, &numprocs);
    MPI_Comm_rank (MPI_COMM_WORLD, &myid);

    size = atoi (argv[2]);
    loop= atoi (argv[1]);
    page_size = getpagesize ();

    for (i = 0; i < size ; i++) {
        s_buf[i] = 'a';
        r_buf[i] = 'b';
    }

    if (myid == 0) {
	/* Start time */
	if ( i == skip )
	    gettimeofday (&t_start, 0);

	for (j = 0; j < loop; j++) {
	    MPI_Isend (s_buf, size, MPI_CHAR, 1, 100, MPI_COMM_WORLD,
		       request + j);
	}
	MPI_Waitall (loop, request, tmp_stat);
    } else {
	for (j = 0; j < loop; j++) {
	    MPI_Irecv (r_buf, size, MPI_CHAR, 0, 100, MPI_COMM_WORLD,
		       request + j);
	    /*MPI_Wait(request, tmp_stat);*/
	}
	MPI_Waitall (loop, request, tmp_stat);
    }

    if (myid == 0) {
	double      latency;
	gettimeofday (&t_end, 0);
	latency = ((1.0e6 * t_end.tv_sec + t_end.tv_usec) 
		- (1.0e6 * t_start.tv_sec + t_start.tv_usec)) 
	    / (loop);
	fprintf (stdout, "%8d  %8.2f\n", size, size / latency);
	fflush (stdout);
    }
    MPI_Finalize ();
    return 0;
}
