
#include <stdio.h>
#include <math.h>
#include <sys/time.h>
#include "mpi.h"
#include "test_util.h"

#define MYBUFSIZE (4*1024*1024)
char        s_buf[MYBUFSIZE];
char        r_buf[MYBUFSIZE];
int         skip = 0;

int
main (int argc, char *argv[])
{
    char hostname[32];

    int         myid, numprocs, i;
    double      startwtime = 0.0, endwtime;
    int         namelen;
    int         size;
    int         loop;
    MPI_Status  stat;
    int         sleep = 1;

    struct timeval t_start, t_end;

    loop = 2;

    if (argc < 2) {
        fprintf (stderr, "Usage: %s msg_size\n", argv[0]);
        return 0;
    } else {
	size = atoi (argv[1]);
	if (argc > 2) 
	    loop = atoi (argv[2]);
    }

    /* Get some environmental variables set for Open MPI, OOB */
    env_init_for_elan();

    MPI_Init (&argc, &argv);
    MPI_Comm_size (MPI_COMM_WORLD, &numprocs);
    MPI_Comm_rank (MPI_COMM_WORLD, &myid);

    /* touch the data */
    for (i = 0; i < size; i++) {
        s_buf[i] = 'a' + i;
    }

    gethostname(hostname, 32);

    fprintf(stdout, "[%s:%s:%d] done with init and barrier\n",
	    hostname, __FUNCTION__, __LINE__);
    fflush(stdout);

    for (i = 0; i < loop + skip; i++) {
	if (i == skip)
	    gettimeofday (&t_start, 0);
	if (myid == 0) {
            MPI_Send (s_buf, size, MPI_CHAR, 1, i, MPI_COMM_WORLD);
            /*MPI_Recv (r_buf, size, MPI_CHAR, 1, i, MPI_COMM_WORLD,
	     * &stat);*/
	} else {
            MPI_Recv (r_buf, size, MPI_CHAR, 0, i, MPI_COMM_WORLD, &stat);
            /*MPI_Send (s_buf, size, MPI_CHAR, 0, i, MPI_COMM_WORLD);*/
        }
    }
    gettimeofday (&t_end, 0);

    fprintf(stdout, "[%s:%s:%d] done with pingpong\n",
	    hostname, __FUNCTION__, __LINE__);
    fflush(stdout);
 
    if (myid == 0) {
        double      latency;
        latency = ((1.0e6 * t_end.tv_sec + t_end.tv_usec) 
		- (1.0e6 * t_start.tv_sec + t_start.tv_usec)) / (2.0 * loop);
	fprintf(stdout, "length %d latency %8f\n", 
		size, latency);
	fflush(stdout);
    }
    MPI_Finalize ();
    return 0;
}
