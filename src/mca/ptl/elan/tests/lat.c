
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <math.h>
#include <sys/time.h>
#include "mpi.h"
#include "test_util.h"

#define MYBUFSIZE (4*1024*1024)
#define CHECK 1
#define PONG 1

char        s_buf[MYBUFSIZE];
char        r_buf[MYBUFSIZE];
int         skip = 0;

int
main (int argc, char *argv[])
{
    int         myid, numprocs, i, j;
    double      startwtime = 0.0, endwtime;
    int         namelen;
    int         size;
    int         loop;
    MPI_Status  stat;
    int         sleep = 1;

    /*while (sleep > 0);*/

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
        s_buf[i] = i;
    }

    fprintf(stderr, "[proc%d:%s:%d] done with init, to loop %d \n",
	    myid, __FUNCTION__, __LINE__, loop);
    fflush(stderr);

    for (i = 0; i < loop + skip; i++) {
	if (i == skip)
	    gettimeofday (&t_start, 0);
	if (myid == 0) {
            MPI_Send (s_buf, size, MPI_CHAR, 1, i, MPI_COMM_WORLD);
	    if (PONG)
            MPI_Recv (r_buf, size, MPI_CHAR, 1, i, MPI_COMM_WORLD, &stat);
	} else {
            MPI_Recv (r_buf, size, MPI_CHAR, 0, i, MPI_COMM_WORLD, &stat);
	    if (PONG)
            MPI_Send (s_buf, size, MPI_CHAR, 0, i, MPI_COMM_WORLD);
        }

	if (CHECK && myid != 0) {
	    for (j=0; j < size; j ++) {
		if (r_buf[j] != j) {
		    fprintf(stderr, "[proc%d:%s] byte %d error, %02X \n",
			    myid, __FUNCTION__, j, r_buf[j]);
		    break;
		} else {
		    r_buf[j] = '0';
		}
	    } 
	}
    }
    gettimeofday (&t_end, 0);

    fprintf(stderr, "[proc%d:%s:%d] pingpong\n",
	    myid, __FUNCTION__, __LINE__);
    fflush(stderr);
 
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
