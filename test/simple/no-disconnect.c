/* Contributed by Marcia Cristina Cera
   <marcia.cristina.cera@gmail.com>,
   http://www.open-mpi.org/community/lists/users/2009/12/11540.php */

/* It was decided that the issue highlighted by this test will NOT be
   fixed in the 1.3/1.4 series.  It is already fixed in the 1.5
   series.  Hence, if we detect Open MPI < v1.5, return 77/skip. */
/* Turns out the hnp cannot handle concurrent MPI_Comm_spawns
   as of Open MPI 1.7.  However, we hope this feature will
   work in 2.0. with the new state machine based orte. */

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <sys/utsname.h>

#include <mpi.h>

#define NCHARS 30
const int max_depth = 4;

/*
 * Here are some replacements for standard, blocking MPI
 * functions.  These replacements are "nice" and yield the
 * CPU instead of spinning hard.  The interfaces are the same.
 * Just replace:
 *     MPI_Recv    with  nice_recv
 *     MPI_Send    with  nice_send
 *     MPI_Barrier with  nice_barrier
 */


static int nice_send(void *buf, int count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm) {
    /* Assume a standard (presumably short/eager) send suffices. */
    return MPI_Send(buf, count, datatype, dest, tag, comm);
}


static int nice_recv(void *buf, int count, MPI_Datatype datatype, int source, int tag, MPI_Comm comm, MPI_Status *status) {
    MPI_Request req;
    int flag;
    struct timespec dt;

    /*
     * We're only interested in modest levels of oversubscription
     * -- e.g., 2-4x more processes than physical processors.
     * So, the sleep time only needs to be about 2-4x longer than
     * a futile MPI_Test call.  For a wide range of processors,
     * something less than a millisecond should be sufficient.
     * Excessive sleep times (e.g., 1 second) would degrade performance.
     */
    dt.tv_sec    =       0;
    dt.tv_nsec   =  100000;

    MPI_Irecv(buf, count, datatype, source, tag, comm, &req);

    MPI_Test(&req, &flag, status);
    while ( ! flag ) {
        nanosleep(&dt, NULL);
        MPI_Test(&req, &flag, status);
    }
    return MPI_SUCCESS;
}


static void nice_barrier(MPI_Comm comm) {
    int me, np, jump, buf = -1;

    MPI_Comm_rank(comm,&me);
    MPI_Comm_size(comm,&np);

    /* fan in */
    for ( jump = 1; jump < np; jump <<= 1 ) {
        if ( ( me & jump ) != 0 ) {
            nice_send(&buf, 1, MPI_INT, me - jump, 343, comm);
            break;
        } else if ( me + jump < np ) {
            nice_recv(&buf, 1, MPI_INT, me + jump, 343, comm, MPI_STATUS_IGNORE);
        }
    }

    /* fan out */
    if ( 0 != me ) {
        nice_recv(&buf, 1, MPI_INT, me - jump, 344, comm, MPI_STATUS_IGNORE);
    }
    jump >>= 1;
    for ( ; jump > 0; jump >>= 1 ) {
        if ( me + jump < np ) {
            nice_send(&buf, 1, MPI_INT, me + jump, 344, comm);
        }
    }
}


int main (int argc, char **argv)
{
    char bufs   [NCHARS];               /* send buffer  */
    char bufr[2][NCHARS];               /* recv buffers */
    MPI_Comm parent;
    int level = 0, participate = 1;
    struct utsname buf;

    /* If this is prior to OMPI v2.0, return 77/skip */
#if defined(OPEN_MPI)
    if (OMPI_MAJOR_VERSION < 2) {
        printf("Skipping, because the orte cannot handle concurrent MPI_Comm_spawns\n");
        return 77;
    } else {
        printf("Verify that this test is truly working because conncurrent MPI_Comm_spawns"
               " has not worked before.\n");
    }
#endif

    uname(&buf);
    printf("I AM pid %d with level %d on %s\n", getpid(), (argc < 2)?0:atoi(argv[1]), buf.nodename);

    MPI_Init(&argc, &argv);
    MPI_Comm_get_parent(&parent);

    if (MPI_COMM_NULL != parent) {
        /* spawned processes get stuff from parent */
        level = atoi(argv[1]);
        MPI_Recv(&bufr[0], sizeof(char)*NCHARS, MPI_CHAR, MPI_ANY_SOURCE,
                 MPI_ANY_TAG, parent, MPI_STATUS_IGNORE);
        printf("Parent sent: %s\n", bufr[0]);
    } else {

        /* original processes have to decide whether to participate */

        /* In this test, each process launched by "mpirun -n <np>" spawns a
         * binary tree of processes.  You end up with <np> * ( 1 << max_depth )
         * processes altogether.  For max_depth=4, this means 16*<np>.  There
         * is potential here for heavy oversubscription, especially if in
         * testing we launch tests with <np> set to the number of available
         * processors.  This test tolerates oversubscription somewhat since
         * it entails little inter-process synchronization.  Nevertheless,
         * we try to idle all but <np>/4 of the original processes, using a
         * minimum of at least two processes
         */

        int me, np;

        MPI_Comm_size(MPI_COMM_WORLD,&np);
        MPI_Comm_rank(MPI_COMM_WORLD,&me);

        if ( np > 4 ) {
            /* turn off all but every 4th process */
            if ( ( me & 3 ) != 0 ) participate = 0;
        } else
        if ( np > 2 ) {
            /* turn off all but every 2nd process */
            if ( ( me & 1 ) != 0 ) participate = 0;
        }
    }

    /* all spawned processes and selected "root" processes participate */
    if ( participate ) {
        printf("level = %d\n", level);

        /* prepare send buffer */
        sprintf(bufs,"level %d (pid:%d)", level, getpid());

        /* spawn */
        if (level < max_depth) {
            int i, nspawn = 2, errcodes[1];
            MPI_Request req[2];
            MPI_Comm   comm[2];
            char argv1[NCHARS];
            char *args[2];

            /* level 0 spawns only one process to mimic the original test */
            if ( level == 0 ) nspawn = 1;

            /* prepare command line arguments */
            snprintf(argv1, sizeof(argv1), "%d", level+1);
            args[0] = argv1;
            args[1] = NULL;

            /* spawn, with a message sent to and received from each child */
            for ( i = 0; i < nspawn; i++ ) {
                MPI_Comm_spawn(argv[0], args, 1, MPI_INFO_NULL, 0, MPI_COMM_SELF,
                               &comm[i], errcodes);
                MPI_Send(&bufs, sizeof(char)*NCHARS, MPI_CHAR, 0, 100, comm[i]);
                MPI_Irecv(&bufr[i], sizeof(char)*NCHARS, MPI_CHAR, MPI_ANY_SOURCE,
                          MPI_ANY_TAG, comm[i], &req[i]);
            }

            /* wait for messages from children and print them */
            MPI_Waitall(nspawn, req, MPI_STATUSES_IGNORE);
            for ( i = 0; i < nspawn; i++ )
                printf("Child %d sent: %s\n", i, bufr[i]);
        }

        /* send message back to parent */
        if (MPI_COMM_NULL != parent) {
            MPI_Send(&bufs, sizeof(char)*NCHARS, MPI_CHAR, 0, 100, parent);
        }
    }

    /* non-participating processes wait at this barrier for their peers */
    /* (This barrier won't cost that many CPU cycles.) */
    if (MPI_COMM_NULL == parent) {
        nice_barrier(MPI_COMM_WORLD);
    }

    MPI_Finalize();
    return 0;
}
