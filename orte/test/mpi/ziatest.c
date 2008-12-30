/* -*- C -*-
 *
 * Copyright (c) 2008 Los Alamos National Security, LLC.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <time.h>

#include <mpi.h>

int main(int argc, char* argv[])
{
    int msg;
    int rank, size, my_twin;
    int ppn, my_node;
    struct timeval tv;
    long my_timestamp;
    long *timestamps;
    int i, maxrank;
    long maxtime, minutes, seconds;
    long start_sec, start_msec;
    float fsecs;
    
    if (argc < 4) {
        fprintf(stderr, "a ppn value must be provided\n");
        return 1;
    }
    ppn = strtol(argv[1], NULL, 10);
    start_sec = strtol(argv[2], NULL, 10);
    start_msec = strtol(argv[3], NULL, 10);
    
    MPI_Init(NULL, NULL);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    /* this program requires that the size be an integer multiple of ppn */
    if (0 != (size % ppn)) {
        if (0 == rank) {
            fprintf(stderr, "The number of procs must be an integer multiple of the ppn\n"
                    "Given: num_procs %d ppn %d\n", size, ppn);
            MPI_Abort(MPI_COMM_WORLD, 1);
        } else {
            goto cleanup;
        }
    }
    
    /* compute the rank of the rank with which I am to exchange a message.
     * Per requirements, this proc must be on another node. To accomplish
     * this with max efficiency, we take advantage of knowing that the ppn
     * on every node will be the same. We therefore pair up the nodes, and
     * pair up the procs on each node, so that only one connection is setup
     * for each proc. We also want to ensure that the node pairs are
     * "neighboring" - i.e., that they hopefully share a switch so that the
     * hop count of sending the messages is minimized.
     */
    
    /* first, determine if my node is odd or even */
    my_node = rank / ppn;
    
     if (0 != (my_node % 2)) {
        /* compute my twin's rank - as I am an odd numbered node, my
         * twin will be on the node below me. Thus, its rank will be
         * my rank - ppn
         */
        my_twin = rank - ppn;
        /* if I am an odd numbered node, then I will receive first */
        MPI_Recv(&msg, 1, MPI_INT, my_twin, 1, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        /* receive the return message so that we meet the stated requirement
         * that -every- proc send a message
         */
        MPI_Send(&msg, 1, MPI_INT, my_twin, 1, MPI_COMM_WORLD);
    } else {
        /* compute my twin's rank - as I am an even numbered node, my
         * twin will be on the node above me. Thus, its rank will be
         * my rank + ppn
         */
        my_twin = rank + ppn;
        /* I am an even numbered node, so I send first */
        MPI_Send(&msg, 1, MPI_INT, my_twin, 1, MPI_COMM_WORLD);
        /* now receive the reply so my twin also meets the requirement */
        MPI_Recv(&msg, 1, MPI_INT, my_twin, 1, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
    }
    
    /* get a completion time stamp */
    gettimeofday(&tv, NULL);
    my_timestamp = tv.tv_sec*1000000 + tv.tv_usec;
    
    /* THIS COMPLETES THE OFFICIAL TIMING POINT */

    /* Gather to get all the timestamps to rank 0 */
    timestamps = NULL;
    if (0 == rank) {
        timestamps = malloc(size * sizeof(long));
        if (NULL == timestamps) {
            MPI_Abort(MPI_COMM_WORLD, 1);
        }
    }
    MPI_Gather(&my_timestamp, 1, MPI_LONG,
               timestamps, 1, MPI_LONG, 0, MPI_COMM_WORLD);
    if (0 == rank) {
        /* The "timestamps" array will now have everyone's timestamp
         (i.e., rank 0's timestamp will be in pos 0,, rank 1's timestamp
         will be in 1, ...etc. */
        /* find the maximum timestamp */
        maxtime = -1;
        maxrank = -1;
        for (i=0; i < size; i++) {
            if (timestamps[i] > maxtime) {
                maxtime = timestamps[i];
                maxrank = i;
            }
        }
        free(timestamps);
        /* subtract starting time to get time in microsecs for test */
        maxtime = maxtime - (start_sec*1000000 + start_msec);
        /* pretty-print the result */
        seconds = maxtime / 1000000;
        minutes = seconds / 60;
        seconds = seconds % 60;
        if (0 == minutes && 0 == seconds) {
            fsecs = (float)(maxtime) / 1000.0;
            fprintf(stderr, "Time test was completed in %3.2f millisecs\nSlowest rank: %d\n",
                    fsecs, maxrank);
        } else {
            fprintf(stderr, "Time test was completed in %3ld:%02ld min:sec\nSlowest rank: %d\n",
                    minutes, seconds, maxrank);
        }
    }
    
cleanup:
    /* this completes the test */
    MPI_Finalize();
    
    return 0;
}
