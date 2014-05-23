/*
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <shmem.h>
#include <stdio.h>

int main (int argc, char * argv[])
{
    static int rbuf = -1;
    int proc, nproc, next;
    int message = 10;

    start_pes(0);
    proc = _my_pe();
    nproc = _num_pes();

    /* Calculate the PE number of the next process in the ring.  Use the
       modulus operator so that the last process "wraps around" to PE 0. */

    next = (proc + 1) % nproc;

    if(proc == 0)
    {
        printf("Process 0 puts message %d to %d (%d processes in ring)\n", message, next, nproc);
        shmem_int_put(&rbuf, &message, 1, next);
    }

    /* Pass the message around the ring.  The exit mechanism works as
       follows: the message (a positive integer) is passed around the
       ring.  Each time it passes PE 0, it is decremented.  When each
       processes receives a message containing a 0 value, it passes the
       message on to the next process and then quits.  By passing the 0
       message first, every process gets the 0 message and can quit
       normally. */

    while(message > 0) {
        shmem_int_wait_until(&rbuf, SHMEM_CMP_EQ, message);
        if(proc == 0) {
            --message;
            printf("Process 0 decremented value: %d\n", message);
        }
        shmem_int_put(&rbuf, &message, 1, next);
        if(proc != 0) {
            --message;
        }
    }

    /* All done */

    printf("Process %d exiting\n", proc);

    return 0;
}
