/*
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <stdio.h>
#include <shmem.h>

#warning This application uses deprecated API see http://www.open-mpi.org/

#define SIZE 16

int main(int argc, char* argv[])
{
    short source[SIZE];
    static short target[SIZE];
    int i;
    int num_pe, my_pe;

    start_pes(0);

    num_pe = _num_pes();
    my_pe = _my_pe();

    if (my_pe == 0) {
        /* initialize array */
        for(i = 0; i < SIZE; i++) {
            source[i] = i;
        }
        /* local, not symmetric */
        /* static makes it symmetric */
        /* put "size" words into target on each PE */
        for(i = 1; i < num_pe; i++) {
            shmem_short_put(target, source, SIZE, i);
        }
    }

    shmem_barrier_all(); /* sync sender and receiver */

    if (my_pe != 0) {
        printf("Target on PE %d is \t", my_pe);

        for(i = 0; i < SIZE; i++) {
            printf("%hd \t", target[i]);
        }
        printf("\n");
    }

    shmem_barrier_all(); /* sync before exiting */

    return 0;
}
