/*
 * Copyright (c) 2014-2016 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <stdio.h>
#include <shmem.h>

#define SIZE 16

int main(int argc, char* argv[])
{
    short source[SIZE];
    static short target[SIZE];
    int i;
    int num_pe, my_pe;

    shmem_init();

    num_pe = shmem_n_pes();
    my_pe = shmem_my_pe();

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
    shmem_finalize();

    return 0;
}
