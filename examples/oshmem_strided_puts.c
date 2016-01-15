/*
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * This program is an adaptation of examples found in the man pages
 * of SGI’s SHMEM implementation.
 *
 * In this program, iput is used to select 5 elements from array source separated by
 * a stride of 2 and write them to array target using a stride of 1.
 *
 * Given the array source = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 }
 * iput will select 5 elements from array source on PE 0, using a stride of 2:
 *
 * selected elements = { 1, 3, 5, 7, 9 }
 *
 * These elements will then be written to the array source on PE 1 using a stride of 1:
 *
 * target = { 1, 3, 5, 7, 9 }
 *
 */

#include <stdio.h>
#include <shmem.h>

#warning This application uses deprecated API see http://www.open-mpi.org/

int main(void)
{
    short source[10] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
    static short target[10];
    int me;

    start_pes(0);
    me = _my_pe();

    if (me == 0) {
        /* put 10 words into target on PE 1 */
        shmem_short_iput(target, source, 1, 2, 5, 1);
    }

    shmem_barrier_all(); /* sync sender and receiver */

    if (me == 1) {
        printf("target on PE %d is %hd %hd %hd %hd %hd\n", me,
        target[0], target[1], target[2],
        target[3], target[4] );
    }
    shmem_barrier_all(); /* sync before exiting */

    return 0;
}
