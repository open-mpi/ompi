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
#include "shmem.h"

int main(int argc, char* argv[])
{
    int proc, nproc;

    start_pes(0);
    nproc = _num_pes();
    proc = _my_pe();

    printf("Hello, world, I am %d of %d\n", 
           proc, nproc);

    return 0;
}
