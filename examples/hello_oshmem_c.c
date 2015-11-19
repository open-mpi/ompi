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
    char name[SHMEM_MAX_NAME_LEN];
    int major, minor;

    shmem_init();
    nproc = shmem_n_pes();
    proc = shmem_my_pe();
    shmem_info_get_name(name);
    shmem_info_get_version(&major, &minor);

    printf("Hello, world, I am %d of %d: %s (version: %d.%d)\n",
           proc, nproc, name, major, minor);
    shmem_finalize();

    return 0;
}
