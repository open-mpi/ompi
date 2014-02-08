/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "oshmem_config.h"

#include "oshmem/constants.h"
#include "oshmem/runtime/runtime.h"
#include "oshmem/runtime/params.h"

int oshmem_shmem_allgather(void *send_buf, void *rcv_buf, int elem_size)
{
    int rc;

    rc = MPI_Allgather(send_buf, elem_size, MPI_BYTE,
                        rcv_buf, elem_size, MPI_BYTE, oshmem_comm_world);

    return rc;
}

void oshmem_shmem_barrier(void)
{
    MPI_Barrier(oshmem_comm_world);
}
