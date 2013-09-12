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

#include "orte/runtime/orte_globals.h"

#include "ompi/communicator/communicator.h" /*TODO:  ompi_communicator_t */
#include "ompi/patterns/comm/coll_ops.h" /*TODO:  comm_bcast_pml */

#include "oshmem/constants.h"
#include "oshmem/runtime/runtime.h"
#include "oshmem/runtime/params.h"

OSHMEM_DECLSPEC int oshmem_shmem_exchange_allgather(void *buf,
                                          int buf_size)
{
    int rc = OSHMEM_SUCCESS;
    int i = 0;
    int *ranks_in_comm = NULL;

    ranks_in_comm = (int *) malloc(orte_process_info.num_procs * sizeof(int));
    if (NULL == ranks_in_comm) {
        return OSHMEM_ERR_OUT_OF_RESOURCE;
    }

    for (i = 0; i < (int) orte_process_info.num_procs; ++i) {
        ranks_in_comm[i] = i;
    }
    void* buf_temp = malloc(buf_size);
    memcpy(buf_temp, (char*)buf + buf_size * ORTE_PROC_MY_NAME->vpid, buf_size);

    rc = comm_allgather_pml( buf_temp,
                             buf,
                             buf_size,
                             MPI_BYTE,
                             ORTE_PROC_MY_NAME->vpid,
                             orte_process_info.num_procs,
                             ranks_in_comm,
                             (ompi_communicator_t *) &ompi_mpi_comm_world);

    if (ranks_in_comm)
        free(ranks_in_comm);
    if (buf_temp)
        free(buf_temp);
    return rc;
}

OSHMEM_DECLSPEC int oshmem_shmem_exchange_bcast(void *buf,
                                          int buf_size,
                                          int peer)
{
    int rc = OSHMEM_SUCCESS;
    int i = 0;
    int *ranks_in_comm = NULL;

    ranks_in_comm = (int *) malloc(orte_process_info.num_procs * sizeof(int));
    if (NULL == ranks_in_comm) {
        return OSHMEM_ERR_OUT_OF_RESOURCE;
    }

    for (i = 0; i < (int) orte_process_info.num_procs; ++i) {
        ranks_in_comm[i] = i;
    }
    rc = comm_bcast_pml((void *) buf,
                        peer,
                        buf_size,
                        MPI_BYTE,
                        ORTE_PROC_MY_NAME->vpid,
                        orte_process_info.num_procs,
                        ranks_in_comm,
                        (ompi_communicator_t *) &ompi_mpi_comm_world);
    if (ranks_in_comm)
        free(ranks_in_comm);

    return rc;
}
