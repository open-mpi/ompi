/*
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdlib.h>

#include "ompi/constants.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/communicator/communicator.h"
#include "ompi/request/request.h"
#include "ompi/runtime/mpiruntime.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"

int
ompi_init_preconnect_mpi(void)
{
    int comm_size = ompi_comm_size(MPI_COMM_WORLD);
    int comm_rank =  ompi_comm_rank(MPI_COMM_WORLD);
    int param, value, next, prev, i, ret = OMPI_SUCCESS;
    struct ompi_request_t * requests[2];
    char inbuf[1], outbuf[1];

    param = mca_base_param_find("mpi", NULL, "preconnect_mpi");
    if (OMPI_ERROR == param) return OMPI_SUCCESS;
    ret = mca_base_param_lookup_int(param, &value);
    if (OMPI_SUCCESS != ret) return OMPI_SUCCESS;
    if (0 == value) {
        param = mca_base_param_find("mpi", NULL, "preconnect_all");
        if (OMPI_ERROR == param) return OMPI_SUCCESS;
        ret = mca_base_param_lookup_int(param, &value);
        if (OMPI_SUCCESS != ret) return OMPI_SUCCESS;
    }
    if (0 == value) return OMPI_SUCCESS;

    inbuf[0] = outbuf[0] = '\0';

    /* Each iteration, every process sends to its neighbor i hops to
       the right and receives from its neighbor i hops to the left.
       Because send_complete is used, there will only ever be one
       outstanding send and one outstanding receive in the network at
       a time for any given process.  This limits any "flooding"
       effect that can occur with other connection algorithms.  While
       the flooding algorithms may be a more efficient use of
       resources, they can overwhelm the out-of-band connection system
       used to wire up some networks, leading to poor performance and
       hangs. */
    for (i = 1 ; i <= comm_size / 2 ; ++i) {
        next = (comm_rank + i) % comm_size;
        prev = (comm_rank - i + comm_size) % comm_size;

        ret = MCA_PML_CALL(isend(outbuf, 1, MPI_CHAR,
                                 next, 1,
                                 MCA_PML_BASE_SEND_COMPLETE,
                                 MPI_COMM_WORLD, 
                                 &requests[1]));
        if (OMPI_SUCCESS != ret) return ret;

        ret = MCA_PML_CALL(irecv(inbuf, 1, MPI_CHAR,
                                 prev, 1,
                                 MPI_COMM_WORLD, 
                                 &requests[0]));
        if(OMPI_SUCCESS != ret) return ret;

        ret = ompi_request_wait_all(2, requests, MPI_STATUSES_IGNORE);
        if (OMPI_SUCCESS != ret) return ret;

        ret = MCA_PML_CALL(isend(outbuf, 1, MPI_CHAR,
                                 next, 1,
                                 MCA_PML_BASE_SEND_COMPLETE,
                                 MPI_COMM_WORLD, 
                                 &requests[1]));
        if (OMPI_SUCCESS != ret) return ret;

        ret = MCA_PML_CALL(irecv(inbuf, 1, MPI_CHAR,
                                 prev, 1,
                                 MPI_COMM_WORLD, 
                                 &requests[0]));
        if(OMPI_SUCCESS != ret) return ret;

        ret = ompi_request_wait_all(2, requests, MPI_STATUSES_IGNORE);
        if (OMPI_SUCCESS != ret) return ret;
    }

    return ret;
}

    
int
ompi_init_preconnect_oob(void)
{
    size_t world_size, next, prev, i, j, world_rank;
    ompi_proc_t **procs;
    int ret, simultaneous, param, value = 0;
    struct iovec inmsg[1], outmsg[1];

    param = mca_base_param_find("mpi", NULL, "preconnect_oob");
    if (OMPI_ERROR == param) return OMPI_SUCCESS;
    ret = mca_base_param_lookup_int(param, &value);
    if (OMPI_SUCCESS != ret) return OMPI_SUCCESS;
    if (0 == value) {
        param = mca_base_param_find("mpi", NULL, "preconnect_all");
        if (OMPI_ERROR == param) return OMPI_SUCCESS;
        ret = mca_base_param_lookup_int(param, &value);
        if (OMPI_SUCCESS != ret) return OMPI_SUCCESS;
    }
    if (0 == value) return OMPI_SUCCESS;

    param = mca_base_param_find("mpi", NULL, "preconnect_oob_simultaneous");
    if (OMPI_ERROR == param) return OMPI_SUCCESS;
    ret = mca_base_param_lookup_int(param, &value);
    if (OMPI_SUCCESS != ret) return OMPI_SUCCESS;
    simultaneous = (value < 1) ? 1 : value;

    procs = ompi_proc_world(&world_size);

    inmsg[0].iov_base = outmsg[0].iov_base = NULL;
    inmsg[0].iov_len = outmsg[0].iov_len = 0;

    /* proc_world and ompi_comm_world should have the same proc list... */
    if ((int) world_size != ompi_comm_size(MPI_COMM_WORLD)) {
        return OMPI_ERR_NOT_FOUND;
    } else if (ompi_proc_local() !=
               procs[ompi_comm_rank(MPI_COMM_WORLD)]) {
        return OMPI_ERR_NOT_FOUND;
    }
    world_rank = (size_t) ompi_comm_rank(MPI_COMM_WORLD);

    /* Each iteration, every process sends to its neighbor i hops to
       the right and receives from its neighbor i hops to the left.
       This limits any "flooding" effect that can occur with other
       connection algorithms, which can overwhelm the out-of-band
       connection system, leading to poor performance and hangs. */
    for (i = 1 ; i <= world_size / 2 ; i += simultaneous) {
        for (j = 0 ; j < (size_t) simultaneous ; ++j) {
            next = (world_rank + (i + j )) % world_size;
                    
            /* sends do not wait for a match */
            ret = orte_rml.send(&procs[next]->proc_name,
                                outmsg,
                                1,
                                ORTE_RML_TAG_WIREUP,
                                0);
            if (ret < 0) return ret;
        }
        for (j = 0 ; j < (size_t) simultaneous ; ++j) {
            prev = (world_rank - (i + j) + world_size) % world_size;
                    
            ret = orte_rml.recv(&procs[prev]->proc_name,
                                inmsg,
                                1,
                                ORTE_RML_TAG_WIREUP,
                                0);
            if (ret < 0) return ret;
        }
    }
    
    return OMPI_SUCCESS;
}
    
