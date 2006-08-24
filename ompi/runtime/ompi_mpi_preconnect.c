/*
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdlib.h>

#include "ompi/mca/pml/pml.h"
#include "ompi/communicator/communicator.h"
#include "ompi/request/request.h"
#include "ompi/runtime/mpiruntime.h"

/* 
 * do zero byte IRECV / ISEND: upper half sends to lower half (i.e. do
 * a ping, not a ping pong)
 */ 
int ompi_init_do_preconnect(void)
{
    int comm_size = ompi_comm_size(MPI_COMM_WORLD);
    int my_rank =  ompi_comm_rank(MPI_COMM_WORLD);
    int i, j, ret;
    struct ompi_request_t **requests; 

    requests = (ompi_request_t**)malloc(comm_size * sizeof(struct ompi_request_t *));
    if (NULL == requests) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for (i = j = 0; i < comm_size; ++i) {
        if (i == my_rank) {
            continue;
        } else if (my_rank < i) {
            ret = MCA_PML_CALL(isend(MPI_BOTTOM, 0, MPI_BYTE,
                                     i, 1,
                                     MCA_PML_BASE_SEND_STANDARD,
                                     MPI_COMM_WORLD,
                                     &requests[j++]));
        } else {
            ret = MCA_PML_CALL(irecv(MPI_BOTTOM,0, MPI_BYTE, i, 
                                     1, MPI_COMM_WORLD, 
                                     &requests[j++]));
        }
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
    }
    ret = ompi_request_wait_all(j, requests, MPI_STATUSES_IGNORE);
    free(requests);

    return ret;
}
    
