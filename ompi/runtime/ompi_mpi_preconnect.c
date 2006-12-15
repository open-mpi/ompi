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
    int i, ret;
    int next,prev;
    struct ompi_request_t * requests[2];
    if(comm_size == 2) {
        if(my_rank){ 
            ret = MCA_PML_CALL(send(MPI_BOTTOM, 0, MPI_BYTE,
                                     0, 1,
                                     MCA_PML_BASE_SEND_SYNCHRONOUS,
                                     MPI_COMM_WORLD));
        } else {
            ret = MCA_PML_CALL(recv(MPI_BOTTOM,0, MPI_BYTE, 1, 
                                    1, MPI_COMM_WORLD, 
                                    MPI_STATUS_IGNORE));
        }   
    } else { 
        for (i = 1; i < comm_size/2; ++i) {
            next = (my_rank + i) % comm_size;
            prev = (my_rank - i + comm_size) % comm_size;
            ret = MCA_PML_CALL(irecv(MPI_BOTTOM,0, MPI_BYTE,
                                     prev, 1,
                                     MPI_COMM_WORLD, 
                                     &requests[0]));
            
            ret = MCA_PML_CALL(isend(MPI_BOTTOM, 0, MPI_BYTE,
                                     next, 1,
                                     MCA_PML_BASE_SEND_STANDARD,
                                     MPI_COMM_WORLD, 
                                     &requests[1]));
            ret = ompi_request_wait_all(2, requests, MPI_STATUSES_IGNORE);
            
        }
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
    }
    
    return ret;
}
    
