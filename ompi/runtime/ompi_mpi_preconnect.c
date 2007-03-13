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

#include "ompi/mca/pml/pml.h"
#include "ompi/communicator/communicator.h"
#include "ompi/request/request.h"
#include "ompi/runtime/mpiruntime.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"

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
    ret = OMPI_SUCCESS;
    if(comm_size == 2) {
        if(my_rank){ 
            ret = MCA_PML_CALL(send(MPI_BOTTOM, 0, MPI_BYTE,
                                    0, 1,
                                    MCA_PML_BASE_SEND_STANDARD,
                                     MPI_COMM_WORLD));
            if(OMPI_SUCCESS != ret) {
                return ret;
            }
        } else {
            ret = MCA_PML_CALL(recv(MPI_BOTTOM,0, MPI_BYTE, 1, 
                                    1, MPI_COMM_WORLD, 
                                    MPI_STATUS_IGNORE));
            if(OMPI_SUCCESS != ret) {
                return ret;
            }
        }   
    } else { 
        for (i = 1; i <= comm_size/2; ++i) {
            next = (my_rank + i) % comm_size;
            prev = (my_rank - i + comm_size) % comm_size;
            ret = MCA_PML_CALL(irecv(MPI_BOTTOM,0, MPI_BYTE,
                                     prev, 1,
                                     MPI_COMM_WORLD, 
                                     &requests[0]));
            if(OMPI_SUCCESS != ret) {
                return ret;
            }
            ret = MCA_PML_CALL(isend(MPI_BOTTOM, 0, MPI_BYTE,
                                     next, 1,
                                     MCA_PML_BASE_SEND_STANDARD,
                                     MPI_COMM_WORLD, 
                                     &requests[1]));
            if (OMPI_SUCCESS != ret) {
                return ret;
            }
            ret = ompi_request_wait_all(2, requests, MPI_STATUSES_IGNORE);
            if (OMPI_SUCCESS != ret) {
                return ret;
            }
            
        }
        
    }
    
    return ret;
}

    
int ompi_init_do_oob_preconnect(void)
{
    size_t world_size, i, next, prev, my_index = 0;
    ompi_proc_t **procs;
    int ret;
    struct iovec msg[1];

    procs = ompi_proc_world(&world_size);

    msg[0].iov_base = NULL;
    msg[0].iov_len = 0;

    if (world_size == 2) {
        if (ompi_proc_local() == procs[0]) {
            ret = orte_rml.send(&procs[1]->proc_name,
                                msg,
                                1,
                                ORTE_RML_TAG_WIREUP,
                                0);
            if (ret < 0) return ret;
        } else {
            ret = orte_rml.recv(&procs[0]->proc_name,
                                msg,
                                1,
                                ORTE_RML_TAG_WIREUP,
                                0);
            if (ret < 0) return ret;
        }
    } else if (world_size > 2) {
        for (i = 0 ; i < world_size ; ++i) {
            if (ompi_proc_local() == procs[i]) {
                my_index = i;
                /* Do all required preconnections */
                for (i = 1 ; i <= world_size / 2 ; ++i) {
                    next = (my_index + i) % world_size;
                    prev = (my_index - i + world_size) % world_size;
                    
                    /* sends do not wait for a match */
                    ret = orte_rml.send(&procs[next]->proc_name,
                                        msg,
                                        1,
                                        ORTE_RML_TAG_WIREUP,
                                        0);
                    if (ret < 0) return ret;
                    
                    ret = orte_rml.recv(&procs[prev]->proc_name,
                                        msg,
                                        1,
                                        ORTE_RML_TAG_WIREUP,
                                        0);
                    if (ret < 0) return ret;
                }
                break;
            }
        }
    }
    
    return OMPI_SUCCESS;
}
    
