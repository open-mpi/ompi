/*
 * Copyright (c) 2014-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "coll_adapt.h"
#include "coll_adapt_algorithms.h"

int ompi_coll_adapt_bcast(void *buff, int count, struct ompi_datatype_t *datatype, int root,
                         struct ompi_communicator_t *comm, mca_coll_base_module_t * module)
{
    ompi_request_t *request = NULL;
    int err = ompi_coll_adapt_ibcast(buff, count, datatype, root, comm, &request, module);
    if( MPI_SUCCESS != err ) {
        if( NULL == request )
            return err;
    }
    ompi_request_wait(&request, MPI_STATUS_IGNORE);
    return err;
}
