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

/* MPI_Reduce and MPI_Ireduce in the ADAPT module only work for commutative operations */
int ompi_coll_adapt_reduce(const void *sbuf, void *rbuf, int count, struct ompi_datatype_t *dtype,
                          struct ompi_op_t *op, int root, struct ompi_communicator_t *comm,
                          mca_coll_base_module_t * module)
{
    if (count == 0) {
        return MPI_SUCCESS;
    }
    ompi_request_t *request = NULL;
    int err = ompi_coll_adapt_ireduce(sbuf, rbuf, count, dtype, op, root, comm, &request, module);
    if( MPI_SUCCESS != err ) {
        if( NULL == request )
            return err;
    }
    ompi_request_wait(&request, MPI_STATUS_IGNORE);
    return err;
}
