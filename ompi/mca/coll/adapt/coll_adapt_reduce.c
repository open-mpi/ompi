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


#include "ompi/op/op.h"
#include "coll_adapt.h"
#include "coll_adapt_algorithms.h"

/* MPI_Reduce and MPI_Ireduce in the ADAPT module only work for commutative operations */
int ompi_coll_adapt_reduce(const void *sbuf, void *rbuf, int count, struct ompi_datatype_t *dtype,
                          struct ompi_op_t *op, int root, struct ompi_communicator_t *comm,
                          mca_coll_base_module_t * module)
{
    /* Fall-back if operation is commutative */
    if (!ompi_op_is_commute(op)){
        mca_coll_adapt_module_t *adapt_module = (mca_coll_adapt_module_t *) module;
        OPAL_OUTPUT_VERBOSE((30, mca_coll_adapt_component.adapt_output,
                    "ADAPT cannot handle reduce with this (commutative) operation. It needs to fall back on another component\n"));
        return adapt_module->previous_reduce(sbuf, rbuf, count, dtype, op, root,
                                             comm,
                                             adapt_module->previous_reduce_module);
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
