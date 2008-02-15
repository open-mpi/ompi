/*
 * Copyright (c) 2007-2008 UT-Battelle, LLC
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file */

#include "ompi_config.h"

#include "ompi/constants.h"
#include "coll_sm2.h"
#include "ompi/op/op.h"


/**
 * Shared memory blocking allreduce.
 */
int mca_coll_sm2_allreduce_intra(void *sbuf, void *rbuf, int count,
                                struct ompi_datatype_t *dtype, 
                                struct ompi_op_t *op,
                                struct ompi_communicator_t *comm,
                                struct mca_coll_base_module_1_1_0_t *module)
{
    /* local variables */
    int rc;
    char *sm_buffer;
    mca_coll_sm2_module_t *sm_module;

    sm_module=(mca_coll_sm2_module_t *) module;

    /* get a pointer to the shared-memory working buffer */
    sm_buffer=alloc_sm2_shared_buffer(sm_module);
    if( NULL == sm_buffer) {
        rc=OMPI_ERR_OUT_OF_RESOURCE;
        goto Error;
    }

    if( 0 != (op->o_flags & OMPI_OP_FLAGS_COMMUTE)) {
        /* Commutative Operation */
    } else {
        /* Non-Commutative Operation */
    }

    /* "free" the shared-memory working buffer */
    rc=alloc_sm2_shared_buffer(sm_module);
    if( OMPI_SUCCESS != rc ) {
        goto Error;
    }

    return OMPI_SUCCESS;

Error:
    return rc;
}
