/*
 * Copyright (c) 2014-2017 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2014-2015 NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_cuda.h"

#include <stdio.h>

#include "ompi/op/op.h"
#include "opal/datatype/opal_convertor.h"

/*
 *	reduce_scatter_block
 *
 *	Function:	- reduce then scatter
 *	Accepts:	- same as MPI_Reduce_scatter_block()
 *	Returns:	- MPI_SUCCESS or error code
 *
 * Algorithm:
 *     reduce and scatter (needs to be cleaned
 *     up at some point)
 */
int
mca_coll_cuda_reduce_scatter_block(const void *sbuf, void *rbuf, int rcount,
                                   struct ompi_datatype_t *dtype,
                                   struct ompi_op_t *op,
                                   struct ompi_communicator_t *comm,
                                   mca_coll_base_module_t *module)
{
    mca_coll_cuda_module_t *s = (mca_coll_cuda_module_t*) module;
    ptrdiff_t gap;
    char *rbuf1 = NULL, *sbuf1 = NULL, *rbuf2 = NULL;
    size_t sbufsize, rbufsize;
    int rc;

    rbufsize = opal_datatype_span(&dtype->super, rcount, &gap);

    sbufsize = rbufsize * ompi_comm_size(comm);
    rc = mca_coll_cuda_check_buf((void *)sbuf);
    if (rc < 0) {
        return rc;
    }
    if ((MPI_IN_PLACE != sbuf) && (rc > 0)) {
        sbuf1 = (char*)malloc(sbufsize);
        if (NULL == sbuf1) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        mca_coll_cuda_memcpy(sbuf1, sbuf, sbufsize);
        sbuf = sbuf1 - gap;
    }
    rc = mca_coll_cuda_check_buf(rbuf);
    if (rc < 0) {
        return rc;
    }
    if (rc > 0) {
        rbuf1 = (char*)malloc(rbufsize);
        if (NULL == rbuf1) {
            if (NULL != sbuf1) free(sbuf1);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        mca_coll_cuda_memcpy(rbuf1, rbuf, rbufsize);
        rbuf2 = rbuf; /* save away original buffer */
        rbuf = rbuf1 - gap;
    }
    rc = s->c_coll.coll_reduce_scatter_block(sbuf, rbuf, rcount, dtype, op, comm,
                                             s->c_coll.coll_reduce_scatter_block_module);
    if (NULL != sbuf1) {
        free(sbuf1);
    }
    if (NULL != rbuf1) {
        rbuf = rbuf2;
        mca_coll_cuda_memcpy(rbuf, rbuf1, rbufsize);
        free(rbuf1);
    }
    return rc;
}

