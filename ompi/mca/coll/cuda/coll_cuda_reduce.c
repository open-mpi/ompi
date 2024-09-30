/*
 * Copyright (c) 2024      NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2004-2015 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2014-2015 NVIDIA Corporation.  All rights reserved.
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
#include "opal/datatype/opal_datatype_cuda.h"

/*
 *	reduce_log_inter
 *
 *	Function:	- reduction using O(N) algorithm
 *	Accepts:	- same as MPI_Reduce()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
mca_coll_cuda_reduce(const void *sbuf, void *rbuf, int count,
                     struct ompi_datatype_t *dtype,
                     struct ompi_op_t *op,
                     int root, struct ompi_communicator_t *comm,
                     mca_coll_base_module_t *module)
{
    mca_coll_cuda_module_t *s = (mca_coll_cuda_module_t*) module;
    int rank = ompi_comm_rank(comm);
    ptrdiff_t gap;
    char *rbuf1 = NULL, *sbuf1 = NULL, *rbuf2 = NULL;
    const char *sbuf2;
    size_t bufsize;
    int rc;

    bufsize = opal_datatype_span(&dtype->super, count, &gap);


    if ((MPI_IN_PLACE != sbuf) && (opal_cuda_check_bufs((char *)sbuf, NULL))) {
        sbuf1 = (char*)malloc(bufsize);
        if (NULL == sbuf1) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        opal_cuda_memcpy_sync(sbuf1, sbuf, bufsize);
        sbuf2 = sbuf; /* save away original buffer */
        sbuf = sbuf1 - gap;
    }

    if ((rank == root) && (opal_cuda_check_bufs((char *)rbuf, NULL))) {
        rbuf1 = (char*)malloc(bufsize);
        if (NULL == rbuf1) {
            if (NULL != sbuf1) free(sbuf1);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        opal_cuda_memcpy_sync(rbuf1, rbuf, bufsize);
        rbuf2 = rbuf; /* save away original buffer */
        rbuf = rbuf1 - gap;
    }
    rc = s->c_coll.coll_reduce((void *) sbuf, rbuf, count,
                               dtype, op, root, comm,
                               s->c_coll.coll_reduce_module);

    if (NULL != sbuf1) {
        free(sbuf1);
    }
    if (NULL != rbuf1) {
        rbuf = rbuf2;
        opal_cuda_memcpy_sync(rbuf, rbuf1, bufsize);
        free(rbuf1);
    }
    return rc;
}

int
mca_coll_cuda_reduce_local(const void *sbuf, void *rbuf, int count,
                           struct ompi_datatype_t *dtype,
                           struct ompi_op_t *op,
                           mca_coll_base_module_t *module)
{
    ptrdiff_t gap;
    char *rbuf1 = NULL, *sbuf1 = NULL, *rbuf2 = NULL;
    size_t bufsize;
    int rc;

    bufsize = opal_datatype_span(&dtype->super, count, &gap);

    if ((MPI_IN_PLACE != sbuf) && (opal_cuda_check_bufs((char *)sbuf, NULL))) {
        sbuf1 = (char*)malloc(bufsize);
        if (NULL == sbuf1) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        opal_cuda_memcpy_sync(sbuf1, sbuf, bufsize);
        sbuf = sbuf1 - gap;
    }

    if (opal_cuda_check_bufs((char *)rbuf, NULL)) {
        rbuf1 = (char*)malloc(bufsize);
        if (NULL == rbuf1) {
            if (NULL != sbuf1) free(sbuf1);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        opal_cuda_memcpy_sync(rbuf1, rbuf, bufsize);
        rbuf2 = rbuf; /* save away original buffer */
        rbuf = rbuf1 - gap;
    }

    ompi_op_reduce(op, (void *)sbuf, rbuf, count, dtype);
    rc = OMPI_SUCCESS;

    if (NULL != sbuf1) {
        free(sbuf1);
    }
    if (NULL != rbuf1) {
        rbuf = rbuf2;
        opal_cuda_memcpy_sync(rbuf, rbuf1, bufsize);
        free(rbuf1);
    }
    return rc;
}
