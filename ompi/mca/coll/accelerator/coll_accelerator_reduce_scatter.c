/*
 * Copyright (c) 2014-2017 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2014-2015 NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * Copyright (c) 2024      Triad National Security, LLC. All rights reserved.
 * Copyright (c) 2024      Advanced Micro Devices, Inc. All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_accelerator.h"

#include <stdio.h>

#include "ompi/op/op.h"
#include "opal/datatype/opal_convertor.h"

/*
 *	reduce_scatter
 *
 *	Function:	- reduce_scatter for device buffers through temp. CPU buffer
 *	Accepts:	- same as MPI_Reduce_scatter()
 *	Returns:	- MPI_SUCCESS or error code
 *
 * Algorithm:
 *     reduce and scatter (needs to be cleaned
 *     up at some point)
 */
int
mca_coll_accelerator_reduce_scatter(const void *sbuf, void *rbuf, ompi_count_array_t rcounts,
                                   struct ompi_datatype_t *dtype,
                                   struct ompi_op_t *op,
                                   struct ompi_communicator_t *comm,
                                   mca_coll_base_module_t *module)
{
    mca_coll_accelerator_module_t *s = (mca_coll_accelerator_module_t*) module;
    ptrdiff_t gap;
    char *rbuf1 = NULL, *sbuf1 = NULL, *rbuf2 = NULL;
    int sbuf_dev, rbuf_dev;
    size_t sbufsize, rbufsize, elemsize;
    int rc, i;
    int comm_size = ompi_comm_size(comm);
    int total_count = 0;
    
    elemsize = opal_datatype_span(&dtype->super, 1, &gap);
    for (i = 0; i < comm_size; i++) {
	total_count += ompi_count_array_get(rcounts, i);
    }
    sbufsize = elemsize * total_count;

    rc = mca_coll_accelerator_check_buf((void *)sbuf, &sbuf_dev);
    if (0 > rc) {
        return rc;
    }
    if ((MPI_IN_PLACE != sbuf) && (0 < rc)) {
        sbuf1 = (char*)malloc(sbufsize);
        if (NULL == sbuf1) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        mca_coll_accelerator_memcpy(sbuf1, MCA_ACCELERATOR_NO_DEVICE_ID, sbuf, sbuf_dev, sbufsize,
                                    MCA_ACCELERATOR_TRANSFER_DTOH);
        sbuf = sbuf1 - gap;
    }

    rc = mca_coll_accelerator_check_buf(rbuf, &rbuf_dev);
    if (0 > rc) {
        goto exit;
    }
    rbufsize = elemsize * ompi_count_array_get(rcounts, ompi_comm_rank(comm));
    if (0 < rc) {
        rbuf1 = (char*)malloc(rbufsize);
        if (NULL == rbuf1) {
            rc = OMPI_ERR_OUT_OF_RESOURCE;
	    goto exit;
        }
        mca_coll_accelerator_memcpy(rbuf1, MCA_ACCELERATOR_NO_DEVICE_ID, rbuf, rbuf_dev, rbufsize,
                                    MCA_ACCELERATOR_TRANSFER_DTOH);
        rbuf2 = rbuf; /* save away original buffer */
        rbuf = rbuf1 - gap;
    }
    rc = s->c_coll.coll_reduce_scatter(sbuf, rbuf, rcounts, dtype, op, comm,
                                       s->c_coll.coll_reduce_scatter_block_module);
    if (0 > rc) {
        goto exit;
    }

    if (NULL != rbuf1) {
        mca_coll_accelerator_memcpy(rbuf2, rbuf_dev, rbuf1, MCA_ACCELERATOR_NO_DEVICE_ID, rbufsize,
                                    MCA_ACCELERATOR_TRANSFER_HTOD);
    }

 exit:
    if (NULL != sbuf1) {
        free(sbuf1);
    }
    if (NULL != rbuf1) {
        free(rbuf1);
    }

    return rc;
}

