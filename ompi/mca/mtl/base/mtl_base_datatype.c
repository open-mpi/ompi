/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/mca/mca.h"
#include "ompi/mca/mtl/mtl.h"
#include "ompi/mca/mtl/base/base.h"
#include "ompi/constants.h"
#include "ompi/datatype/convertor.h"

int
ompi_mtl_datatype_pack(struct ompi_convertor_t *convertor,
                       void **buffer,
                       size_t *buffer_len,
                       bool *freeAfter)
{
    struct iovec iov;
    uint32_t iov_count = 1;
    int32_t free_after;
    size_t max_data;

    ompi_convertor_get_packed_size(convertor, &max_data);
    iov.iov_len = max_data;

    if (max_data > 0 && ompi_convertor_need_buffers(convertor)) {
        iov.iov_base = malloc(max_data);
        if (NULL == iov.iov_base) return OMPI_ERR_OUT_OF_RESOURCE;
        *freeAfter = true;
    } else {
        iov.iov_base = NULL;
        *freeAfter = false;
    }

    ompi_convertor_pack(convertor, &iov, &iov_count, &max_data,
                        &free_after);

    *buffer = iov.iov_base;
    *buffer_len = iov.iov_len;

    return OMPI_SUCCESS;
}


int
ompi_mtl_datatype_recv_buf(struct ompi_convertor_t *convertor,
                           void ** buffer,
                           size_t *buffer_len,
                           bool *free_on_error)
{
    size_t max_data;
    long lb;

    ompi_convertor_get_packed_size(convertor, &max_data);

    if (max_data > 0 && ompi_convertor_need_buffers(convertor)) {
        *buffer = malloc(max_data);
        *free_on_error = true;
    } else {
        ompi_ddt_type_lb(convertor->pDesc, &lb);
        *buffer = convertor->pBaseBuf + lb;
        *free_on_error = false;
    }

    *buffer_len = max_data;

    return OMPI_SUCCESS;
}


int
ompi_mtl_datatype_unpack(struct ompi_convertor_t *convertor,
                         void *buffer,
                         size_t buffer_len)
{
    struct iovec iov;
    uint32_t iov_count = 1;
    int32_t free_after;
    size_t max_data;

    iov.iov_len = buffer_len;
    iov.iov_base = buffer;
    max_data = iov.iov_len;

    
    ompi_convertor_unpack(convertor, &iov, &iov_count,
                          &max_data, &free_after);

    if (max_data > 0 && ompi_convertor_need_buffers(convertor)) {
        free(buffer);
    }

    return OMPI_ERROR;
}
