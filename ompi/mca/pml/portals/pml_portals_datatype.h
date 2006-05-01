/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PML_PORTALS_DATATYPE_H
#define PML_PORTALS_DATATYPE_H


static inline int
ompi_pml_portals_prepare_md_send(ompi_convertor_t *convertor,
                                 ptl_md_t *md,
                                 int *free_after)
{
    struct iovec iov;
    uint32_t iov_count = 1;
    int32_t conv_free_after;
    size_t bufsize;

    ompi_convertor_get_packed_size(convertor, &bufsize);
    iov.iov_len = bufsize;
    if (0 == ompi_convertor_need_buffers(convertor)) {
        iov.iov_base = NULL;
        *free_after = 0;
    } else {
        /* BWB - try to use the iovec option here */

        iov.iov_base = malloc(bufsize);
        if (NULL == iov.iov_base) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        *free_after = 1;
    }

    ompi_convertor_pack(convertor, &iov, &iov_count, &bufsize, 
                        &conv_free_after);

    md->start = iov.iov_base;
    md->length = iov.iov_len;
    md->options = 0;

    return OMPI_SUCCESS;
}


static inline int
ompi_pml_portals_free_md_send(ptl_md_t *md, int free_after)
{
    if (free_after) {
        free(md->start);
    }

    return OMPI_SUCCESS;
}


static inline int
ompi_pml_portals_prepare_md_recv(ompi_convertor_t *convertor,
                                 ptl_md_t *md,
                                 int *free_after)
{
    struct iovec iov;
    size_t bufsize;
    long lb;

    ompi_convertor_get_packed_size(convertor, &bufsize);
    iov.iov_len = bufsize;
    if (0 == ompi_convertor_need_buffers(convertor)) {
        ompi_ddt_type_lb(convertor->pDesc, &lb);
        iov.iov_base = convertor->pBaseBuf + lb + convertor->bConverted;
        *free_after = 0;
    } else {
        /* BWB - try to use the iovec option here */

        iov.iov_base = malloc(bufsize);
        if (NULL == iov.iov_base) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        *free_after = 1;
    }

    md->start = iov.iov_base;
    md->length = iov.iov_len;
    md->options = 0;

    return OMPI_SUCCESS;
}

static inline int
ompi_pml_portals_free_md_recv(ompi_convertor_t *convertor,
                              ptl_md_t *md,
                              int free_after)
{
    uint32_t iov_count = 1;
    size_t max_data;
    struct iovec iov;

    iov.iov_len = md->length;
    iov.iov_base = md->start;

    if (free_after) {
        /* need to unpack into user buffer */
        ompi_convertor_unpack(convertor, &iov, &iov_count, 
                              &max_data, &free_after);
        free(md->start);
    }

    return OMPI_SUCCESS;
}

#endif
