/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2024 Bull SAS.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file pml_ubcl_datatype.c
 *
 * PML/UBCL datatype and convertor related functions
 *
 */

#include "ompi/mca/pml/ubcl/pml_ubcl.h"
#include "ompi/mca/pml/ubcl/pml_ubcl_utils.h"
#include "ompi/mca/pml/ubcl/pml_ubcl_request.h"

size_t pml_ubcl_datatype_pack(void *pack_buf, const void *usr_handle, size_t pack_size,
                              size_t offset)
{
    opal_convertor_t *convertor = (opal_convertor_t *) usr_handle;

    /* Set input data size and start pointer. */
    uint32_t iov_count = 1;
    int ret = 0;
    struct iovec iov;
    iov.iov_len = pack_size;
    iov.iov_base = (IOVBASE_TYPE *) pack_buf;

    opal_convertor_set_position(convertor, &offset);

    /* Pack data from converter to iov */
    ret = opal_convertor_pack(convertor, &iov, &iov_count, &pack_size);
    if (-1 == ret) {
        mca_pml_ubcl_error(ret, "opal_convertor_unpack failed\n");
    }

    return pack_size;
}

size_t pml_ubcl_datatype_unpack(void *usr_handle, const void *pack_buf, size_t pack_size,
                                size_t offset)
{
    opal_convertor_t *convertor = (opal_convertor_t *) usr_handle;

    /* Set input data size and start pointer. */
    uint32_t iov_count = 1;
    int ret = 0;
    struct iovec iov;
    iov.iov_len = pack_size;
    iov.iov_base = (IOVBASE_TYPE *) pack_buf;

    opal_convertor_set_position(convertor, &offset);

    /* Pack data from converter to iov */
    ret = opal_convertor_unpack(convertor, &iov, &iov_count, &pack_size);
    if (-1 == ret) {
        mca_pml_ubcl_error(ret, "opal_convertor_unpack failed\n");
    }

    return pack_size;
}

size_t pml_ubcl_datatype_mem_size(const void *usr_handle, size_t offset)
{
    opal_convertor_t *convertor = (opal_convertor_t *) usr_handle;
    size_t size = 0;

    opal_datatype_type_size(convertor->pDesc, &size);

    if (offset > size * convertor->count) {
        return 0;
    }

    return size * convertor->count - offset;
}

void pml_ubcl_datatype_finish(void *usr_handle)
{
    /*
     * Does nothing
     */

    return;
}
