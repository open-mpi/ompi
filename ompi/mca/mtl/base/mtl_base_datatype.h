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
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mca/mca.h"
#include "ompi/mca/mtl/mtl.h"
#include "ompi/mca/mtl/base/base.h"
#include "ompi/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/datatype/opal_datatype_internal.h"
#include "opal/mca/accelerator/accelerator.h"

#ifndef MTL_BASE_DATATYPE_H_INCLUDED
#define MTL_BASE_DATATYPE_H_INCLUDED

__opal_attribute_always_inline__ static inline int
ompi_mtl_datatype_pack(struct opal_convertor_t *convertor,
                       void **buffer,
                       size_t *buffer_len,
                       bool *free_after)
{
    struct iovec iov;
    uint32_t iov_count = 1;
    bool is_accelerator = opal_convertor_on_device(convertor);
#if !(OPAL_ENABLE_HETEROGENEOUS_SUPPORT)
    if (convertor->pDesc &&
	!(convertor->flags & CONVERTOR_COMPLETED) &&
	opal_datatype_is_contiguous_memory_layout(convertor->pDesc,
						  convertor->count) &&
        !is_accelerator) {
	    *free_after = false;
	    *buffer = convertor->pBaseBuf + convertor->bConverted + convertor->pDesc->true_lb;
	    *buffer_len = convertor->local_size;
	    return OPAL_SUCCESS;
    }
#endif

    opal_convertor_get_packed_size(convertor, buffer_len);
    *free_after = false;
    if( 0 == *buffer_len ) {
        *buffer = NULL;
        return OMPI_SUCCESS;
    }
    iov.iov_len = *buffer_len;
    iov.iov_base = NULL;
    /* If we need buffers or we don't have accelerator support and it is a device buffer, we will need to copy */
    if (opal_convertor_need_buffers(convertor) || (is_accelerator && false == ompi_mtl_base_selected_component->accelerator_support)) {
        /* If this is a host buffer or an accelerator buffer and we don't have accelerator support we need to host malloc */
        if (!is_accelerator || (is_accelerator && false == ompi_mtl_base_selected_component->accelerator_support)) {
            iov.iov_base = malloc(*buffer_len);
        /* The remaining case is if this is an accelerator buffer and we have accelerator support and we need buffers. We will need to do an accelerator malloc*/
        } else {
            opal_accelerator.mem_alloc(MCA_ACCELERATOR_NO_DEVICE_ID, &iov.iov_base, *buffer_len);
        }

        if (NULL == iov.iov_base) return OMPI_ERR_OUT_OF_RESOURCE;
        *free_after = true;
    }

    opal_convertor_pack(convertor, &iov, &iov_count, buffer_len);

    *buffer = iov.iov_base;
    return OMPI_SUCCESS;
}

__opal_attribute_always_inline__ static inline int
ompi_mtl_datatype_recv_buf(struct opal_convertor_t *convertor,
                           void **buffer,
                           size_t *buffer_len,
                           bool *free_on_error)
{
    opal_convertor_get_packed_size(convertor, buffer_len);
    *free_on_error = false;
    *buffer = NULL;
    if( 0 == *buffer_len ) {
        *buffer_len = 0;
        return OMPI_SUCCESS;
    }
    bool is_accelerator = opal_convertor_on_device(convertor);;

    /* If we need buffers or we don't have accelerator support and it is a device buffer, we will need to copy */
    if (opal_convertor_need_buffers(convertor) || (is_accelerator && false == ompi_mtl_base_selected_component->accelerator_support)) {
        /* If this is a host buffer or an accelerator buffer and we don't have accelerator support we need to host malloc */
        if (!is_accelerator || (is_accelerator && false == ompi_mtl_base_selected_component->accelerator_support)) {
            *buffer = malloc(*buffer_len);
        /* The remaining case is if this is an accelerator buffer and we have accelerator support and we need buffers. Wwe will need to do an accelerator malloc*/
        } else {
            opal_accelerator.mem_alloc(MCA_ACCELERATOR_NO_DEVICE_ID, buffer, *buffer_len);
        }

        if (NULL == *buffer) return OMPI_ERR_OUT_OF_RESOURCE;
        *free_on_error = true;
    } else {
        *buffer = convertor->pBaseBuf +
            convertor->use_desc->desc[convertor->use_desc->used].end_loop.first_elem_disp;
    }
    return OMPI_SUCCESS;
}

__opal_attribute_always_inline__ static inline int
ompi_mtl_datatype_unpack(struct opal_convertor_t *convertor,
                         void *buffer,
                         size_t buffer_len)
{
    struct iovec iov;
    uint32_t iov_count = 1;
    bool is_accelerator = opal_convertor_on_device(convertor);

    /* If the buffer length is greater than 0 and we allocated buffers previously, we need to unpack them */
    if (buffer_len > 0 && (opal_convertor_need_buffers(convertor) || (is_accelerator && false == ompi_mtl_base_selected_component->accelerator_support))) {
        iov.iov_len = buffer_len;
        iov.iov_base = buffer;

        opal_convertor_unpack(convertor, &iov, &iov_count, &buffer_len);
        /* If it's an accelerator buffer and we have accelerator support, we will need to free the accelerator buffer */
        if (is_accelerator && true == ompi_mtl_base_selected_component->accelerator_support) {
            opal_accelerator.mem_release(MCA_ACCELERATOR_NO_DEVICE_ID, buffer);
        /* If it's not an accelerator buffer or we don't have accelerator support, we will need to free the host buffer */
        } else {
            free(buffer);
        }
    }

    return OMPI_SUCCESS;
}

#endif /* MTL_BASE_DATATYPE_H_INCLUDED */
