/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
 
/*
 * DSS Buffer Operations
 */
#include "opal_config.h"

#include "opal/util/error.h"

#include "opal/dss/dss_internal.h"


int opal_dss_unload(opal_buffer_t *buffer, void **payload,
                    int32_t *bytes_used)
{
    /* check that buffer is not null */
    if (!buffer) {
        return OPAL_ERR_BAD_PARAM;
    }

    /* were we given someplace to point to the payload */
    if (NULL == payload) {
        return OPAL_ERR_BAD_PARAM;
    }
    
    /* anything in the buffer - if not, nothing to do */
    if (NULL == buffer->base_ptr || 0 == buffer->bytes_used) {
        *payload = NULL;
        *bytes_used = 0;
        return OPAL_SUCCESS;
    }

    /* okay, we have something to provide - pass it back */
    *payload = buffer->base_ptr;
    *bytes_used = buffer->bytes_used;
    
    /* dereference everything in buffer */
    buffer->base_ptr = NULL;
    buffer->pack_ptr = buffer->unpack_ptr = NULL;
    buffer->bytes_allocated = buffer->bytes_used = 0;

    /* All done */

    return OPAL_SUCCESS;
}


int opal_dss_load(opal_buffer_t *buffer, void *payload,
                  int32_t bytes_used)
{
    /* check to see if the buffer has been initialized */
    if (NULL == buffer) {
        return OPAL_ERR_BAD_PARAM;
    }
    
    /* check if buffer already has payload - free it if so */
    if (NULL != buffer->base_ptr) {
        free(buffer->base_ptr);
    }

    /* populate the buffer */
    buffer->base_ptr = (char*)payload; 

    /* set pack/unpack pointers */
    buffer->pack_ptr = ((char*)buffer->base_ptr) + bytes_used; 
    buffer->unpack_ptr = buffer->base_ptr;

    /* set counts for size and space */
    buffer->bytes_allocated = buffer->bytes_used = bytes_used;

    /* All done */

    return OPAL_SUCCESS;    
}


/* Copy the UNPACKED portion of a source buffer into a destination buffer
 * The complete contents of the src buffer are NOT copied - only that
 * portion that has not been previously unpacked is copied.
 */
int opal_dss_copy_payload(opal_buffer_t *dest, opal_buffer_t *src)
{
    char *dst_ptr;
    int32_t bytes_left;

    /* ensure we have valid source and destination */
    if (NULL == dest || NULL == src) {
        return OPAL_ERR_BAD_PARAM;
    }
    
    /* if the dest is already populated, check to ensure that both
     * source and dest are of the same buffer type
     */
    if (0 != dest->bytes_used) {
        if (dest->type != src->type) {
            return OPAL_ERR_BUFFER;
        }
    }
    
    /* either the dest was empty or the two types already match -
     * either way, just ensure the two types DO match
     */
    dest->type = src->type;
    
    /* compute how much of the src buffer remains unpacked
     * buffer->bytes_used is the total number of bytes in the buffer that
     * have been packed. However, we may have already unpacked some of
     * that data. We only want to unload what remains unpacked. This
     * means we have to look at how much of the buffer remains "used"
     * beyond the unpack_ptr
     */
    bytes_left = src->bytes_used - (src->unpack_ptr - src->base_ptr);
    
    /* if nothing is left, then nothing to do */
    if (0 == bytes_left) {
        return OPAL_SUCCESS;
    }
    
    /* add room to the dest for the src buffer's payload */
    if (NULL == (dst_ptr = opal_dss_buffer_extend(dest, bytes_left))) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    
    /* copy the src payload to the specified location in dest */
    memcpy(dst_ptr, src->unpack_ptr, bytes_left);
    
    /* adjust the dest buffer's bookkeeping */
    dest->bytes_used += bytes_left;
    dest->pack_ptr = ((char*)dest->pack_ptr) + bytes_left;
    
    return OPAL_SUCCESS;
}

int opal_value_load(opal_value_t *kv,
                    void *data, opal_data_type_t type)
{
    opal_byte_object_t *boptr;

    switch (type) {
    case OPAL_STRING:
        kv->type = OPAL_STRING;
        if (NULL != data) {
            kv->data.string = strdup( (const char *) data);
        } else {
            kv->data.string = NULL;
        }
        break;
    case OPAL_UINT64:
        if (NULL == data) {
            OPAL_ERROR_LOG(OPAL_ERR_BAD_PARAM);
            return OPAL_ERR_BAD_PARAM;
        }
        kv->type = OPAL_UINT64;
        kv->data.uint64 = *(uint64_t*)(data);
        break;
    case OPAL_UINT32:
        if (NULL == data) {
            OPAL_ERROR_LOG(OPAL_ERR_BAD_PARAM);
            return OPAL_ERR_BAD_PARAM;
        }
        kv->type = OPAL_UINT32;
        kv->data.uint32 = *(uint32_t*)data;
        break;
    case OPAL_UINT16:
        if (NULL == data) {
            OPAL_ERROR_LOG(OPAL_ERR_BAD_PARAM);
            return OPAL_ERR_BAD_PARAM;
        }
        kv->type = OPAL_UINT16;
        kv->data.uint16 = *(uint16_t*)(data);
        break;
    case OPAL_INT:
        if (NULL == data) {
            OPAL_ERROR_LOG(OPAL_ERR_BAD_PARAM);
            return OPAL_ERR_BAD_PARAM;
        }
        kv->type = OPAL_INT;
        kv->data.integer = *(int*)(data);
        break;
    case OPAL_UINT:
        if (NULL == data) {
            OPAL_ERROR_LOG(OPAL_ERR_BAD_PARAM);
            return OPAL_ERR_BAD_PARAM;
        }
        kv->type = OPAL_UINT;
        kv->data.uint = *(unsigned int*)(data);
        break;
    case OPAL_FLOAT:
        if (NULL == data) {
            OPAL_ERROR_LOG(OPAL_ERR_BAD_PARAM);
            return OPAL_ERR_BAD_PARAM;
        }
        kv->type = OPAL_FLOAT;
        kv->data.fval = *(float*)(data);
        break;
    case OPAL_BYTE_OBJECT:
        kv->type = OPAL_BYTE_OBJECT;
        boptr = (opal_byte_object_t*)data;
        if (NULL != boptr && NULL != boptr->bytes && 0 < boptr->size) {
            kv->data.bo.bytes = (uint8_t *) malloc(boptr->size);
            memcpy(kv->data.bo.bytes, boptr->bytes, boptr->size);
            kv->data.bo.size = boptr->size;
        } else {
            kv->data.bo.bytes = NULL;
            kv->data.bo.size = 0;
        }
        break;
    default:
        OPAL_ERROR_LOG(OPAL_ERR_NOT_SUPPORTED);
        return OPAL_ERR_NOT_SUPPORTED;
    }
    return OPAL_SUCCESS;
}

int opal_value_unload(opal_value_t *kv,
                      void **data, opal_data_type_t type)
{
    opal_byte_object_t *boptr;

    switch (type) {
    case OPAL_STRING:
        if (OPAL_STRING != kv->type) {
            return OPAL_ERR_TYPE_MISMATCH;
        }
        if (NULL != kv->data.string) {
            *data = strdup(kv->data.string);
        } else {
            *data = NULL;
        }
        break;
    case OPAL_UINT64:
        if (OPAL_UINT64 != kv->type) {
            return OPAL_ERR_TYPE_MISMATCH;
        }
        memcpy(*data, &kv->data.uint64, 8);
        break;
    case OPAL_UINT32:
        if (OPAL_UINT32 != kv->type) {
            return OPAL_ERR_TYPE_MISMATCH;
        }
        memcpy(*data, &kv->data.uint32, 4);
        break;
    case OPAL_UINT16:
        if (OPAL_UINT16 != kv->type) {
            return OPAL_ERR_TYPE_MISMATCH;
        }
        memcpy(*data, &kv->data.uint16, 2);
        break;
    case OPAL_INT:
        if (OPAL_INT != kv->type) {
            return OPAL_ERR_TYPE_MISMATCH;
        }
        memcpy(*data, &kv->data.integer, sizeof(int));
        break;
    case OPAL_UINT:
        if (OPAL_UINT != kv->type) {
            return OPAL_ERR_TYPE_MISMATCH;
        }
        memcpy(*data, &kv->data.uint, sizeof(unsigned int));
        break;
    case OPAL_FLOAT:
        if (OPAL_FLOAT != kv->type) {
            return OPAL_ERR_TYPE_MISMATCH;
        }
        memcpy(*data, &kv->data.fval, sizeof(float));
        break;
    case OPAL_BYTE_OBJECT:
        if (OPAL_BYTE_OBJECT != kv->type) {
            return OPAL_ERR_TYPE_MISMATCH;
        }
        boptr = (opal_byte_object_t*)malloc(sizeof(opal_byte_object_t));
        if (NULL != kv->data.bo.bytes && 0 < kv->data.bo.size) {
            boptr->bytes = (uint8_t *) malloc(kv->data.bo.size);
            memcpy(boptr->bytes, kv->data.bo.bytes, kv->data.bo.size);
            boptr->size = kv->data.bo.size;
        } else {
            boptr->bytes = NULL;
            boptr->size = 0;
        }
        *data = boptr;
        break;
    default:
        OPAL_ERROR_LOG(OPAL_ERR_NOT_SUPPORTED);
        return OPAL_ERR_NOT_SUPPORTED;
    }
    return OPAL_SUCCESS;
}
