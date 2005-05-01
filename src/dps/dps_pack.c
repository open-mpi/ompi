/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
 
#include "orte_config.h"

#include <sys/types.h>
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "mca/errmgr/errmgr.h"

#include "dps/dps_internal.h"

int orte_dps_pack(orte_buffer_t *buffer, void *src, size_t num_vals,
                  orte_data_type_t type)
{
    int rc;

    /* check for error */
    if (NULL == buffer || NULL == src || num_vals < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    /* Pack the number of values */
    if (ORTE_SUCCESS != (rc = orte_dps_store_data_type(buffer, ORTE_SIZE))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_dps_pack_sizet(buffer, &num_vals, 1, ORTE_SIZE))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* Pack the value(s) */
    if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer, src, num_vals, type))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

int orte_dps_pack_buffer(orte_buffer_t *buffer, void *src, size_t num_vals,
                  orte_data_type_t type)
{
    int rc;
    orte_dps_type_info_t *info;
    
    /* Pack the declared data type */
    if (ORTE_SUCCESS != (rc = orte_dps_pack_data_type(buffer, &type, 1, type))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* Lookup the pack function for this type and call it */

    if (NULL == (info = orte_pointer_array_get_item(orte_dps_types, type))) {
        ORTE_ERROR_LOG(ORTE_ERR_PACK_FAILURE);
        return ORTE_ERR_PACK_FAILURE;
    }
    
    if (ORTE_SUCCESS != (rc = info->odti_pack_fn(buffer, src, num_vals, type))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}


/* PACK FUNCTIONS FOR GENERIC SYSTEM TYPES */

/*
 * NULL
 */
int orte_dps_pack_null(orte_buffer_t *buffer, void *src,
                       size_t num_vals, orte_data_type_t type)
{
    char null=0x00;
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_dps_pack_byte(buffer, &null, num_vals, ORTE_BYTE))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

/*
 * BOOL
 */
int orte_dps_pack_bool(orte_buffer_t *buffer, void *src,
                       size_t num_vals, orte_data_type_t type)
{
    int ret;

    /* Turn around and pack the real type */
    if (ORTE_SUCCESS != (
        ret = orte_dps_pack_buffer(buffer, src, num_vals, DPS_TYPE_BOOL))) {
        ORTE_ERROR_LOG(ret);
    }

    return ret;
}

/*
 * INT
 */
int orte_dps_pack_int(orte_buffer_t *buffer, void *src,
                      size_t num_vals, orte_data_type_t type)
{
    int ret;

    /* Turn around and pack the real type */
    if (ORTE_SUCCESS != (
        ret = orte_dps_pack_buffer(buffer, src, num_vals, DPS_TYPE_INT))) {
        ORTE_ERROR_LOG(ret);
    }

    return ret;
}

/*
 * SIZE_T
 */
int orte_dps_pack_sizet(orte_buffer_t *buffer, void *src,
                        size_t num_vals, orte_data_type_t type)
{
    int ret;

    /* Turn around and pack the real type */
    if (ORTE_SUCCESS != (
        ret = orte_dps_pack_buffer(buffer, src, num_vals, DPS_TYPE_SIZE_T))) {
        ORTE_ERROR_LOG(ret);
    }

    return ret;
}


/* PACK FUNCTIONS FOR NON-GENERIC SYSTEM TYPES */

/*
 * BYTE, CHAR, INT8
 */
int orte_dps_pack_byte(orte_buffer_t *buffer, void *src,
                       size_t num_vals, orte_data_type_t type)
{
    char *dst;
    
    /* check to see if buffer needs extending */
    if (NULL == (dst = orte_dps_buffer_extend(buffer, num_vals))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    /* store the data */
    memcpy(dst, src, num_vals);
    
    /* update buffer pointers */
    buffer->pack_ptr += num_vals;
    buffer->bytes_used += num_vals;
    buffer->bytes_avail -= num_vals;
    
    return ORTE_SUCCESS;
}

/*
 * INT16
 */
int orte_dps_pack_int16(orte_buffer_t *buffer, void *src,
                        size_t num_vals, orte_data_type_t type)
{
    size_t i;
    uint16_t tmp, *dsttmp, *srctmp = (uint16_t*) src;
    char *dst;
    
    /* check to see if buffer needs extending */
    if (NULL == (dst = orte_dps_buffer_extend(buffer, num_vals*sizeof(tmp)))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    dsttmp = (uint16_t*)dst;
    for (i = 0; i < num_vals; ++i) {
        tmp = htons(srctmp[i]);
        memcpy(&dsttmp[i], &tmp, sizeof(tmp));
    }
    buffer->pack_ptr += num_vals * sizeof(tmp);
    buffer->bytes_used += num_vals * sizeof(tmp);
    buffer->bytes_avail -= num_vals * sizeof(tmp);
    
    return ORTE_SUCCESS;
}

/*
 * INT32
 */
int orte_dps_pack_int32(orte_buffer_t *buffer, void *src,
                        size_t num_vals, orte_data_type_t type)
{
    size_t i;
    uint32_t tmp, *dsttmp, *srctmp = (uint32_t*) src;
    char *dst;
    
    /* check to see if buffer needs extending */
    if (NULL == (dst = orte_dps_buffer_extend(buffer, num_vals*sizeof(tmp)))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    dsttmp = (uint32_t*)dst;
    for (i = 0; i < num_vals; ++i) {
        tmp = htonl(srctmp[i]);
        memcpy(&dsttmp[i], &tmp, sizeof(tmp));
    }
    buffer->pack_ptr += num_vals * sizeof(tmp);
    buffer->bytes_used += num_vals * sizeof(tmp);
    buffer->bytes_avail -= num_vals * sizeof(tmp);
    
    return ORTE_SUCCESS;
}

/*
 * INT64
 */
int orte_dps_pack_int64(orte_buffer_t *buffer, void *src,
                        size_t num_vals, orte_data_type_t type)
{
    size_t i;
    uint32_t tmp, *dsttmp, *srctmp = (uint32_t*) src;
    char *dst;
    
    /* check to see if buffer needs extending */
    if (NULL == (dst = orte_dps_buffer_extend(buffer, num_vals*sizeof(tmp)))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    dsttmp = (uint32_t*)dst;
    for (i = 0; i < num_vals; i += 2) {
        tmp = htonl(srctmp[i]);
        memcpy(&dsttmp[i], &tmp, sizeof(tmp));
        tmp = htonl(srctmp[i+1]);
        memcpy(&dsttmp[i+1], &tmp, sizeof(tmp));
    }
    buffer->pack_ptr += 2*num_vals * sizeof(tmp);
    buffer->bytes_used += 2*num_vals * sizeof(tmp);
    buffer->bytes_avail -= num_vals * sizeof(tmp);
    
    return ORTE_SUCCESS;
}

/*
 * STRING
 */
int orte_dps_pack_string(orte_buffer_t *buffer, void *src,
                         size_t num_vals, orte_data_type_t type)
{
    int ret = ORTE_SUCCESS;
    size_t i, len;
    char **ssrc = (char**) src;

    for (i = 0; i < num_vals; ++i) {
        len = strlen(ssrc[i]) + 1;
        if (ORTE_SUCCESS != (ret = orte_dps_pack_sizet(buffer, &len, 1, ORTE_SIZE))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
        if (ORTE_SUCCESS != (ret = 
            orte_dps_pack_byte(buffer, ssrc[i], len, ORTE_BYTE))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
    }

    return ORTE_SUCCESS;
}

/* PACK FUNCTIONS FOR GENERIC ORTE TYPES */

/*
 * ORTE_DATA_TYPE
 */
int orte_dps_pack_data_type(orte_buffer_t *buffer, void *src, size_t num,
                             orte_data_type_t type)
{
    size_t required;
    int rc;
    
    required = sizeof(orte_data_type_t);
    switch (required) {
    
        case 1:
            if (ORTE_SUCCESS != (
                rc = orte_dps_pack_byte(buffer, src, num, ORTE_BYTE))) {
                ORTE_ERROR_LOG(rc);
            }
            break;
        
        case 2:
            if (ORTE_SUCCESS != (
                rc = orte_dps_pack_int16(buffer, src, num, ORTE_INT16))) {
                ORTE_ERROR_LOG(rc);
            }
            break;
        
        case 4:
            if (ORTE_SUCCESS != (
                rc = orte_dps_pack_int32(buffer, src, num, ORTE_INT32))) {
                ORTE_ERROR_LOG(rc);
            }
            break;
        
        case 8:
            if (ORTE_SUCCESS != (
                rc = orte_dps_pack_int64(buffer, src, num, ORTE_INT64))) {
                ORTE_ERROR_LOG(rc);
            }
            break;
        
        default:
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
            return ORTE_ERR_BAD_PARAM;
    }

    return rc;
}

/*
 * ORTE_BYTE_OBJECT
 */
int orte_dps_pack_byte_object(orte_buffer_t *buffer, void *src, size_t num,
                             orte_data_type_t type)
{
    orte_byte_object_t *sbyteptr;
    size_t i, n;
    int ret;
    
    sbyteptr = (orte_byte_object_t *) src;

    for (i = 0; i < num; ++i) {
        n = sbyteptr->size;
        if (0 < n) {
            if (ORTE_SUCCESS != (ret = orte_dps_pack_sizet(buffer, &n, 1, ORTE_SIZE))) {
                ORTE_ERROR_LOG(ret);
                return ret;
            }
            if (ORTE_SUCCESS != (ret = 
                orte_dps_pack_byte(buffer, sbyteptr->bytes, n, ORTE_BYTE))) {
                ORTE_ERROR_LOG(ret);
                return ret;
            }
        }
        sbyteptr++;
    }
    
    return ORTE_SUCCESS;
}
