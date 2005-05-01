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
    if (NULL == buffer || NULL == src || num_vals < 0 ||
        type > orte_value_array_get_size(&orte_dps_types)) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    /* Pack the number of values */
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
    orte_dps_type_info_t *info;
    int rc;
    
    /* Pack the declared data type */
    if (ORTE_SUCCESS != (rc = orte_dps_store_data_type(buffer, type))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* Lookup the pack function for this type and call it */

    info = orte_value_array_get_item(&orte_dps_types, type);
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
    char null=NULL;
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
    buffer->pack_ptr += num_bytes;
    buffer->bytes_used += num_bytes;
    buffer->bytes_avail -= num_bytes;
    
    return ORTE_SUCCESS;
}

/*
 * INT16
 */
int orte_dps_pack_int16(orte_buffer_t *buffer, void *src,
                        size_t num_vals, orte_data_type_t type)
{
    int ret;
    size_t i;
    uint16_t tmp, *srctmp = (uint16_t*) src;
    char *dst;
    
    /* check to see if buffer needs extending */
    if (NULL == (dst = orte_dps_buffer_extend(buffer, num_vals*sizeof(tmp)))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    for (i = 0; i < num_vals; ++i) {
        tmp = htons(srctmp[i]);
        memcpy(((uint16_t*) dst) + i, &tmp, sizeof(tmp));
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
    int ret;
    size_t i;
    uint32_t tmp, *srctmp = (uint32_t*) src;
    char *dst;
    
    /* check to see if buffer needs extending */
    if (NULL == (dst = orte_dps_buffer_extend(buffer, num_vals*sizeof(tmp)))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    for (i = 0; i < num_vals; ++i) {
        tmp = htonl(srctmp[i]);
        memcpy(((uint32_t*) dst) + i, &tmp, sizeof(tmp));
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
    int ret;
    size_t i;
    uint32_t tmp, *srctmp = (uint32_t*) src;
    char *dst;
    
    num_vals *= 2;
    
    /* check to see if buffer needs extending */
    if (NULL == (dst = orte_dps_buffer_extend(buffer, num_vals*sizeof(tmp)))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    for (i = 0; i < num_vals; ++i) {
        tmp = htonl(srctmp[i]);
        memcpy(((uint32_t*) dst) + i, &tmp, sizeof(tmp));
    }
    buffer->pack_ptr += num_vals * sizeof(tmp);
    buffer->bytes_used += num_vals * sizeof(tmp);
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
