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

#include "opal/types.h"
#include "opal/util/output.h"

#include "orte/mca/errmgr/errmgr.h"

#include "orte/dss/dss_internal.h"

int orte_dss_pack(orte_buffer_t *buffer, void *src, orte_std_cntr_t num_vals,
                  orte_data_type_t type)
{
    int rc;

    /* check for error */
    if (NULL == buffer) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    /* Pack the number of values */
    if (ORTE_DSS_BUFFER_FULLY_DESC == buffer->type) {
        if (ORTE_SUCCESS != (rc = orte_dss_store_data_type(buffer, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    if (ORTE_SUCCESS != (rc = orte_dss_pack_std_cntr(buffer, &num_vals, 1, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* Pack the value(s) */
    if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer, src, num_vals, type))) {
        ORTE_ERROR_LOG(rc);
    }

    return rc;
}

int orte_dss_pack_buffer(orte_buffer_t *buffer, void *src, orte_std_cntr_t num_vals,
                  orte_data_type_t type)
{
    int rc;
    orte_dss_type_info_t *info;

    OPAL_OUTPUT( ( orte_dss_verbose, "orte_dss_pack_buffer( %p, %p, %lu, %d )\n", buffer, src, num_vals, (int)type ) );

    /* Pack the declared data type */
    if (ORTE_DSS_BUFFER_FULLY_DESC == buffer->type) {
        if (ORTE_SUCCESS != (rc = orte_dss_store_data_type(buffer, type))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    /* Lookup the pack function for this type and call it */

    if (NULL == (info = (orte_dss_type_info_t*)orte_pointer_array_get_item(orte_dss_types, type))) {
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
 * BOOL
 */
int orte_dss_pack_bool(orte_buffer_t *buffer, void *src,
                       orte_std_cntr_t num_vals, orte_data_type_t type)
{
    int ret;

    /* System types need to always be described so we can properly
       unpack them.  If we aren't fully described, then add the
       description for this type... */
    if (ORTE_DSS_BUFFER_FULLY_DESC != buffer->type) {
        if (ORTE_SUCCESS != (ret = orte_dss_store_data_type(buffer, DSS_TYPE_BOOL))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
    }

    /* Turn around and pack the real type */
    if (ORTE_SUCCESS != (
        ret = orte_dss_pack_buffer(buffer, src, num_vals, DSS_TYPE_BOOL))) {
        ORTE_ERROR_LOG(ret);
    }

    return ret;
}

/*
 * INT
 */
int orte_dss_pack_int(orte_buffer_t *buffer, void *src,
                      orte_std_cntr_t num_vals, orte_data_type_t type)
{
    int ret;

    /* System types need to always be described so we can properly
       unpack them.  If we aren't fully described, then add the
       description for this type... */
    if (ORTE_DSS_BUFFER_FULLY_DESC != buffer->type) {
        if (ORTE_SUCCESS != (ret = orte_dss_store_data_type(buffer, DSS_TYPE_INT))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
    }

    /* Turn around and pack the real type */
    if (ORTE_SUCCESS != (
        ret = orte_dss_pack_buffer(buffer, src, num_vals, DSS_TYPE_INT))) {
        ORTE_ERROR_LOG(ret);
    }

    return ret;
}

/*
 * SIZE_T
 */
int orte_dss_pack_sizet(orte_buffer_t *buffer, void *src,
                        orte_std_cntr_t num_vals, orte_data_type_t type)
{
    int ret;

    /* System types need to always be described so we can properly
       unpack them.  If we aren't fully described, then add the
       description for this type... */
    if (ORTE_DSS_BUFFER_FULLY_DESC != buffer->type) {
        if (ORTE_SUCCESS != (ret = orte_dss_store_data_type(buffer, DSS_TYPE_SIZE_T))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
    }

    /* Turn around and pack the real type */
    if (ORTE_SUCCESS != (
        ret = orte_dss_pack_buffer(buffer, src, num_vals, DSS_TYPE_SIZE_T))) {
        ORTE_ERROR_LOG(ret);
    }

    return ret;
}

/*
 * PID_T
 */
int orte_dss_pack_pid(orte_buffer_t *buffer, void *src,
                      orte_std_cntr_t num_vals, orte_data_type_t type)
{
    int ret;

    /* System types need to always be described so we can properly
       unpack them.  If we aren't fully described, then add the
       description for this type... */
    if (ORTE_DSS_BUFFER_FULLY_DESC != buffer->type) {
        if (ORTE_SUCCESS != (ret = orte_dss_store_data_type(buffer, DSS_TYPE_PID_T))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
    }

    /* Turn around and pack the real type */
    if (ORTE_SUCCESS != (
        ret = orte_dss_pack_buffer(buffer, src, num_vals, DSS_TYPE_PID_T))) {
        ORTE_ERROR_LOG(ret);
    }

    return ret;
}


/* PACK FUNCTIONS FOR NON-GENERIC SYSTEM TYPES */

/*
 * NULL
 */
int orte_dss_pack_null(orte_buffer_t *buffer, void *src,
                       orte_std_cntr_t num_vals, orte_data_type_t type)
{
    char null=0x00;
    char *dst;

    OPAL_OUTPUT( ( orte_dss_verbose, "orte_dss_pack_null * %d\n", num_vals ) );
    /* check to see if buffer needs extending */
    if (NULL == (dst = orte_dss_buffer_extend(buffer, num_vals))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* store the nulls */
    memset(dst, (int)null, num_vals);

    /* update buffer pointers */
    buffer->pack_ptr += num_vals;
    buffer->bytes_used += num_vals;
    buffer->bytes_avail -= num_vals;


    return ORTE_SUCCESS;
}

/*
 * BYTE, CHAR, INT8
 */
int orte_dss_pack_byte(orte_buffer_t *buffer, void *src,
                       orte_std_cntr_t num_vals, orte_data_type_t type)
{
    char *dst;

    OPAL_OUTPUT( ( orte_dss_verbose, "orte_dss_pack_byte * %d\n", num_vals ) );
    /* check to see if buffer needs extending */
    if (NULL == (dst = orte_dss_buffer_extend(buffer, num_vals))) {
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
int orte_dss_pack_int16(orte_buffer_t *buffer, void *src,
                        orte_std_cntr_t num_vals, orte_data_type_t type)
{
    orte_std_cntr_t i;
    uint16_t tmp, *srctmp = (uint16_t*) src;
    char *dst;

    OPAL_OUTPUT( ( orte_dss_verbose, "orte_dss_pack_int16 * %d\n", num_vals ) );
    /* check to see if buffer needs extending */
    if (NULL == (dst = orte_dss_buffer_extend(buffer, num_vals*sizeof(tmp)))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    for (i = 0; i < num_vals; ++i) {
        tmp = htons(srctmp[i]);
        memcpy(dst, &tmp, sizeof(tmp));
        dst += sizeof(tmp);
    }
    buffer->pack_ptr += num_vals * sizeof(tmp);
    buffer->bytes_used += num_vals * sizeof(tmp);
    buffer->bytes_avail -= num_vals * sizeof(tmp);

    return ORTE_SUCCESS;
}

/*
 * INT32
 */
int orte_dss_pack_int32(orte_buffer_t *buffer, void *src,
                        orte_std_cntr_t num_vals, orte_data_type_t type)
{
    orte_std_cntr_t i;
    uint32_t tmp, *srctmp = (uint32_t*) src;
    char *dst;

    OPAL_OUTPUT( ( orte_dss_verbose, "orte_dss_pack_int32 * %d\n", num_vals ) );
    /* check to see if buffer needs extending */
    if (NULL == (dst = orte_dss_buffer_extend(buffer, num_vals*sizeof(tmp)))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    for (i = 0; i < num_vals; ++i) {
        tmp = htonl(srctmp[i]);
        memcpy(dst, &tmp, sizeof(tmp));
        dst += sizeof(tmp);
    }
    buffer->pack_ptr += num_vals * sizeof(tmp);
    buffer->bytes_used += num_vals * sizeof(tmp);
    buffer->bytes_avail -= num_vals * sizeof(tmp);

    return ORTE_SUCCESS;
}

/*
 * INT64
 */
int orte_dss_pack_int64(orte_buffer_t *buffer, void *src,
                        orte_std_cntr_t num_vals, orte_data_type_t type)
{
    orte_std_cntr_t i;
    uint64_t tmp, *srctmp = (uint64_t*) src;
    char *dst;
    size_t bytes_packed = num_vals * sizeof(tmp);

    OPAL_OUTPUT( ( orte_dss_verbose, "orte_dss_pack_int64 * %d\n", num_vals ) );
    /* check to see if buffer needs extending */
    if (NULL == (dst = orte_dss_buffer_extend(buffer, bytes_packed))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    for (i = 0; i < num_vals; ++i) {
        tmp = hton64(srctmp[i]);
        memcpy(dst, &tmp, sizeof(tmp));
        dst += sizeof(tmp);
    }
    buffer->pack_ptr += bytes_packed;
    buffer->bytes_used += bytes_packed;
    buffer->bytes_avail -= bytes_packed;

    return ORTE_SUCCESS;
}

/*
 * STRING
 */
int orte_dss_pack_string(orte_buffer_t *buffer, void *src,
                         orte_std_cntr_t num_vals, orte_data_type_t type)
{
    int ret = ORTE_SUCCESS;
    orte_std_cntr_t i, len;
    char **ssrc = (char**) src;

    for (i = 0; i < num_vals; ++i) {
        if (NULL == ssrc[i]) {  /* got zero-length string/NULL pointer - store NULL */
            len = 0;
            if (ORTE_SUCCESS != (ret = orte_dss_pack_std_cntr(buffer, &len, 1, ORTE_STD_CNTR))) {
                ORTE_ERROR_LOG(ret);
                return ret;
            }
        } else {
            len = (orte_std_cntr_t)strlen(ssrc[i]) + 1;
            if (ORTE_SUCCESS != (ret = orte_dss_pack_std_cntr(buffer, &len, 1, ORTE_STD_CNTR))) {
                ORTE_ERROR_LOG(ret);
                return ret;
            }
            if (ORTE_SUCCESS != (ret =
                orte_dss_pack_byte(buffer, ssrc[i], len, ORTE_BYTE))) {
                ORTE_ERROR_LOG(ret);
                return ret;
            }
        }
    }

    return ORTE_SUCCESS;
}

/* PACK FUNCTIONS FOR GENERIC ORTE TYPES */

/*
 * ORTE_STD_CNTR
 */
int orte_dss_pack_std_cntr(orte_buffer_t *buffer, void *src, orte_std_cntr_t num_vals,
                            orte_data_type_t type)
{
    int ret;
    
    /* Turn around and pack the real type */
    if (ORTE_SUCCESS != (ret = orte_dss_pack_buffer(buffer, src, num_vals, ORTE_STD_CNTR_T))) {
        ORTE_ERROR_LOG(ret);
    }
    
    return ret;
}

/*
 * ORTE_DATA_TYPE
 */
int orte_dss_pack_data_type(orte_buffer_t *buffer, void *src, orte_std_cntr_t num_vals,
                             orte_data_type_t type)
{
    int ret;
    
    /* Turn around and pack the real type */
    if (ORTE_SUCCESS != (ret = orte_dss_pack_buffer(buffer, src, num_vals, ORTE_DATA_TYPE_T))) {
        ORTE_ERROR_LOG(ret);
    }
    
    return ret;
}

/*
 * ORTE_DATA_VALUE
 */
int orte_dss_pack_data_value(orte_buffer_t *buffer, void *src, orte_std_cntr_t num, orte_data_type_t type)
{
    orte_dss_type_info_t *info;
    orte_data_value_t **sdv;
    orte_std_cntr_t i;
    int ret;

    sdv = (orte_data_value_t **) src;

    for (i = 0; i < num; ++i) {
        /* if the src data value is NULL, then we will pack it as ORTE_NULL to indicate
         * that the unpack should leave a NULL data value
         */
        if (NULL == sdv[i]) {
            if (ORTE_SUCCESS != (ret = orte_dss_store_data_type(buffer, ORTE_NULL))) {
                ORTE_ERROR_LOG(ret);
                return ret;
            }
            continue;
        }
        
        /* pack the data type - we'll need it on the other end */
        if (ORTE_SUCCESS != (ret = orte_dss_store_data_type(buffer, sdv[i]->type))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }

        /* if the data type is UNDEF, then nothing more to do */
        if (ORTE_UNDEF == sdv[i]->type) continue;
        
        /* Lookup the pack function for this type and call it */

        if (NULL == (info = (orte_dss_type_info_t*)orte_pointer_array_get_item(orte_dss_types, sdv[i]->type))) {
            ORTE_ERROR_LOG(ORTE_ERR_PACK_FAILURE);
            return ORTE_ERR_PACK_FAILURE;
        }

        if (info->odti_structured) {
            if (ORTE_SUCCESS != (ret = orte_dss_pack_buffer(buffer, &(sdv[i]->data), 1, sdv[i]->type))) {
                ORTE_ERROR_LOG(ret);
                return ret;
            }
        } else {
            if (ORTE_SUCCESS != (ret = orte_dss_pack_buffer(buffer, sdv[i]->data, 1, sdv[i]->type))) {
                ORTE_ERROR_LOG(ret);
                return ret;
            }
        }
    }

    return ORTE_SUCCESS;
}

/*
 * ORTE_BYTE_OBJECT
 */
int orte_dss_pack_byte_object(orte_buffer_t *buffer, void *src, orte_std_cntr_t num,
                             orte_data_type_t type)
{
    orte_byte_object_t **sbyteptr;
    orte_std_cntr_t i, n;
    int ret;

    sbyteptr = (orte_byte_object_t **) src;

    for (i = 0; i < num; ++i) {
        n = sbyteptr[i]->size;
        if (ORTE_SUCCESS != (ret = orte_dss_pack_std_cntr(buffer, &n, 1, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
        if (0 < n) {
            if (ORTE_SUCCESS != (ret =
                orte_dss_pack_byte(buffer, sbyteptr[i]->bytes, n, ORTE_BYTE))) {
                ORTE_ERROR_LOG(ret);
                return ret;
            }
        }
    }

    return ORTE_SUCCESS;
}
