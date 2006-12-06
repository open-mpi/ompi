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
#include "orte/orte_types.h"

#include <sys/types.h>
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "opal/types.h"
#include "opal/util/output.h"
#include "opal/mca/backtrace/backtrace.h"

#include "orte/mca/errmgr/errmgr.h"

#include "orte/dss/dss_internal.h"

int orte_dss_unpack(orte_buffer_t *buffer, void *dst, orte_std_cntr_t *num_vals,
                    orte_data_type_t type)
{
    int rc, ret;
    orte_std_cntr_t local_num, n=1;
    orte_data_type_t local_type;

    /* check for error */
    if (NULL == buffer || NULL == dst || NULL == num_vals) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    /* if user provides a zero for num_vals, then there is no storage allocated
     * so return an appropriate error
     */
    if (0 == *num_vals) {
        ORTE_ERROR_LOG(ORTE_ERR_UNPACK_INADEQUATE_SPACE);
        return ORTE_ERR_UNPACK_INADEQUATE_SPACE;
    }

    /** Unpack the declared number of values
     * REMINDER: it is possible that the buffer is corrupted and that
     * the DSS will *think* there is a proper orte_std_cntr_t variable at the
     * beginning of the unpack region - but that the value is bogus (e.g., just
     * a byte field in a string array that so happens to have a value that
     * matches the orte_std_cntr_t data type flag). Therefore, this error check is
     * NOT completely safe. This is true for ALL unpack functions, not just
     * orte_std_cntr_t as used here.
     */
    if (ORTE_DSS_BUFFER_FULLY_DESC == buffer->type) {
        if (ORTE_SUCCESS != (
            rc = orte_dss_get_data_type(buffer, &local_type))) {
            *num_vals = 0;
            return rc;
        }
        if (ORTE_STD_CNTR != local_type) { /* if the length wasn't first, then error */
            ORTE_ERROR_LOG(ORTE_ERR_UNPACK_FAILURE);
            *num_vals = 0;
            return ORTE_ERR_UNPACK_FAILURE;
        }
    }

    n=1;
    if (ORTE_SUCCESS != (rc = orte_dss_unpack_std_cntr(buffer, &local_num, &n, ORTE_STD_CNTR))) {
        *num_vals = 0;
        return rc;
    }

    /** if the storage provided is inadequate, set things up
     * to unpack as much as we can and to return an error code
     * indicating that everything was not unpacked - the buffer
     * is left in a state where it can not be further unpacked.
     */
    if (local_num > *num_vals) {
        ORTE_ERROR_LOG(ORTE_ERR_UNPACK_INADEQUATE_SPACE);
        local_num = *num_vals;
        ret = ORTE_ERR_UNPACK_INADEQUATE_SPACE;
    } else {  /** enough or more than enough storage */
        *num_vals = local_num;  /** let the user know how many we actually unpacked */
        ret = ORTE_SUCCESS;
    }

    /** Unpack the value(s) */
    if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, dst, &local_num, type))) {
        *num_vals = 0;
        ret = rc;
    }

    return ret;
}

int orte_dss_unpack_buffer(orte_buffer_t *buffer, void *dst, orte_std_cntr_t *num_vals,
                    orte_data_type_t type)
{
    int rc;
    orte_data_type_t local_type;
    orte_dss_type_info_t *info;

    OPAL_OUTPUT( ( orte_dss_verbose, "orte_dss_unpack_buffer( %p, %p, %lu, %d )\n", buffer, dst, *num_vals, (int)type ) );

    /** Unpack the declared data type */
    if (ORTE_DSS_BUFFER_FULLY_DESC == buffer->type) {
        if (ORTE_SUCCESS != (rc = orte_dss_get_data_type(buffer, &local_type))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* if the data types don't match, then return an error */
        if (type != local_type) {
            ORTE_ERROR_LOG(ORTE_ERR_PACK_MISMATCH);
            return ORTE_ERR_PACK_MISMATCH;
        }
    }

    /* Lookup the unpack function for this type and call it */

    if (NULL == (info = (orte_dss_type_info_t*)orte_pointer_array_get_item(orte_dss_types, type))) {
        ORTE_ERROR_LOG(ORTE_ERR_UNPACK_FAILURE);
        return ORTE_ERR_UNPACK_FAILURE;
    }

    rc = info->odti_unpack_fn(buffer, dst, num_vals, type);

    return rc;
}


/* UNPACK GENERIC SYSTEM TYPES */

/*
 * BOOL
 */
int orte_dss_unpack_bool(orte_buffer_t *buffer, void *dest,
                         orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    int ret;
    orte_data_type_t remote_type;

    if (ORTE_DSS_BUFFER_FULLY_DESC == buffer->type) {
        /* see what type was actually packed */
        if (ORTE_SUCCESS != (ret = orte_dss_peek_type(buffer, &remote_type))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
    } else {
        if (ORTE_SUCCESS != (ret = orte_dss_get_data_type(buffer, &remote_type))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
    }

    if (remote_type == DSS_TYPE_BOOL) {
        /* fast path it if the sizes are the same */
        /* Turn around and unpack the real type */
        if (ORTE_SUCCESS != (ret = orte_dss_unpack_buffer(buffer, dest, num_vals, DSS_TYPE_BOOL))) {
            ORTE_ERROR_LOG(ret);
        }
    } else {
        /* slow path - types are different sizes */
        UNPACK_SIZE_MISMATCH(bool, remote_type, ret);
    }
    return ret;
}

/*
 * INT
 */
int orte_dss_unpack_int(orte_buffer_t *buffer, void *dest,
                        orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    int ret;
    orte_data_type_t remote_type;

    if (ORTE_DSS_BUFFER_FULLY_DESC == buffer->type) {
        /* see what type was actually packed */
        if (ORTE_SUCCESS != (ret = orte_dss_peek_type(buffer, &remote_type))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
    } else {
        if (ORTE_SUCCESS != (ret = orte_dss_get_data_type(buffer, &remote_type))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
    }

    if (remote_type == DSS_TYPE_INT) {
        /* fast path it if the sizes are the same */
        /* Turn around and unpack the real type */
        if (ORTE_SUCCESS != (ret = orte_dss_unpack_buffer(buffer, dest, num_vals, DSS_TYPE_INT))) {
            ORTE_ERROR_LOG(ret);
        }
    } else {
        /* slow path - types are different sizes */
        UNPACK_SIZE_MISMATCH(int, remote_type, ret);
    }
    
    return ret;
}

/*
 * SIZE_T
 */
int orte_dss_unpack_sizet(orte_buffer_t *buffer, void *dest,
                          orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    int ret;
    orte_data_type_t remote_type;

    if (ORTE_DSS_BUFFER_FULLY_DESC == buffer->type) {
        /* see what type was actually packed */
        if (ORTE_SUCCESS != (ret = orte_dss_peek_type(buffer, &remote_type))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
    } else {
        if (ORTE_SUCCESS != (ret = orte_dss_get_data_type(buffer, &remote_type))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
    }

    if (remote_type == DSS_TYPE_SIZE_T) {
        /* fast path it if the sizes are the same */
        /* Turn around and unpack the real type */
        if (ORTE_SUCCESS != (ret = orte_dss_unpack_buffer(buffer, dest, num_vals, DSS_TYPE_SIZE_T))) {
            ORTE_ERROR_LOG(ret);
        }
    } else {
        /* slow path - types are different sizes */
        UNPACK_SIZE_MISMATCH(size_t, remote_type, ret);
    }
    
    return ret;
}

/*
 * PID_T
 */
int orte_dss_unpack_pid(orte_buffer_t *buffer, void *dest,
                        orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    int ret;
    orte_data_type_t remote_type;

    if (ORTE_DSS_BUFFER_FULLY_DESC == buffer->type) {
        /* see what type was actually packed */
        if (ORTE_SUCCESS != (ret = orte_dss_peek_type(buffer, &remote_type))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
    } else {
        if (ORTE_SUCCESS != (ret = orte_dss_get_data_type(buffer, &remote_type))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
    }

    if (remote_type == DSS_TYPE_PID_T) {
        /* fast path it if the sizes are the same */
        /* Turn around and unpack the real type */
        if (ORTE_SUCCESS != (ret = orte_dss_unpack_buffer(buffer, dest, num_vals, DSS_TYPE_PID_T))) {
            ORTE_ERROR_LOG(ret);
        }
    } else {
        /* slow path - types are different sizes */
        UNPACK_SIZE_MISMATCH(pid_t, remote_type, ret);
    }
    
    return ret;
}


/* UNPACK FUNCTIONS FOR NON-GENERIC SYSTEM TYPES */

/*
 * NULL
 */
int orte_dss_unpack_null(orte_buffer_t *buffer, void *dest,
                         orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    OPAL_OUTPUT( ( orte_dss_verbose, "orte_dss_unpack_null * %d\n", (int)*num_vals ) );
    /* check to see if there's enough data in buffer */
    if (orte_dss_too_small(buffer, *num_vals)) {
        return ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    }

    /* unpack the data */
    memcpy(dest, buffer->unpack_ptr, *num_vals);

    /* update buffer pointer */
    buffer->unpack_ptr += *num_vals;

    return ORTE_SUCCESS;
}

/*
 * BYTE, CHAR, INT8
 */
int orte_dss_unpack_byte(orte_buffer_t *buffer, void *dest,
                         orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    OPAL_OUTPUT( ( orte_dss_verbose, "orte_dss_unpack_byte * %d\n", (int)*num_vals ) );
    /* check to see if there's enough data in buffer */
    if (orte_dss_too_small(buffer, *num_vals)) {
        return ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    }

    /* unpack the data */
    memcpy(dest, buffer->unpack_ptr, *num_vals);

    /* update buffer pointer */
    buffer->unpack_ptr += *num_vals;

    return ORTE_SUCCESS;
}

int orte_dss_unpack_int16(orte_buffer_t *buffer, void *dest,
                          orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    orte_std_cntr_t i;
    uint16_t tmp, *desttmp = (uint16_t*) dest;

   OPAL_OUTPUT( ( orte_dss_verbose, "orte_dss_unpack_int16 * %d\n", (int)*num_vals ) );
    /* check to see if there's enough data in buffer */
    if (orte_dss_too_small(buffer, (*num_vals)*sizeof(tmp))) {
        return ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    }

    /* unpack the data */
    for (i = 0; i < (*num_vals); ++i) {
        memcpy( &(tmp), buffer->unpack_ptr, sizeof(tmp) );
        desttmp[i] = ntohs(tmp);
        buffer->unpack_ptr += sizeof(tmp);
    }

    return ORTE_SUCCESS;
}

int orte_dss_unpack_int32(orte_buffer_t *buffer, void *dest,
                          orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    orte_std_cntr_t i;
    uint32_t tmp, *desttmp = (uint32_t*) dest;

   OPAL_OUTPUT( ( orte_dss_verbose, "orte_dss_unpack_int32 * %d\n", (int)*num_vals ) );
    /* check to see if there's enough data in buffer */
    if (orte_dss_too_small(buffer, (*num_vals)*sizeof(tmp))) {
        return ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    }

    /* unpack the data */
    for (i = 0; i < (*num_vals); ++i) {
        memcpy( &(tmp), buffer->unpack_ptr, sizeof(tmp) );
        desttmp[i] = ntohl(tmp);
        buffer->unpack_ptr += sizeof(tmp);
    }

    return ORTE_SUCCESS;
}

int orte_dss_unpack_int64(orte_buffer_t *buffer, void *dest,
                          orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    orte_std_cntr_t i;
    uint64_t tmp, *desttmp = (uint64_t*) dest;

   OPAL_OUTPUT( ( orte_dss_verbose, "orte_dss_unpack_int64 * %d\n", (int)*num_vals ) );
    /* check to see if there's enough data in buffer */
    if (orte_dss_too_small(buffer, (*num_vals)*sizeof(tmp))) {
        return ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    }

    /* unpack the data */
    for (i = 0; i < (*num_vals); ++i) {
        memcpy( &(tmp), buffer->unpack_ptr, sizeof(tmp) );
        desttmp[i] = ntoh64(tmp);
        buffer->unpack_ptr += sizeof(tmp);
    }

    return ORTE_SUCCESS;
}

int orte_dss_unpack_string(orte_buffer_t *buffer, void *dest,
                           orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    int ret;
    orte_std_cntr_t i, len, n=1;
    char **sdest = (char**) dest;

    for (i = 0; i < (*num_vals); ++i) {
        if (ORTE_SUCCESS != (ret = orte_dss_unpack_std_cntr(buffer, &len, &n, ORTE_STD_CNTR))) {
            return ret;
        }
        if (0 ==  len) {   /* zero-length string - unpack the NULL */
            sdest[i] = NULL;
        } else {
        sdest[i] = (char*)malloc(len);
            if (NULL == sdest[i]) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            if (ORTE_SUCCESS != (ret = orte_dss_unpack_byte(buffer, sdest[i], &len, ORTE_BYTE))) {
                return ret;
            }
        }
    }

    return ORTE_SUCCESS;
}


/* UNPACK FUNCTIONS FOR GENERIC ORTE TYPES */

/*
 * ORTE_STD_CNTR
 */
int orte_dss_unpack_std_cntr(orte_buffer_t *buffer, void *dest, orte_std_cntr_t *num_vals,
                              orte_data_type_t type)
{
    int ret;
    
    /* turn around and unpack the real type */
    ret = orte_dss_unpack_buffer(buffer, dest, num_vals, ORTE_STD_CNTR_T);
    
    return ret;
}

/*
 * ORTE_DATA_TYPE
 */
int orte_dss_unpack_data_type(orte_buffer_t *buffer, void *dest, orte_std_cntr_t *num_vals,
                             orte_data_type_t type)
{
    int ret;
    
     /* turn around and unpack the real type */
     ret = orte_dss_unpack_buffer(buffer, dest, num_vals, ORTE_DATA_TYPE_T);
    
    return ret;
}

/*
 * ORTE_DATA_VALUE
 */
int orte_dss_unpack_data_value(orte_buffer_t *buffer, void *dest, orte_std_cntr_t *num,
                             orte_data_type_t type)
{
    orte_dss_type_info_t *info;
    orte_data_value_t **ddv;
    orte_std_cntr_t i, n;
    orte_data_type_t dt;
    size_t nsize;
    int ret;

    ddv = (orte_data_value_t **) dest;

    for (i = 0; i < *num; ++i) {
        /* see what the data type is */
        n = 1;
        if (ORTE_SUCCESS != (ret = orte_dss_get_data_type(buffer, &dt))) {
            return ret;
        }
        
        /* if it is ORTE_NULL, then do nothing */
        if (ORTE_NULL == dt) continue;
        
        /* otherwise, allocate the new object and set the type */
        
        ddv[i] = OBJ_NEW(orte_data_value_t);
        if (NULL == ddv[i]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        ddv[i]->type = dt;

        /* if it is UNDEF, then nothing more to do */
        if (ORTE_UNDEF == ddv[i]->type) continue;

        /* get enough memory to hold it */
        if (ORTE_SUCCESS != (ret = orte_dss.size(&nsize, NULL, ddv[i]->type))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
        ddv[i]->data = (void*)malloc(nsize);
        if (NULL == ddv[i]->data) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }

        /* Lookup the unpack function for this type and call it */

        if (NULL == (info = (orte_dss_type_info_t*)orte_pointer_array_get_item(orte_dss_types, ddv[i]->type))) {
            ORTE_ERROR_LOG(ORTE_ERR_PACK_FAILURE);
            return ORTE_ERR_PACK_FAILURE;
        }

        if (info->odti_structured) {
            n=1;
            if (ORTE_SUCCESS != (ret = orte_dss_unpack_buffer(buffer, &(ddv[i]->data), &n, ddv[i]->type))) {
                return ret;
            }
        } else {
            n=1;
            if (ORTE_SUCCESS != (ret = orte_dss_unpack_buffer(buffer, ddv[i]->data, &n, ddv[i]->type))) {
                return ret;
            }
        }
    }

    return ORTE_SUCCESS;
}


/*
 * ORTE_BYTE_OBJECT
 */
int orte_dss_unpack_byte_object(orte_buffer_t *buffer, void *dest, orte_std_cntr_t *num,
                             orte_data_type_t type)
{
    int ret;
    orte_std_cntr_t i, n, m=1;
    orte_byte_object_t **dbyteptr;

    dbyteptr = (orte_byte_object_t**)dest;
    n = *num;
    for(i=0; i<n; i++) {
        /* allocate memory for the byte object itself */
        dbyteptr[i] = (orte_byte_object_t*)malloc(sizeof(orte_byte_object_t));
        if (NULL == dbyteptr[i]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }

        /* unpack object size in bytes */
        if (ORTE_SUCCESS != (ret = orte_dss_unpack_std_cntr(buffer, &(dbyteptr[i]->size), &m, ORTE_STD_CNTR))) {
            return ret;
        }
        if (0 < dbyteptr[i]->size) {
            dbyteptr[i]->bytes = (uint8_t*)malloc(dbyteptr[i]->size);
            if (NULL == dbyteptr[i]->bytes) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            if (ORTE_SUCCESS != (ret = orte_dss_unpack_byte(buffer, (dbyteptr[i]->bytes),
                                            &(dbyteptr[i]->size), ORTE_BYTE))) {
                return ret;
            }
        }
    }

    return ORTE_SUCCESS;
}
