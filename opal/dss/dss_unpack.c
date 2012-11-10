/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, Inc.  All rights reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/types.h"
#include "opal/util/error.h"
#include "opal/util/output.h"
#include "opal/dss/dss_internal.h"

int opal_dss_unpack(opal_buffer_t *buffer, void *dst, int32_t *num_vals,
                    opal_data_type_t type)
{
    int rc, ret;
    int32_t local_num, n=1;
    opal_data_type_t local_type;

    /* check for error */
    if (NULL == buffer || NULL == dst || NULL == num_vals) {
        return OPAL_ERR_BAD_PARAM;
    }

    /* if user provides a zero for num_vals, then there is no storage allocated
     * so return an appropriate error
     */
    if (0 == *num_vals) {
        return OPAL_ERR_UNPACK_INADEQUATE_SPACE;
    }

    /** Unpack the declared number of values
     * REMINDER: it is possible that the buffer is corrupted and that
     * the DSS will *think* there is a proper int32_t variable at the
     * beginning of the unpack region - but that the value is bogus (e.g., just
     * a byte field in a string array that so happens to have a value that
     * matches the int32_t data type flag). Therefore, this error check is
     * NOT completely safe. This is true for ALL unpack functions, not just
     * int32_t as used here.
     */
    if (OPAL_DSS_BUFFER_FULLY_DESC == buffer->type) {
        if (OPAL_SUCCESS != (
            rc = opal_dss_get_data_type(buffer, &local_type))) {
            *num_vals = 0;
            return rc;
        }
        if (OPAL_INT32 != local_type) { /* if the length wasn't first, then error */
            *num_vals = 0;
            return OPAL_ERR_UNPACK_FAILURE;
        }
    }

    n=1;
    if (OPAL_SUCCESS != (rc = opal_dss_unpack_int32(buffer, &local_num, &n, OPAL_INT32))) {
        *num_vals = 0;
        return rc;
    }

    /** if the storage provided is inadequate, set things up
     * to unpack as much as we can and to return an error code
     * indicating that everything was not unpacked - the buffer
     * is left in a state where it can not be further unpacked.
     */
    if (local_num > *num_vals) {
        local_num = *num_vals;
        ret = OPAL_ERR_UNPACK_INADEQUATE_SPACE;
    } else {  /** enough or more than enough storage */
        *num_vals = local_num;  /** let the user know how many we actually unpacked */
        ret = OPAL_SUCCESS;
    }

    /** Unpack the value(s) */
    if (OPAL_SUCCESS != (rc = opal_dss_unpack_buffer(buffer, dst, &local_num, type))) {
        *num_vals = 0;
        ret = rc;
    }

    return ret;
}

int opal_dss_unpack_buffer(opal_buffer_t *buffer, void *dst, int32_t *num_vals,
                    opal_data_type_t type)
{
    int rc;
    opal_data_type_t local_type;
    opal_dss_type_info_t *info;

    OPAL_OUTPUT( ( opal_dss_verbose, "opal_dss_unpack_buffer( %p, %p, %lu, %d )\n",
                   (void*)buffer, dst, (long unsigned int)*num_vals, (int)type ) );

    /** Unpack the declared data type */
    if (OPAL_DSS_BUFFER_FULLY_DESC == buffer->type) {
        if (OPAL_SUCCESS != (rc = opal_dss_get_data_type(buffer, &local_type))) {
            return rc;
        }
        /* if the data types don't match, then return an error */
        if (type != local_type) {
            opal_output(0, "OPAL dss:unpack: got type %d when expecting type %d", local_type, type);
            return OPAL_ERR_PACK_MISMATCH;
        }
    }

    /* Lookup the unpack function for this type and call it */

    if (NULL == (info = (opal_dss_type_info_t*)opal_pointer_array_get_item(&opal_dss_types, type))) {
        return OPAL_ERR_UNPACK_FAILURE;
    }

    return info->odti_unpack_fn(buffer, dst, num_vals, type);
}


/* UNPACK GENERIC SYSTEM TYPES */

/*
 * BOOL
 */
int opal_dss_unpack_bool(opal_buffer_t *buffer, void *dest,
                         int32_t *num_vals, opal_data_type_t type)
{
    int ret;
    opal_data_type_t remote_type;

    if (OPAL_DSS_BUFFER_FULLY_DESC == buffer->type) {
        /* see what type was actually packed */
        if (OPAL_SUCCESS != (ret = opal_dss_peek_type(buffer, &remote_type))) {
            return ret;
        }
    } else {
        if (OPAL_SUCCESS != (ret = opal_dss_get_data_type(buffer, &remote_type))) {
            return ret;
        }
    }

    if (remote_type == DSS_TYPE_BOOL) {
        /* fast path it if the sizes are the same */
        /* Turn around and unpack the real type */
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, dest, num_vals, DSS_TYPE_BOOL))) {
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
int opal_dss_unpack_int(opal_buffer_t *buffer, void *dest,
                        int32_t *num_vals, opal_data_type_t type)
{
    int ret;
    opal_data_type_t remote_type;

    if (OPAL_DSS_BUFFER_FULLY_DESC == buffer->type) {
        /* see what type was actually packed */
        if (OPAL_SUCCESS != (ret = opal_dss_peek_type(buffer, &remote_type))) {
            return ret;
        }
    } else {
        if (OPAL_SUCCESS != (ret = opal_dss_get_data_type(buffer, &remote_type))) {
            return ret;
        }
    }

    if (remote_type == DSS_TYPE_INT) {
        /* fast path it if the sizes are the same */
        /* Turn around and unpack the real type */
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, dest, num_vals, DSS_TYPE_INT))) {
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
int opal_dss_unpack_sizet(opal_buffer_t *buffer, void *dest,
                          int32_t *num_vals, opal_data_type_t type)
{
    int ret;
    opal_data_type_t remote_type;

    if (OPAL_DSS_BUFFER_FULLY_DESC == buffer->type) {
        /* see what type was actually packed */
        if (OPAL_SUCCESS != (ret = opal_dss_peek_type(buffer, &remote_type))) {
            return ret;
        }
    } else {
        if (OPAL_SUCCESS != (ret = opal_dss_get_data_type(buffer, &remote_type))) {
            return ret;
        }
    }

    if (remote_type == DSS_TYPE_SIZE_T) {
        /* fast path it if the sizes are the same */
        /* Turn around and unpack the real type */
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, dest, num_vals, DSS_TYPE_SIZE_T))) {
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
int opal_dss_unpack_pid(opal_buffer_t *buffer, void *dest,
                        int32_t *num_vals, opal_data_type_t type)
{
    int ret;
    opal_data_type_t remote_type;

    if (OPAL_DSS_BUFFER_FULLY_DESC == buffer->type) {
        /* see what type was actually packed */
        if (OPAL_SUCCESS != (ret = opal_dss_peek_type(buffer, &remote_type))) {
            return ret;
        }
    } else {
        if (OPAL_SUCCESS != (ret = opal_dss_get_data_type(buffer, &remote_type))) {
            return ret;
        }
    }

    if (remote_type == DSS_TYPE_PID_T) {
        /* fast path it if the sizes are the same */
        /* Turn around and unpack the real type */
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, dest, num_vals, DSS_TYPE_PID_T))) {
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
int opal_dss_unpack_null(opal_buffer_t *buffer, void *dest,
                         int32_t *num_vals, opal_data_type_t type)
{
    OPAL_OUTPUT( ( opal_dss_verbose, "opal_dss_unpack_null * %d\n", (int)*num_vals ) );
    /* check to see if there's enough data in buffer */
    if (opal_dss_too_small(buffer, *num_vals)) {
        return OPAL_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    }

    /* unpack the data */
    memcpy(dest, buffer->unpack_ptr, *num_vals);

    /* update buffer pointer */
    buffer->unpack_ptr += *num_vals;

    return OPAL_SUCCESS;
}

/*
 * BYTE, CHAR, INT8
 */
int opal_dss_unpack_byte(opal_buffer_t *buffer, void *dest,
                         int32_t *num_vals, opal_data_type_t type)
{
    OPAL_OUTPUT( ( opal_dss_verbose, "opal_dss_unpack_byte * %d\n", (int)*num_vals ) );
    /* check to see if there's enough data in buffer */
    if (opal_dss_too_small(buffer, *num_vals)) {
        return OPAL_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    }

    /* unpack the data */
    memcpy(dest, buffer->unpack_ptr, *num_vals);

    /* update buffer pointer */
    buffer->unpack_ptr += *num_vals;

    return OPAL_SUCCESS;
}

int opal_dss_unpack_int16(opal_buffer_t *buffer, void *dest,
                          int32_t *num_vals, opal_data_type_t type)
{
    int32_t i;
    uint16_t tmp, *desttmp = (uint16_t*) dest;

   OPAL_OUTPUT( ( opal_dss_verbose, "opal_dss_unpack_int16 * %d\n", (int)*num_vals ) );
    /* check to see if there's enough data in buffer */
    if (opal_dss_too_small(buffer, (*num_vals)*sizeof(tmp))) {
        return OPAL_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    }

    /* unpack the data */
    for (i = 0; i < (*num_vals); ++i) {
        memcpy( &(tmp), buffer->unpack_ptr, sizeof(tmp) );
        desttmp[i] = ntohs(tmp);
        buffer->unpack_ptr += sizeof(tmp);
    }

    return OPAL_SUCCESS;
}

int opal_dss_unpack_int32(opal_buffer_t *buffer, void *dest,
                          int32_t *num_vals, opal_data_type_t type)
{
    int32_t i;
    uint32_t tmp, *desttmp = (uint32_t*) dest;

   OPAL_OUTPUT( ( opal_dss_verbose, "opal_dss_unpack_int32 * %d\n", (int)*num_vals ) );
    /* check to see if there's enough data in buffer */
    if (opal_dss_too_small(buffer, (*num_vals)*sizeof(tmp))) {
        return OPAL_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    }

    /* unpack the data */
    for (i = 0; i < (*num_vals); ++i) {
        memcpy( &(tmp), buffer->unpack_ptr, sizeof(tmp) );
        desttmp[i] = ntohl(tmp);
        buffer->unpack_ptr += sizeof(tmp);
    }

    return OPAL_SUCCESS;
}

int opal_dss_unpack_int64(opal_buffer_t *buffer, void *dest,
                          int32_t *num_vals, opal_data_type_t type)
{
    int32_t i;
    uint64_t tmp, *desttmp = (uint64_t*) dest;

   OPAL_OUTPUT( ( opal_dss_verbose, "opal_dss_unpack_int64 * %d\n", (int)*num_vals ) );
    /* check to see if there's enough data in buffer */
    if (opal_dss_too_small(buffer, (*num_vals)*sizeof(tmp))) {
        return OPAL_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    }

    /* unpack the data */
    for (i = 0; i < (*num_vals); ++i) {
        memcpy( &(tmp), buffer->unpack_ptr, sizeof(tmp) );
        desttmp[i] = ntoh64(tmp);
        buffer->unpack_ptr += sizeof(tmp);
    }

    return OPAL_SUCCESS;
}

int opal_dss_unpack_string(opal_buffer_t *buffer, void *dest,
                           int32_t *num_vals, opal_data_type_t type)
{
    int ret;
    int32_t i, len, n=1;
    char **sdest = (char**) dest;

    for (i = 0; i < (*num_vals); ++i) {
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_int32(buffer, &len, &n, OPAL_INT32))) {
            return ret;
        }
        if (0 ==  len) {   /* zero-length string - unpack the NULL */
            sdest[i] = NULL;
        } else {
        sdest[i] = (char*)malloc(len);
            if (NULL == sdest[i]) {
                return OPAL_ERR_OUT_OF_RESOURCE;
            }
            if (OPAL_SUCCESS != (ret = opal_dss_unpack_byte(buffer, sdest[i], &len, OPAL_BYTE))) {
                return ret;
            }
        }
    }

    return OPAL_SUCCESS;
}


/* UNPACK FUNCTIONS FOR GENERIC OPAL TYPES */

/*
 * OPAL_DATA_TYPE
 */
int opal_dss_unpack_data_type(opal_buffer_t *buffer, void *dest, int32_t *num_vals,
                             opal_data_type_t type)
{
     /* turn around and unpack the real type */
    return opal_dss_unpack_buffer(buffer, dest, num_vals, OPAL_DATA_TYPE_T);
}

/*
 * OPAL_BYTE_OBJECT
 */
int opal_dss_unpack_byte_object(opal_buffer_t *buffer, void *dest, int32_t *num,
                             opal_data_type_t type)
{
    int ret;
    int32_t i, n, m=1;
    opal_byte_object_t **dbyteptr;

    dbyteptr = (opal_byte_object_t**)dest;
    n = *num;
    for(i=0; i<n; i++) {
        /* allocate memory for the byte object itself */
        dbyteptr[i] = (opal_byte_object_t*)malloc(sizeof(opal_byte_object_t));
        if (NULL == dbyteptr[i]) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }

        /* unpack object size in bytes */
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_int32(buffer, &(dbyteptr[i]->size), &m, OPAL_INT32))) {
            return ret;
        }
        if (0 < dbyteptr[i]->size) {
            dbyteptr[i]->bytes = (uint8_t*)malloc(dbyteptr[i]->size);
            if (NULL == dbyteptr[i]->bytes) {
                return OPAL_ERR_OUT_OF_RESOURCE;
            }
            if (OPAL_SUCCESS != (ret = opal_dss_unpack_byte(buffer, (dbyteptr[i]->bytes),
                                            &(dbyteptr[i]->size), OPAL_BYTE))) {
                return ret;
            }
        } else {
            /* be sure to init the bytes pointer to NULL! */
            dbyteptr[i]->bytes = NULL;
        }
    }

    return OPAL_SUCCESS;
}

static int opal_dss_unpack_float(opal_buffer_t *buffer, float *value)
{
    int32_t m, tmp1, tmp2;
    int ret;

    *value = 0.0;
    m=1;
    if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &tmp1, &m, OPAL_INT32))) {
        OPAL_ERROR_LOG(ret);
        return ret;
    }
    m=1;
    if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &tmp2, &m, OPAL_INT32))) {
        OPAL_ERROR_LOG(ret);
        return ret;
    }
    *value = (float)tmp1 + (float)tmp2/100.0;
    return OPAL_SUCCESS;
}

/*
 * OPAL_PSTAT
 */
int opal_dss_unpack_pstat(opal_buffer_t *buffer, void *dest,
                          int32_t *num_vals, opal_data_type_t type)
{
    opal_pstats_t **ptr;
    int32_t i, n, m, tmp1, tmp2;
    int ret;
    char *cptr;
    
    ptr = (opal_pstats_t **) dest;
    n = *num_vals;
    
    for (i = 0; i < n; ++i) {
        /* allocate the new object */
        ptr[i] = OBJ_NEW(opal_pstats_t);
        if (NULL == ptr[i]) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
        m=1;
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &cptr, &m, OPAL_STRING))) {
            OPAL_ERROR_LOG(ret);
            return ret;
        }
        memmove(ptr[i]->node, cptr, strlen(cptr));
        free(cptr);
        m=1;
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &ptr[i]->rank, &m, OPAL_INT32))) {
            OPAL_ERROR_LOG(ret);
            return ret;
        }
        m=1;
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &ptr[i]->pid, &m, OPAL_PID))) {
            OPAL_ERROR_LOG(ret);
            return ret;
        }
        m=1;
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &cptr, &m, OPAL_STRING))) {
            OPAL_ERROR_LOG(ret);
            return ret;
        }
        memmove(ptr[i]->cmd, cptr, strlen(cptr));
        free(cptr);
        m=1;
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &ptr[i]->state[0], &m, OPAL_BYTE))) {
            OPAL_ERROR_LOG(ret);
            return ret;
        }
        m=1;
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &tmp1, &m, OPAL_INT32))) {
            OPAL_ERROR_LOG(ret);
            return ret;
        }
        m=1;
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &tmp2, &m, OPAL_INT32))) {
            OPAL_ERROR_LOG(ret);
            return ret;
        }
        ptr[i]->time.tv_sec = tmp1;
        ptr[i]->time.tv_usec = tmp2;
        m=1;
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &ptr[i]->priority, &m, OPAL_INT32))) {
            OPAL_ERROR_LOG(ret);
            return ret;
        }
        m=1;
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &ptr[i]->num_threads, &m, OPAL_INT16))) {
            OPAL_ERROR_LOG(ret);
            return ret;
        }
        m=1;
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_float(buffer, &ptr[i]->vsize))) {
            OPAL_ERROR_LOG(ret);
            return ret;
        }
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_float(buffer, &ptr[i]->rss))) {
            OPAL_ERROR_LOG(ret);
            return ret;
        }
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_float(buffer, &ptr[i]->peak_vsize))) {
            OPAL_ERROR_LOG(ret);
            return ret;
        }
        m=1;
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &ptr[i]->processor, &m, OPAL_INT16))) {
            OPAL_ERROR_LOG(ret);
            return ret;
        }
        m=1;
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &tmp1, &m, OPAL_INT32))) {
            OPAL_ERROR_LOG(ret);
            return ret;
        }
        m=1;
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &tmp2, &m, OPAL_INT32))) {
            OPAL_ERROR_LOG(ret);
            return ret;
        }
        ptr[i]->sample_time.tv_sec = tmp1;
        ptr[i]->sample_time.tv_usec = tmp2;
    }
    
    return OPAL_SUCCESS;
}

/*
 * OPAL_NODE_STAT
 */
int opal_dss_unpack_node_stat(opal_buffer_t *buffer, void *dest,
                              int32_t *num_vals, opal_data_type_t type)
{
    opal_node_stats_t **ptr;
    int32_t i, n, m, tmp1, tmp2;
    int ret;
    
    ptr = (opal_node_stats_t **) dest;
    n = *num_vals;
    
    for (i = 0; i < n; ++i) {
        /* allocate the new object */
        ptr[i] = OBJ_NEW(opal_node_stats_t);
        if (NULL == ptr[i]) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_float(buffer, &ptr[i]->la))) {
            OPAL_ERROR_LOG(ret);
            return ret;
        }
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_float(buffer, &ptr[i]->la5))) {
            OPAL_ERROR_LOG(ret);
            return ret;
        }
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_float(buffer, &ptr[i]->la15))) {
            OPAL_ERROR_LOG(ret);
            return ret;
        }
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_float(buffer, &ptr[i]->total_mem))) {
            OPAL_ERROR_LOG(ret);
            return ret;
        }
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_float(buffer, &ptr[i]->free_mem))) {
            OPAL_ERROR_LOG(ret);
            return ret;
        }
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_float(buffer, &ptr[i]->buffers))) {
            OPAL_ERROR_LOG(ret);
            return ret;
        }
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_float(buffer, &ptr[i]->cached))) {
            OPAL_ERROR_LOG(ret);
            return ret;
        }
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_float(buffer, &ptr[i]->swap_cached))) {
            OPAL_ERROR_LOG(ret);
            return ret;
        }
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_float(buffer, &ptr[i]->swap_total))) {
            OPAL_ERROR_LOG(ret);
            return ret;
        }
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_float(buffer, &ptr[i]->swap_free))) {
            OPAL_ERROR_LOG(ret);
            return ret;
        }
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_float(buffer, &ptr[i]->mapped))) {
            OPAL_ERROR_LOG(ret);
            return ret;
        }
        m=1;
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &tmp1, &m, OPAL_INT32))) {
            OPAL_ERROR_LOG(ret);
            return ret;
        }
        m=1;
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &tmp2, &m, OPAL_INT32))) {
            OPAL_ERROR_LOG(ret);
            return ret;
        }
        ptr[i]->sample_time.tv_sec = tmp1;
        ptr[i]->sample_time.tv_usec = tmp2;
    }
    
    return OPAL_SUCCESS;
}

/*
 * OPAL_VALUE
 */
int opal_dss_unpack_value(opal_buffer_t *buffer, void *dest,
                          int32_t *num_vals, opal_data_type_t type)
{
    opal_value_t **ptr;
    int32_t i, n, m;
    int ret;

    ptr = (opal_value_t **) dest;
    n = *num_vals;
    
    for (i = 0; i < n; ++i) {
        /* allocate the new object */
        ptr[i] = OBJ_NEW(opal_value_t);
        if (NULL == ptr[i]) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
        /* unpack the key and type */
        m=1;
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_string(buffer, &ptr[i]->key, &m, OPAL_STRING))) {
            return ret;
        }
        m=1;
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_data_type(buffer, &ptr[i]->type, &m, OPAL_DATA_TYPE))) {
            return ret;
        }
        /* now unpack the right field */
        m=1;
        switch (ptr[i]->type) {
        case OPAL_BYTE:
            if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &ptr[i]->data.byte, &m, OPAL_BYTE))) {
                return ret;
            }
            break;
        case OPAL_STRING:
            if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &ptr[i]->data.string, &m, OPAL_STRING))) {
                return ret;
            }
            break;
        case OPAL_PID:
            if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &ptr[i]->data.pid, &m, OPAL_PID))) {
                return ret;
            }
            break;
        case OPAL_INT:
            if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &ptr[i]->data.integer, &m, OPAL_INT))) {
                return ret;
            }
            break;
        case OPAL_INT8:
            if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &ptr[i]->data.int8, &m, OPAL_INT8))) {
                return ret;
            }
            break;
        case OPAL_INT16:
            if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &ptr[i]->data.int16, &m, OPAL_INT16))) {
                return ret;
            }
            break;
        case OPAL_INT32:
            if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &ptr[i]->data.int32, &m, OPAL_INT32))) {
                return ret;
            }
            break;
        case OPAL_INT64:
            if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &ptr[i]->data.int64, &m, OPAL_INT64))) {
                return ret;
            }
            break;
        case OPAL_UINT:
            if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &ptr[i]->data.uint, &m, OPAL_UINT))) {
                return ret;
            }
            break;
        case OPAL_UINT8:
            if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &ptr[i]->data.uint8, &m, OPAL_UINT8))) {
                return ret;
            }
            break;
        case OPAL_UINT16:
            if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &ptr[i]->data.uint16, &m, OPAL_UINT16))) {
                return ret;
            }
            break;
        case OPAL_UINT32:
            if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &ptr[i]->data.uint32, &m, OPAL_UINT32))) {
                return ret;
            }
            break;
        case OPAL_UINT64:
            if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &ptr[i]->data.uint64, &m, OPAL_UINT64))) {
                return ret;
            }
            break;
        case OPAL_BYTE_OBJECT:
            /* cannot use byte object unpack as it allocates memory, so unpack object size in bytes */
            if (OPAL_SUCCESS != (ret = opal_dss_unpack_int32(buffer, &(ptr[i]->data.bo.size), &m, OPAL_INT32))) {
                return ret;
            }
            if (0 < ptr[i]->data.bo.size) {
                ptr[i]->data.bo.bytes = (uint8_t*)malloc(ptr[i]->data.bo.size);
                if (NULL == ptr[i]->data.bo.bytes) {
                    return OPAL_ERR_OUT_OF_RESOURCE;
                }
                if (OPAL_SUCCESS != (ret = opal_dss_unpack_byte(buffer, ptr[i]->data.bo.bytes,
                                                                &(ptr[i]->data.bo.size), OPAL_BYTE))) {
                    return ret;
                }
            } else {
                ptr[i]->data.bo.bytes = NULL;
            }
            break;
        default:
            opal_output(0, "PACK-OPAL-VALUE: UNSUPPORTED TYPE");
            return OPAL_ERROR;
        }
    }

    return OPAL_SUCCESS;
}

/*
 * OPAL_BUFFER
 */
int opal_dss_unpack_buffer_contents(opal_buffer_t *buffer, void *dest,
                                    int32_t *num_vals, opal_data_type_t type)
{
    opal_buffer_t **ptr;
    int32_t i, n, m;
    int ret;
    size_t nbytes;

    ptr = (opal_buffer_t **) dest;
    n = *num_vals;
    
    for (i = 0; i < n; ++i) {
        /* allocate the new object */
        ptr[i] = OBJ_NEW(opal_buffer_t);
        if (NULL == ptr[i]) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
        /* unpack the number of bytes */
        m=1;
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_sizet(buffer, &nbytes, &m, OPAL_SIZE))) {
            return ret;
        }
        /* setup the buffer's data region */
        if (0 < nbytes) {
            ptr[i]->base_ptr = (char*)malloc(nbytes);
        }
        /* unpack the bytes */
        m=nbytes;
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_byte(buffer, ptr[i]->base_ptr, &m, OPAL_BYTE))) {
            return ret;
        }
        /* fill-in the metadata */
        ptr[i]->pack_ptr = ptr[i]->base_ptr + m;
        ptr[i]->unpack_ptr = ptr[i]->base_ptr;
        ptr[i]->bytes_allocated = nbytes;
        ptr[i]->bytes_used = m;
    }
    return OPAL_SUCCESS;
}
