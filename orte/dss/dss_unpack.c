/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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

#include "opal/util/output.h"

#include "orte/mca/errmgr/errmgr.h"

#include "orte/dss/dss_internal.h"

int orte_dss_unpack(orte_buffer_t *buffer, void *dst, size_t *num_vals,
                    orte_data_type_t type)
{
    int ret=ORTE_SUCCESS, rc=ORTE_SUCCESS;
    size_t local_num, n=1;
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

    /* Unpack the declared number of values
     * REMINDER: it is possible that the buffer is corrupted and that
     * the DSS will *think* there is a proper size_t variable at the
     * beginning of the unpack region - but that the value is bogus (e.g., just
     * a byte field in a string array that so happens to have a value that
     * matches the size_t data type flag). Therefore, this error check is
     * NOT completely safe. This is true for ALL unpack functions, not just
     * size_t as used here.
     */
    if (ORTE_SUCCESS != (
        rc = orte_dss_get_data_type(buffer, &local_type))) {
        ORTE_ERROR_LOG(rc);
        *num_vals = 0;
        return rc;
    }
    if (ORTE_SIZE != local_type) { /* if the length wasn't first, then error */
        ORTE_ERROR_LOG(ORTE_ERR_UNPACK_FAILURE);
        *num_vals = 0;
        return ORTE_ERR_UNPACK_FAILURE;
    }
    if (ORTE_SUCCESS != (
        rc = orte_dss_unpack_sizet(buffer, &local_num, &n, ORTE_SIZE))) {
        ORTE_ERROR_LOG(rc);
        *num_vals = 0;
        return rc;
    }

    /* if the storage provided is inadequate, set things up
     * to unpack as much as we can and to return an error code
     * indicating that everything was not unpacked - the buffer
     * is left in a state where it can not be further unpacked.
     */
    if (local_num > *num_vals) {
        ORTE_ERROR_LOG(ORTE_ERR_UNPACK_INADEQUATE_SPACE);
        local_num = *num_vals;
        ret = ORTE_ERR_UNPACK_INADEQUATE_SPACE;
    } else if (local_num < *num_vals) {  /* more than enough storage */
        *num_vals = local_num;  /* let the user know how many we actually unpacked */
    }

    /* Unpack the value(s) */
    if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, dst, &local_num, type))) {
        ORTE_ERROR_LOG(rc);
        *num_vals = 0;
    }

    if (ORTE_SUCCESS != ret) {
        return ret;
    }

    return rc;
}

int orte_dss_unpack_buffer(orte_buffer_t *buffer, void *dst, size_t *num_vals,
                    orte_data_type_t type)
{
    int rc;
    orte_data_type_t local_type;
    orte_dss_type_info_t *info;

    OPAL_OUTPUT( ( orte_dss_verbose, "orte_dss_unpack_buffer( %p, %p, %lu, %d )\n", buffer, dst, *num_vals, (int)type ) );
    /* Unpack the declared data type */
    if (ORTE_SUCCESS != (rc = orte_dss_get_data_type(buffer, &local_type))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* if the data types don't match, then return an error */
    if (type != local_type) {
        ORTE_ERROR_LOG(ORTE_ERR_PACK_MISMATCH);
        return ORTE_ERR_PACK_MISMATCH;
    }

    /* Lookup the unpack function for this type and call it */

    if (NULL == (info = orte_pointer_array_get_item(orte_dss_types, type))) {
        ORTE_ERROR_LOG(ORTE_ERR_UNPACK_FAILURE);
        return ORTE_ERR_UNPACK_FAILURE;
    }

    if (ORTE_SUCCESS != (rc = info->odti_unpack_fn(buffer, dst, num_vals, type))) {
        ORTE_ERROR_LOG(rc);
    }

    return rc;
}


/* UNPACK GENERIC SYSTEM TYPES */

/*
 * BOOL
 */
int orte_dss_unpack_bool(orte_buffer_t *buffer, void *dest,
                         size_t *num_vals, orte_data_type_t type)
{
    int ret;
    /* turn this off for now - just a prototype to think about */
#if 0
    orte_data_type_t remote_type;
    bool *tf = (bool*)dest;
    uint8_t *tfi8;
    uint16_t *tfi16;
    uint32_t *tfi32;
    uint64_t *tfi64;

    /* see what type was actually packed */
    if (ORTE_SUCCESS != (ret = orte_dss_peek_type(buffer, &remote_type))) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /* see if we have a size mismatch */
    if ((SIZEOF_BOOL == 1 && remote_type != ORTE_INT8) ||
        (SIZEOF_BOOL == 2 && remote_type != ORTE_INT16) ||
        (SIZEOF_BOOL == 4 && remote_type != ORTE_INT32) ||
        (SIZEOF_BOOL == 8 && remote_type != ORTE_INT64)) {
        /*  type mismatch exists - need to do something more complex */

        /* allocate enough space for the remote variables to be unpacked,
         * unpack the buffer to the new destination, and then converty the
         * remote value to the local type
         */
        switch(remote_type) {
            case ORTE_INT8:
                new_dest = (void*)malloc(*num_vals);
                if (NULL == new_dest) {
                    ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                    return ORTE_ERR_OUT_OF_RESOURCE;
                }
                if (ORTE_SUCCESS != (
                    ret = orte_dss_unpack_buffer(buffer, new_dest, num_vals, remote_type))) {
                    ORTE_ERROR_LOG(ret);
                    return ret;
                }
                tfi8 = (uint8_t*)new_dest;
                for (i=0; i < *num_vals; i++) {
                    *tf = (bool)*tfi8;
                    tf++;
                    tfi8++;
                }
                free(new_dest);
                break;

            case ORTE_INT16:
                new_dest = (void*)malloc(*num_vals * 2);
                if (NULL == new_dest) {
                    ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                    return ORTE_ERR_OUT_OF_RESOURCE;
                }
                if (ORTE_SUCCESS != (
                    ret = orte_dss_unpack_buffer(buffer, new_dest, num_vals, remote_type))) {
                    ORTE_ERROR_LOG(ret);
                    return ret;
                }
                tfi16 = (uint16_t*)new_dest;
                for (i=0; i < *num_vals; i++) {
                    *tf = (bool)*tfi16;
                    tf++;
                    tfi16++;
                }
                free(new_dest);
                break;

            case ORTE_INT32:
                new_dest = (void*)malloc(*num_vals * 4);
                if (NULL == new_dest) {
                    ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                    return ORTE_ERR_OUT_OF_RESOURCE;
                }
                if (ORTE_SUCCESS != (
                    ret = orte_dss_unpack_buffer(buffer, new_dest, num_vals, remote_type))) {
                    ORTE_ERROR_LOG(ret);
                    return ret;
                }
                tfi32 = (uint32_t*)new_dest;
                for (i=0; i < *num_vals; i++) {
                    *tf = (bool)*tfi32;
                    tf++;
                    tfi32++;
                }
                free(new_dest);
                break;

            case ORTE_INT64:
                new_dest = (void*)malloc(*num_vals * 8);
                if (NULL == new_dest) {
                    ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                    return ORTE_ERR_OUT_OF_RESOURCE;
                }
                if (ORTE_SUCCESS != (
                    ret = orte_dss_unpack_buffer(buffer, new_dest, num_vals, remote_type))) {
                    ORTE_ERROR_LOG(ret);
                    return ret;
                }
                tfi64 = (uint64_t*)new_dest;
                for (i=0; i < *num_vals; i++) {
                    *tf = (bool)*tfi64;
                    tf++;
                    tfi64++;
                }
                free(new_dest);
                break;
        }
    } else {  /* no size mismatch, so just go ahead and unpack */
        if (ORTE_SUCCESS != (
            ret = orte_dss_unpack_buffer(buffer, dest, num_vals, DSS_TYPE_BOOL))) {
            ORTE_ERROR_LOG(ret);
        }
    }
#endif

    if (ORTE_SUCCESS != (
        ret = orte_dss_unpack_buffer(buffer, dest, num_vals, DSS_TYPE_BOOL))) {
        ORTE_ERROR_LOG(ret);
    }

    return ret;
}

/*
 * INT
 */
int orte_dss_unpack_int(orte_buffer_t *buffer, void *dest,
                        size_t *num_vals, orte_data_type_t type)
{
    int ret;

    /* Turn around and unpack the real type */
    if (ORTE_SUCCESS != (
        ret = orte_dss_unpack_buffer(buffer, dest, num_vals, DSS_TYPE_INT))) {
        ORTE_ERROR_LOG(ret);
    }

    return ret;
}

/*
 * SIZE_T
 */
int orte_dss_unpack_sizet(orte_buffer_t *buffer, void *dest,
                          size_t *num_vals, orte_data_type_t type)
{
    int ret;

    /* Turn around and unpack the real type */
    if (ORTE_SUCCESS != (
        ret = orte_dss_unpack_buffer(buffer, dest, num_vals, DSS_TYPE_SIZE_T))) {
        ORTE_ERROR_LOG(ret);
    }

    return ret;
}

/*
 * PID_T
 */
int orte_dss_unpack_pid(orte_buffer_t *buffer, void *dest,
                        size_t *num_vals, orte_data_type_t type)
{
    int ret;

    /* Turn around and unpack the real type */
    if (ORTE_SUCCESS != (
        ret = orte_dss_unpack_buffer(buffer, dest, num_vals, DSS_TYPE_PID_T))) {
        ORTE_ERROR_LOG(ret);
    }

    return ret;
}


/* UNPACK FUNCTIONS FOR NON-GENERIC SYSTEM TYPES */

/*
 * NULL
 */
int orte_dss_unpack_null(orte_buffer_t *buffer, void *dest,
                         size_t *num_vals, orte_data_type_t type)
{
    OPAL_OUTPUT( ( orte_dss_verbose, "orte_dss_unpack_null * %d\n", (int)*num_vals ) );
    /* check to see if there's enough data in buffer */
    if (orte_dss_too_small(buffer, *num_vals)) {
        ORTE_ERROR_LOG(ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER);
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
                         size_t *num_vals, orte_data_type_t type)
{
    OPAL_OUTPUT( ( orte_dss_verbose, "orte_dss_unpack_byte * %d\n", (int)*num_vals ) );
    /* check to see if there's enough data in buffer */
    if (orte_dss_too_small(buffer, *num_vals)) {
        ORTE_ERROR_LOG(ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER);
        return ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    }

    /* unpack the data */
    memcpy(dest, buffer->unpack_ptr, *num_vals);

    /* update buffer pointer */
    buffer->unpack_ptr += *num_vals;

    return ORTE_SUCCESS;
}

int orte_dss_unpack_int16(orte_buffer_t *buffer, void *dest,
                          size_t *num_vals, orte_data_type_t type)
{
    size_t i;
    uint16_t tmp, *desttmp = (uint16_t*) dest;

   OPAL_OUTPUT( ( orte_dss_verbose, "orte_dss_unpack_int16 * %d\n", (int)*num_vals ) );
    /* check to see if there's enough data in buffer */
    if (orte_dss_too_small(buffer, (*num_vals)*sizeof(tmp))) {
        ORTE_ERROR_LOG(ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER);
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
                          size_t *num_vals, orte_data_type_t type)
{
    size_t i;
    uint32_t tmp, *desttmp = (uint32_t*) dest;

   OPAL_OUTPUT( ( orte_dss_verbose, "orte_dss_unpack_int32 * %d\n", (int)*num_vals ) );
    /* check to see if there's enough data in buffer */
    if (orte_dss_too_small(buffer, (*num_vals)*sizeof(tmp))) {
        ORTE_ERROR_LOG(ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER);
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
                          size_t *num_vals, orte_data_type_t type)
{
    size_t i;
    uint32_t tmp, *desttmp = (uint32_t*) dest;

   OPAL_OUTPUT( ( orte_dss_verbose, "orte_dss_unpack_int64 * %d\n", (int)*num_vals ) );
    /* check to see if there's enough data in buffer */
    if (orte_dss_too_small(buffer, 2*(*num_vals)*sizeof(tmp))) {
        ORTE_ERROR_LOG(ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER);
        return ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    }

    /* unpack the data */
    for (i = 0; i < (2 * (*num_vals)); i += 2) {
        memcpy( &(tmp), buffer->unpack_ptr, sizeof(tmp) );
        desttmp[i] = ntohl(tmp);
        buffer->unpack_ptr += sizeof(tmp);
        memcpy( &(tmp), buffer->unpack_ptr, sizeof(tmp) );
        desttmp[i+1] = ntohl(tmp);
        buffer->unpack_ptr += sizeof(tmp);
    }

    return ORTE_SUCCESS;
}

int orte_dss_unpack_string(orte_buffer_t *buffer, void *dest,
                           size_t *num_vals, orte_data_type_t type)
{
    int ret;
    size_t i, len, n=1;
    char **sdest = (char**) dest;

    for (i = 0; i < (*num_vals); ++i) {
        if (ORTE_SUCCESS != (ret = orte_dss_unpack_sizet(buffer, &len, &n, ORTE_SIZE))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
        if (0 ==  len) {   /* zero-length string - unpack the NULL */
            sdest[i] = NULL;
        } else {
        sdest[i] = malloc(len);
            if (NULL == sdest[i]) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            if (ORTE_SUCCESS != (ret = orte_dss_unpack_byte(buffer, sdest[i], &len, ORTE_BYTE))) {
                ORTE_ERROR_LOG(ret);
                return ret;
            }
        }
    }

    return ORTE_SUCCESS;
}


/* UNPACK FUNCTIONS FOR GENERIC ORTE TYPES */

/*
 * ORTE_DATA_TYPE
 */
int orte_dss_unpack_data_type(orte_buffer_t *buffer, void *dest, size_t *num,
                             orte_data_type_t type)
{
    size_t required;
    int rc;

    required = sizeof(orte_data_type_t);
    switch (required) {

        case 1:
            if (ORTE_SUCCESS != (
                rc = orte_dss_unpack_byte(buffer, dest, num, ORTE_BYTE))) {
                ORTE_ERROR_LOG(rc);
            }
            break;

        case 2:
            if (ORTE_SUCCESS != (
                rc = orte_dss_unpack_int16(buffer, dest, num, ORTE_INT16))) {
                ORTE_ERROR_LOG(rc);
            }
            break;

        case 4:
            if (ORTE_SUCCESS != (
                rc = orte_dss_unpack_int32(buffer, dest, num, ORTE_INT32))) {
                ORTE_ERROR_LOG(rc);
            }
            break;

        case 8:
            if (ORTE_SUCCESS != (
                rc = orte_dss_unpack_int64(buffer, dest, num, ORTE_INT64))) {
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
 * ORTE_DAEMON_CMD
 */
int orte_dss_unpack_daemon_cmd(orte_buffer_t *buffer, void *dest, size_t *num,
                               orte_data_type_t type)
{
    size_t required;
    int rc;

    required = sizeof(orte_daemon_cmd_flag_t);
    switch (required) {

        case 1:
            if (ORTE_SUCCESS != (
                rc = orte_dss_unpack_byte(buffer, dest, num, ORTE_BYTE))) {
                ORTE_ERROR_LOG(rc);
            }
            break;

        case 2:
            if (ORTE_SUCCESS != (
                rc = orte_dss_unpack_int16(buffer, dest, num, ORTE_INT16))) {
                ORTE_ERROR_LOG(rc);
            }
            break;

        case 4:
            if (ORTE_SUCCESS != (
                rc = orte_dss_unpack_int32(buffer, dest, num, ORTE_INT32))) {
                ORTE_ERROR_LOG(rc);
            }
            break;

        case 8:
            if (ORTE_SUCCESS != (
                rc = orte_dss_unpack_int64(buffer, dest, num, ORTE_INT64))) {
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
 * ORTE_DATA_VALUE
 */
int orte_dss_unpack_data_value(orte_buffer_t *buffer, void *dest, size_t *num,
                             orte_data_type_t type)
{
    orte_dss_type_info_t *info;
    orte_data_value_t **ddv;
    size_t i, n;
    int ret;

    ddv = (orte_data_value_t **) dest;

    for (i = 0; i < *num; ++i) {
        ddv[i] = OBJ_NEW(orte_data_value_t);
        if (NULL == ddv[i]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }

        /* see what the data type is */
        if (ORTE_SUCCESS != (ret = orte_dss_get_data_type(buffer, &(ddv[i]->type)))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }

        /* get enough memory to hold it */
        if (ORTE_SUCCESS != (ret = orte_dss.size(&n, NULL, ddv[i]->type))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
        ddv[i]->data = (void*)malloc(n);
        if (NULL == ddv[i]->data) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }

        /* Lookup the unpack function for this type and call it */

        if (NULL == (info = orte_pointer_array_get_item(orte_dss_types, ddv[i]->type))) {
            ORTE_ERROR_LOG(ORTE_ERR_PACK_FAILURE);
            return ORTE_ERR_PACK_FAILURE;
        }

        if (info->odti_structured) {
            n=1;
            if (ORTE_SUCCESS != (ret = orte_dss_unpack_buffer(buffer, &(ddv[i]->data), &n, ddv[i]->type))) {
                ORTE_ERROR_LOG(ret);
                return ret;
            }
        } else {
            n=1;
            if (ORTE_SUCCESS != (ret = orte_dss_unpack_buffer(buffer, ddv[i]->data, &n, ddv[i]->type))) {
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
int orte_dss_unpack_byte_object(orte_buffer_t *buffer, void *dest, size_t *num,
                             orte_data_type_t type)
{
    int ret;
    size_t i, n, m=1;
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
        if (ORTE_SUCCESS != (ret = orte_dss_unpack_sizet(buffer, &(dbyteptr[i]->size), &m, ORTE_SIZE))) {
            ORTE_ERROR_LOG(ret);
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
                ORTE_ERROR_LOG(ret);
                return ret;
            }
        }
    }

    return ORTE_SUCCESS;
}
