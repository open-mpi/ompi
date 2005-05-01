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
#include "include/orte_types.h"

#include <sys/types.h>
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#include <string.h>

#include "mca/errmgr/errmgr.h"
#include "mca/ns/ns.h"

#include "dps/dps_internal.h"


int orte_dps_unpack(orte_buffer_t *buffer, void *dst, size_t *num_vals,
                    orte_data_type_t type)
{
    int ret=ORTE_SUCCESS, rc=ORTE_SUCCESS;
    size_t local_num;

    /* check for error */
    if (NULL == buffer || NULL == dst || NULL == num_vals || 0 > *num_vals) { 
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    /* if user provides a zero for num_vals, then there is no storage allocated
     * so return an appropriate error
     */
    if (0 == *num_vals) {
        ORTE_ERROR_LOG(ORTE_UNPACK_INADEQUATE_SPACE);
        return ORTE_UNPACK_INADEQUATE_SPACE;
    }

    /* Unpack the stored number of values
     * REMINDER: it is possible that the buffer is corrupted and that
     * the DPS will *think* there is a proper size_t variable at the
     * beginning of the unpack region - but that the value is bogus (e.g., just
     * a byte field in a string array that so happens to have a value that
     * matches the size_t data type flag). Therefore, this error check is
     * NOT completely safe. This is true for ALL unpack functions, not just
     * size_t as used here.
     */
    if (ORTE_SUCCESS != (
        rc = orte_dps_unpack_sizet(buffer, &local_num, 1, ORTE_SIZE))) {
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
        ORTE_ERROR_LOG(ORTE_UNPACK_INADEQUATE_SPACE);
        local_num = *num_vals;
        ret = ORTE_UNPACK_INADEQUATE_SPACE;
    } else if (local_num < *num_vals) {  /* more than enough storage */
        *num_vals = local_num;  /* let the user know how many we actually unpacked */
    }

    /* Unpack the value(s) */
    if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, dst, local_num, type))) {
        ORTE_ERROR_LOG(rc);
        *num_vals = 0;
    }
    
    if (ORTE_SUCCESS != ret) {
        return ret;
    }
    
    return rc;
}

int orte_dps_unpack_buffer(orte_buffer_t *buffer, void *dst, size_t num_vals,
                    orte_data_type_t type)
{
    int rc;
    orte_data_type_t local_type;

    /* Unpack the data type */
    if (ORTE_SUCCESS != (rc = orte_dps_get_data_type(buffer, &local_type))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* if the data types don't match, then return an error */
    if (type != local_type) {
        ORTE_ERROR_LOG(ORTE_PACK_MISMATCH);
        return ORTE_PACK_MISMATCH;
    }

    /* Lookup the unpack function for this type and call it */
    
    info = orte_value_array_get_item(&orte_dps_types, type);
    if (ORTE_SUCCESS != (rc = info->odti_unpack_fn(buffer, dst, num_vals, type))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}


/* UNPACK GENERIC SYSTEM TYPES */

/*
 * NULL
 */
int orte_dps_unpack_null(orte_buffer_t *buffer, void *dest,
                         size_t num_vals, orte_data_type_t type)
{
    int rc;
    
    /* Turn around and unpack the real type */
    if (ORTE_SUCCESS != (rc = orte_dps_unpack_byte(buffer, dest, num_vals, ORTE_BYTE))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

/*
 * BOOL
 */
int orte_dps_unpack_bool(orte_buffer_t *buffer, void *dest,
                         size_t num_vals, orte_data_type_t type)
{
    int ret;

    /* Turn around and unpack the real type */
    if (ORTE_SUCCESS != (
        ret = orte_dps_unpack_buffer(buffer, dest, num_vals, DPS_TYPE_BOOL))) {
        ORTE_ERROR_LOG(ret);
    }

    return ret;
}

/*
 * INT
 */
int orte_dps_unpack_int(orte_buffer_t *buffer, void *dest,
                        size_t num_vals, orte_data_type_t type)
{
    int ret;

    /* Turn around and unpack the real type */
    if (ORTE_SUCCESS != (
        ret = orte_dps_unpack_buffer(buffer, dest, num_vals, DPS_TYPE_INT))) {
        ORTE_ERROR_LOG(ret);
    }

    return ret;
}

/*
 * SIZE_T
 */
int orte_dps_unpack_sizet(orte_buffer_t *buffer, void *dest,
                          size_t num_vals, orte_data_type_t type)
{
    int ret;

    /* Turn around and unpack the real type */
    if (ORTE_SUCCESS != (
        ret = orte_dps_unpack_buffer(buffer, dest, num_vals, DPS_TYPE_SIZE_T))) {
        ORTE_ERROR_LOG(ret);
    }

    return ret;
}


/* UNPACK FUNCTIONS FOR NON-GENERIC SYSTEM TYPES */

/*
 * BYTE, CHAR, INT8
 */
int orte_dps_unpack_byte(orte_buffer_t *buffer, void *dest,
                         size_t num_vals, orte_data_type_t type)
{
    /* check to see if there's enough data in buffer */
    if (orte_dps_too_small(buffer, num_vals) {
        ORTE_ERROR_LOG(ORTE_UNPACK_READ_PAST_END_OF_BUFFER);
        return ORTE_UNPACK_READ_PAST_END_OF_BUFFER;
    }
    
    /* unpack the data */
    memcpy(dest, buffer->unpack_ptr, num_vals);
    
    /* update buffer pointer */
    buffer->unpack_ptr += num_vals;

    return ORTE_SUCCESS;
}

int orte_dps_unpack_int16(orte_buffer_t *buffer, void *dest,
                          size_t num_vals, orte_data_type_t type)
{
    size_t i;
    uint16_t tmp, *desttmp = (uint16_t*) dest;

    /* check to see if there's enough data in buffer */
    if (orte_dps_too_small(buffer, num_vals*sizeof(tmp)) {
        ORTE_ERROR_LOG(ORTE_UNPACK_READ_PAST_END_OF_BUFFER);
        return ORTE_UNPACK_READ_PAST_END_OF_BUFFER;
    }
    
    /* unpack the data */
    for (i = 0; i < num_vals; ++i) {
        tmp = ntohs(desttmp[i]);
        memcpy(&desttmp[i], &tmp, sizeof(tmp));
    }
    buffer->unpack_ptr += num_vals * sizeof(tmp);
    
    return ORTE_SUCCESS;
}

int orte_dps_unpack_int32(orte_buffer_t *buffer, void *dest,
                          size_t num_vals, orte_data_type_t type)
{
    size_t i;
    uint32_t tmp, *desttmp = (uint32_t*) dest;

    /* check to see if there's enough data in buffer */
    if (orte_dps_too_small(buffer, num_vals*sizeof(tmp)) {
        ORTE_ERROR_LOG(ORTE_UNPACK_READ_PAST_END_OF_BUFFER);
        return ORTE_UNPACK_READ_PAST_END_OF_BUFFER;
    }
    
    /* unpack the data */
    for (i = 0; i < num_vals; ++i) {
        tmp = ntohl(desttmp[i]);
        memcpy(&tmp, &desttmp[i], sizeof(tmp));
    }
    buffer->unpack_ptr += num_vals * sizeof(tmp);
    
    return ret;
}

int orte_dps_unpack_int64(orte_buffer_t *buffer, void *dest,
                          size_t num_vals, orte_data_type_t type)
{
    int ret;
    size_t i;
    uint32_t tmp, *desttmp = (uint32_t*) dest;

    num_vals *= 2;
    
    /* check to see if there's enough data in buffer */
    if (orte_dps_too_small(buffer, num_vals*sizeof(tmp)) {
        ORTE_ERROR_LOG(ORTE_UNPACK_READ_PAST_END_OF_BUFFER);
        return ORTE_UNPACK_READ_PAST_END_OF_BUFFER;
    }
    
    /* unpack the data */
    for (i = 0; i < num_vals; ++i) {
        tmp = ntohl(desttmp[i]);
        memcpy(&tmp, &desttmp[i], sizeof(tmp));
    }
    buffer->unpack_ptr += num_vals * sizeof(tmp);
    
    return ret;
}

int orte_dps_unpack_string(orte_buffer_t *buffer, void *dest,
                           size_t num_vals, orte_data_type_t type)
{
    int ret;
    size_t i, len;
    char **sdest = (char**) dest;

    for (i = 0; i < num_vals; ++i) {
        if (ORTE_SUCCESS != (ret = orte_dps_unpack_sizet(buffer, &len, 1, ORTE_SIZE))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
        sdest[i] = malloc(len);
        if (NULL == sdest[i]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        if (ORTE_SUCCESS != (ret = orte_dps_unpack_byte(buffer, sdest[i], len, ORTE_BYTE))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
    }

    return ORTE_SUCCESS;
}
