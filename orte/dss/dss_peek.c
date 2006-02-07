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

#include "orte/mca/errmgr/errmgr.h"

#include "orte/dss/dss_internal.h"


int orte_dss_peek(orte_buffer_t *buffer, orte_data_type_t *type,
                  size_t *num_vals)
{
    int ret;
    orte_buffer_t tmp;
    size_t n=1;
    orte_data_type_t local_type;

    /* check for errors */
    if (buffer == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    /* Double check and ensure that there is data left in the buffer. */

    if (buffer->unpack_ptr >= buffer->base_ptr + buffer->bytes_used) {
        *type = ORTE_NULL;
        *num_vals = 0;
        return ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    }

    /* cheat: unpack from a copy of the buffer -- leaving all the
       original pointers intact */
    tmp = *buffer;

    if (ORTE_SUCCESS != (
        ret = orte_dss_get_data_type(&tmp, &local_type))) {
        *type = ORTE_NULL;
        *num_vals = 0;
        return ret;
    }
    if (ORTE_SIZE != local_type) { /* if the length wasn't first, then error */
        ORTE_ERROR_LOG(ORTE_ERR_UNPACK_FAILURE);
        *type = ORTE_NULL;
        *num_vals = 0;
        return ORTE_ERR_UNPACK_FAILURE;
    }
    if (ORTE_SUCCESS != (ret = orte_dss_unpack_sizet(&tmp, num_vals, &n, ORTE_SIZE))) {
        ORTE_ERROR_LOG(ret);
        *type = ORTE_NULL;
        *num_vals = 0;
        return ret;
    }
    if (ORTE_SUCCESS != (ret = orte_dss_get_data_type(&tmp, type))) {
        ORTE_ERROR_LOG(ret);
        *type = ORTE_NULL;
        *num_vals = 0;
    }

    return ret;
}

int orte_dss_peek_type(orte_buffer_t *buffer, orte_data_type_t *type)
{
    int ret;
    orte_buffer_t tmp;
    size_t n=1;

    /* check for errors */
    if (buffer == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    /* Double check and ensure that there is data left in the buffer. */

    if (buffer->unpack_ptr >= buffer->base_ptr + buffer->bytes_used) {
        *type = ORTE_UNDEF;
        return ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    }

    /* cheat: unpack from a copy of the buffer -- leaving all the
    original pointers intact */
    tmp = *buffer;

    if (ORTE_SUCCESS != (
        ret = orte_dss_get_data_type(&tmp, type))) {
        *type = ORTE_UNDEF;
        return ret;
    }

    return ORTE_SUCCESS;
}
