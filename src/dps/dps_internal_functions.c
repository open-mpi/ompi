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

#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <string.h>
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h> 
#endif

#include "util/output.h"
#include "mca/errmgr/errmgr.h"

#include "dps/dps_internal.h"

/**
 * Internal function that resizes (expands) an inuse buffer if
 * necessary.
 */
char* orte_dps_buffer_extend(orte_buffer_t *buffer, size_t bytes_to_add)
{
    size_t required, num_pages;
    size_t pack_offset, unpack_offset;

    /* Check to see if we have enough space already */

    if (buffer->bytes_avail >= bytes_to_add) {
        return buffer->pack_ptr;
    }

    /* If we don't, see how many pages will be required and alloc
       that */

    required = buffer->bytes_used + bytes_to_add;
    num_pages = required / orte_dps_page_size;
    if (0 != required % orte_dps_page_size) {
        ++num_pages;
    }
    if (NULL != buffer->base_ptr) {
        pack_offset = ((char*) buffer->pack_ptr) - ((char*) buffer->base_ptr);
        unpack_offset = ((char*) buffer->unpack_ptr) -
            ((char*) buffer->base_ptr);
        buffer->base_ptr = realloc(buffer->base_ptr, 
                                   num_pages * orte_dps_page_size);
    } else {
        pack_offset = 0;
        unpack_offset = 0;
        buffer->bytes_used = 0;
        buffer->base_ptr = malloc(num_pages * orte_dps_page_size);
    }
    
    if (NULL == buffer->base_ptr) { 
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return NULL; 
    }
    buffer->pack_ptr = ((char*) buffer->base_ptr) + pack_offset;
    buffer->unpack_ptr = ((char*) buffer->base_ptr) + unpack_offset;
    buffer->bytes_allocated = num_pages * orte_dps_page_size;
    buffer->bytes_avail = buffer->bytes_allocated - buffer->bytes_used;
    
    /* All done */

    return buffer->pack_ptr;
}

/*
 * Internal function that checks to see if the specified number of bytes
 * remain in the buffer for unpacking
 */
bool orte_dps_too_small(orte_buffer_t *buffer, size_t bytes_reqd)
{
    size_t bytes_remaining_packed;
    
    if (buffer->pack_ptr < buffer->unpack_ptr) {
        ORTE_ERROR_LOG(ORTE_ERR_UNPACK_FAILURE);
        return true;
    }

    bytes_remaining_packed = buffer->pack_ptr - buffer->unpack_ptr;
    
    if (bytes_remaining_packed < bytes_reqd) {
        ORTE_ERROR_LOG(ORTE_UNPACK_READ_PAST_END_OF_BUFFER);
        return true;
    }

    return false;
}

/*
 * Internal function to store data type in buffer
 */
int orte_dps_store_data_type(orte_buffer_t *buffer, orte_data_type_t type)
{
    size_t required;
    int rc;
    
    required = sizeof(orte_data_type_t);
    switch (required) {
    
        case 1:
            if (ORTE_SUCCESS != (
                rc = orte_dps_pack_byte(buffer, &type, 1, ORTE_BYTE))) {
                ORTE_ERROR_LOG(rc);
            }
            break;
        
        case 2:
            if (ORTE_SUCCESS != (
                rc = orte_dps_pack_int16(buffer, &type, 1, ORTE_INT16))) {
                ORTE_ERROR_LOG(rc);
            }
            break;
        
        case 4:
            if (ORTE_SUCCESS != (
                rc = orte_dps_pack_int32(buffer, &type, 1, ORTE_INT32))) {
                ORTE_ERROR_LOG(rc);
            }
            break;
        
        case 8:
            if (ORTE_SUCCESS != (
                rc = orte_dps_pack_int64(buffer, &type, 1, ORTE_INT64))) {
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
 * Internal function to retrieve data type from buffer
 */
int orte_dps_get_data_type(orte_buffer_t *buffer, orte_data_type_t *type)
{
    size_t required;
    int rc;
    
    switch (required) {
    
        case 1:
            if (ORTE_SUCCESS != (
                rc = orte_dps_unpack_byte(buffer, type, 1, ORTE_BYTE))) {
                ORTE_ERROR_LOG(rc);
            }
            break;
        
        case 2:
            if (ORTE_SUCCESS != (
                rc = orte_dps_unpack_int16(buffer, type, 1, ORTE_INT16))) {
                ORTE_ERROR_LOG(rc);
            }
            break;
        
        case 4:
            if (ORTE_SUCCESS != (
                rc = orte_dps_unpack_int32(buffer, type, 1, ORTE_INT32))) {
                ORTE_ERROR_LOG(rc);
            }
            break;
        
        case 8:
            if (ORTE_SUCCESS != (
                rc = orte_dps_unpack_int64(buffer, type, 1, ORTE_INT64))) {
                ORTE_ERROR_LOG(rc);
            }
            break;
        
        default:
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
            return ORTE_ERR_BAD_PARAM;
    }

    return rc;
}
