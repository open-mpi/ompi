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

#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h> 
#endif

#include "opal/util/output.h"

#include "orte/class/orte_pointer_array.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/dss/dss_internal.h"

/**
 * Internal function that resizes (expands) an inuse buffer if
 * necessary.
 */
char* orte_dss_buffer_extend(orte_buffer_t *buffer, size_t bytes_to_add)
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
    num_pages = required / orte_dss_page_size;
    if (0 != required % orte_dss_page_size) {
        ++num_pages;
    }
    if (NULL != buffer->base_ptr) {
        pack_offset = ((char*) buffer->pack_ptr) - ((char*) buffer->base_ptr);
        unpack_offset = ((char*) buffer->unpack_ptr) -
            ((char*) buffer->base_ptr);
        buffer->base_ptr = (char*)realloc(buffer->base_ptr, 
                                          num_pages * orte_dss_page_size);
    } else {
        pack_offset = 0;
        unpack_offset = 0;
        buffer->bytes_used = 0;
        buffer->base_ptr = (char*)malloc(num_pages * orte_dss_page_size);
    }
    
    if (NULL == buffer->base_ptr) { 
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return NULL; 
    }
    buffer->pack_ptr = ((char*) buffer->base_ptr) + pack_offset;
    buffer->unpack_ptr = ((char*) buffer->base_ptr) + unpack_offset;
    buffer->bytes_allocated = num_pages * orte_dss_page_size;
    buffer->bytes_avail = buffer->bytes_allocated - buffer->bytes_used;
    
    /* All done */

    return buffer->pack_ptr;
}

/*
 * Internal function that checks to see if the specified number of bytes
 * remain in the buffer for unpacking
 */
bool orte_dss_too_small(orte_buffer_t *buffer, size_t bytes_reqd)
{
    size_t bytes_remaining_packed;
    
    if (buffer->pack_ptr < buffer->unpack_ptr) {
        ORTE_ERROR_LOG(ORTE_ERR_UNPACK_FAILURE);
        return true;
    }

    bytes_remaining_packed = buffer->pack_ptr - buffer->unpack_ptr;
    
    if (bytes_remaining_packed < bytes_reqd) {
        /* don't error log this - it could be that someone is trying to
         * simply read until the buffer is empty
         */
        return true;
    }

    return false;
}

int orte_dss_store_data_type(orte_buffer_t *buffer, orte_data_type_t type)
{
    int rc;
    orte_dss_type_info_t *info;

    /* Lookup the pack function for the actual orte_data_type type and call it */
    
    if (NULL == (info = (orte_dss_type_info_t*)orte_pointer_array_get_item(orte_dss_types, ORTE_DATA_TYPE_T))) {
        ORTE_ERROR_LOG(ORTE_ERR_PACK_FAILURE);
        return ORTE_ERR_PACK_FAILURE;
    }
    
    if (ORTE_SUCCESS != (rc = info->odti_pack_fn(buffer, &type, 1, ORTE_DATA_TYPE_T))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

int orte_dss_get_data_type(orte_buffer_t *buffer, orte_data_type_t *type)
{
    int rc;
    orte_dss_type_info_t *info;
    orte_std_cntr_t n=1;
    
    /* Lookup the unpack function for the actual orte_data_type type and call it */
    
    if (NULL == (info = (orte_dss_type_info_t*)orte_pointer_array_get_item(orte_dss_types, ORTE_DATA_TYPE_T))) {
        ORTE_ERROR_LOG(ORTE_ERR_PACK_FAILURE);
        return ORTE_ERR_PACK_FAILURE;
    }
    
    rc = info->odti_unpack_fn(buffer, type, &n, ORTE_DATA_TYPE_T);
    
    return rc;
}
