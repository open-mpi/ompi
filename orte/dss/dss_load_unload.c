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
 
/*
 * DPS Buffer Operations
 */
 
/** @file:
 *
 */

#include "orte_config.h"

#include <sys/types.h>
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "orte/mca/ns/base/base.h"

#include "orte/dss/dss_internal.h"


int orte_dss_unload(orte_buffer_t *buffer, void **payload,
                    orte_std_cntr_t *bytes_used)
{
    char *hdr_dst = NULL;
    orte_dss_buffer_type_t type;

    /* check that buffer is not null */
    if (!buffer) {
        return ORTE_ERR_BAD_PARAM;
    }

    /* were we given someplace to point to the payload */
    if (NULL == payload) {
        return ORTE_ERR_BAD_PARAM;
    }
    
    /* anything in the buffer - if not, nothing to do */
    if (NULL == buffer->base_ptr || 0 == buffer->bytes_used) {
        *payload = NULL;
        *bytes_used = 0;
        return ORTE_SUCCESS;
    }

    /* add room for our description of the buffer -- currently just the type */
    if (NULL == (hdr_dst = orte_dss_buffer_extend(buffer, 
                                                  sizeof(orte_dss_buffer_type_t)))) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* add the header (at the end, so perhaps it's a footer? */
    type = buffer->type;
    ORTE_DSS_BUFFER_TYPE_HTON(type);
    memcpy(hdr_dst, &type, sizeof(orte_dss_buffer_type_t));
    buffer->bytes_used += sizeof(orte_dss_buffer_type_t);
    
    /* okay, we have something to provide - pass it back */
    *payload = buffer->base_ptr;
    *bytes_used = (orte_std_cntr_t)buffer->bytes_used;

    /* dereference everything in buffer */
    buffer->base_ptr = NULL;
    buffer->pack_ptr = buffer->unpack_ptr = NULL;
    buffer->bytes_allocated = buffer->bytes_used = buffer->bytes_avail = 0;

    /* All done */

    return ORTE_SUCCESS;
}


int orte_dss_load(orte_buffer_t *buffer, void *payload,
                  orte_std_cntr_t bytes_used)
{
    char *hdr_ptr;
    orte_dss_buffer_type_t type;

    /* check to see if the buffer has been initialized */
    if (NULL == buffer) {
        return ORTE_ERR_BAD_PARAM;
    }
    
    /* check that the payload is there */
    if (NULL == payload) {
        return ORTE_SUCCESS;
    }
    
    /* check if buffer already has payload - free it if so */
    if (NULL != buffer->base_ptr) {
        free(buffer->base_ptr);
    }

    /* get our header */
    hdr_ptr = (char*) payload + bytes_used - sizeof(orte_dss_buffer_type_t);
    memcpy(&type, hdr_ptr, sizeof(orte_dss_buffer_type_t));
    ORTE_DSS_BUFFER_TYPE_NTOH(type);
    buffer->type = type;
    bytes_used -= sizeof(orte_dss_buffer_type_t);
    
    /* populate the buffer */
    buffer->base_ptr = (char*)payload; 

    /* set pack/unpack pointers */
    buffer->pack_ptr = ((char*)buffer->base_ptr) + bytes_used; 
    buffer->unpack_ptr = buffer->base_ptr;

    /* set counts for size and space */
    buffer->bytes_allocated = buffer->bytes_used = bytes_used;
    buffer->bytes_avail = 0;

    /* All done */

    return ORTE_SUCCESS;    
}

