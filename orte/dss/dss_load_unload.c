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

#include "orte/mca/errmgr/errmgr.h"

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
    buffer->bytes_allocated = buffer->bytes_used = 0;

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

    /* All done */

    return ORTE_SUCCESS;    
}

int orte_dss_xfer_payload(orte_buffer_t *dest, orte_buffer_t *src)
{
    void *payload;
    orte_std_cntr_t bytes_used;
    int rc;
    
    /* ensure we have valid source and destination */
    if (NULL == dest || NULL == src) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    
    /* unload the src payload */
    if (ORTE_SUCCESS != (rc = orte_dss_unload(src, &payload, &bytes_used))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* load it into the dest, overwriting anything already there */
    if (ORTE_SUCCESS != (rc = orte_dss_load(dest, payload, bytes_used))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ORTE_SUCCESS;
}

int orte_dss_copy_payload(orte_buffer_t *dest, orte_buffer_t *src)
{
    char *dst_ptr;
    
    /* ensure we have valid source and destination */
    if (NULL == dest || NULL == src) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    
    /* if the dest is already populated, check to ensure that both
     * source and dest are of the same buffer type
     */
    if (0 != dest->bytes_used) {
        if (dest->type != src->type) {
            ORTE_ERROR_LOG(ORTE_ERR_BUFFER);
            return ORTE_ERR_BUFFER;
        }
    }
    
    /* either the dest was empty or the two types already match -
     * either way, just ensure the two types DO match
     */
    dest->type = src->type;
    
    /* add room to the dest for the src buffer's payload */
    if (NULL == (dst_ptr = orte_dss_buffer_extend(dest, src->bytes_used))) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    /* copy the src payload to the specified location in dest */
    memcpy(dst_ptr, src->base_ptr, src->bytes_used);
    
    /* adjust the dest buffer's bookkeeping */
    dest->bytes_used += src->bytes_used;
    dest->pack_ptr = ((char*)dest->pack_ptr) + src->bytes_used;
    
    return ORTE_SUCCESS;
}

