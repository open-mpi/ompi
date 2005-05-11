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

#include "mca/ns/base/base.h"

#include "dps/dps_internal.h"


int orte_dps_unload(orte_buffer_t *buffer, void **payload,
                    size_t *bytes_used)
{
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
    
    /* okay, we have something to provide - pass it back */
    *payload = buffer->base_ptr;
    *bytes_used = buffer->bytes_used;

    /* dereference everything in buffer */
    buffer->base_ptr = NULL;
    buffer->pack_ptr = buffer->unpack_ptr = NULL;
    buffer->bytes_allocated = buffer->bytes_used = buffer->bytes_avail = 0;

    /* All done */

    return OMPI_SUCCESS;
}


int orte_dps_load(orte_buffer_t *buffer, void *payload,
                  size_t bytes_used)
{
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
    
    /* populate the buffer */
    buffer->base_ptr = payload; 

    /* set pack/unpack pointers */
    buffer->pack_ptr = ((char*)buffer->base_ptr) + bytes_used; 
    buffer->unpack_ptr = buffer->base_ptr;

    /* set counts for size and space */
    buffer->bytes_allocated = buffer->bytes_used = bytes_used;
    buffer->bytes_avail = 0;

    /* All done */

    return ORTE_SUCCESS;    
}

