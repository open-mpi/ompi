/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
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
#include <netinet/in.h>

#include "mca/ns/base/base.h"

#include "dps_internal.h"


int orte_dps_unload(orte_buffer_t *buffer,
                    void **payload,
                    size_t *size)
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
    if (NULL == buffer->base_ptr || 0 == buffer->len) {
        *payload = NULL;
        *size = 0;
        return ORTE_SUCCESS;
    }
    
    /* okay, we have something to provide - pass it back
     */
    *payload = buffer->base_ptr;
    *size = buffer->len;

    /* dereference everything in buffer */
    buffer->base_ptr = NULL;
    buffer->size = 0;
    buffer->len  = 0;
    buffer->space = 0;
    buffer->toend = 0;
    
    return (OMPI_SUCCESS);

}


int orte_dps_load(orte_buffer_t *buffer,
                  void *payload,
                  size_t size)
{
    /* check to see if the buffer has been initialized */
    if (NULL == buffer) {
        return ORTE_ERR_BAD_PARAM;
    }
    
    /* check that the payload is there */
    if (NULL == payload || 0 >= size) {
        return ORTE_SUCCESS;
    }
    
    /* check if buffer already has payload - free it if so */
    if (NULL != buffer->base_ptr) {
        free(buffer->base_ptr);
    }
    
    /* populate the buffer */
    buffer->base_ptr = payload; /* set the start of the buffer */

    /* set data pointer to END of the buffer */
    buffer->data_ptr = ((char*)buffer->base_ptr) + size; 

    buffer->from_ptr = buffer->base_ptr; /* set the unpack start at start */

  /* set counts for size and space */
    buffer->size = size;
    buffer->len  = size;     /* users buffer is expected to be full */
    buffer->space = 0;                /* ditto */
    buffer->toend = size;    /* ditto */

    /* dereference the payload pointer to protect it */
    payload = NULL;
    
    return ORTE_SUCCESS;    
}

