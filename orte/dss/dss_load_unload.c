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
    *bytes_used = buffer->bytes_used;
    
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


/* Move the UNPACKED portion of a source buffer into a destination buffer
 * The complete contents of the src buffer are NOT moved - only that
 * portion that has not been previously unpacked. However, we must ensure
 * that we don't subsequently "free" memory from inside a previously
 * malloc'd block. Hence, we must obtain a new memory allocation for the
 * dest buffer's storage before we move the data across. As a result, this
 * looks functionally a lot more like a destructive "copy" - both for
 * the source and destination buffers - then a direct transfer of data!
 */
int orte_dss_xfer_payload(orte_buffer_t *dest, orte_buffer_t *src)
{
    int rc;
    
    /* ensure we have valid source and destination */
    if (NULL == dest || NULL == src) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    
    /* if the dest is already populated, release the data */
    if (0 != dest->bytes_used) {
        free(dest->base_ptr);
        dest->base_ptr = NULL;
        dest->pack_ptr = dest->unpack_ptr = NULL;
        dest->bytes_allocated = dest->bytes_used = 0;
    }
    
    /* ensure the dest buffer type matches the src */
    dest->type = src->type;
    
    /* copy the src payload to the dest - this will allocate "fresh"
     * memory for the unpacked payload remaining in the src buffer
     */
    if (ORTE_SUCCESS != (rc = orte_dss_copy_payload(dest, src))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* dereference everything in src */
    free(src->base_ptr);
    src->base_ptr = NULL;
    src->pack_ptr = src->unpack_ptr = NULL;
    src->bytes_allocated = src->bytes_used = 0;
    
    return ORTE_SUCCESS;
}


/* Copy the UNPACKED portion of a source buffer into a destination buffer
 * The complete contents of the src buffer are NOT copied - only that
 * portion that has not been previously unpacked is copied.
 */
int orte_dss_copy_payload(orte_buffer_t *dest, orte_buffer_t *src)
{
    char *dst_ptr;
    orte_std_cntr_t bytes_left;

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
    
    /* compute how much of the src buffer remains unpacked
     * buffer->bytes_used is the total number of bytes in the buffer that
     * have been packed. However, we may have already unpacked some of
     * that data. We only want to unload what remains unpacked. This
     * means we have to look at how much of the buffer remains "used"
     * beyond the unpack_ptr
     */
    bytes_left = src->bytes_used - (src->unpack_ptr - src->base_ptr);
    
    /* if nothing is left, then nothing to do */
    if (0 == bytes_left) {
        return ORTE_SUCCESS;
    }
    
    /* add room to the dest for the src buffer's payload */
    if (NULL == (dst_ptr = orte_dss_buffer_extend(dest, bytes_left))) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    /* copy the src payload to the specified location in dest */
    memcpy(dst_ptr, src->unpack_ptr, bytes_left);
    
    /* adjust the dest buffer's bookkeeping */
    dest->bytes_used += bytes_left;
    dest->pack_ptr = ((char*)dest->pack_ptr) + bytes_left;
    
    return ORTE_SUCCESS;
}

