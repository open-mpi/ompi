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


#include "dps_internal.h"

int orte_dps_peek(orte_buffer_t *buffer,
                  orte_data_type_t *type,
                  size_t *number)
{
    int rc=ORTE_SUCCESS;
    size_t num_vals;
    size_t mem_left;
    size_t num_bytes, hdr_bytes;
    void *src;
    uint32_t * s32;
    orte_data_type_t stored_type;

    /* check for errors */
    if (buffer == NULL) {
        return (ORTE_ERR_BAD_PARAM);
    }

    num_bytes = 0; /* have not unpacked any yet */
    hdr_bytes = 0;

    src = buffer->from_ptr;  /* get location in buffer */
    mem_left = buffer->toend;  /* how much data is left in buffer */

    /* check to see if there is enough in the buffer to hold the pack type */
    if (mem_left < sizeof(orte_data_type_t)) {
        return ORTE_ERR_UNPACK_FAILURE;
    }

    /* first thing in the current buffer space must be the type */
    if (ORTE_SUCCESS != (rc =orte_dps_unpack_nobuffer(&stored_type, src, 1,
                                    ORTE_DATA_TYPE, &mem_left, &hdr_bytes))) {
        return rc;
    }
    src = (void*)((char*)src + hdr_bytes);

    /* got enough left for num_vals? */
    if (sizeof(uint32_t) > mem_left) { /* not enough memory  */
        return ORTE_ERR_UNPACK_FAILURE;
    }

    /* unpack the number of values */
    s32 = (uint32_t *) src;
    num_vals = (size_t)ntohl(*s32);
 
    if (type != NULL)
        *type = stored_type;
    if (number != NULL)
        *number = num_vals;
    return ORTE_SUCCESS;
}
