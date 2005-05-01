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

#include "mca/errmgr/errmgr.h"

#include "dps/dps_internal.h"


int orte_dps_peek(orte_buffer_t *buffer, orte_data_type_t *type,
                  size_t *num_vals)
{
    int ret;
    orte_buffer_t tmp;

    /* check for errors */
    if (buffer == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    /* cheat: unpack from a copy of the buffer -- leaving all the
       original pointers intact */
    tmp = *buffer;

    if (ORTE_SUCCESS != (ret = orte_dps_unpack_sizet(&tmp, num_vals, 1, ORTE_SIZE))) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    if (ORTE_SUCCESS != (ret = orte_dps_get_data_type(&tmp, type))) {
        ORTE_ERROR_LOG(ret);
    }

    return ret;
}
