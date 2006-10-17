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
#include "orte/orte_constants.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/dss/dss_internal.h"

#include "orte/mca/odls/base/odls_private.h"

/*
 * ORTE_DAEMON_CMD
 */
int orte_odls_pack_daemon_cmd(orte_buffer_t *buffer, void *src, orte_std_cntr_t num_vals,
                              orte_data_type_t type)
{
    int ret;
    
    /* Turn around and pack the real type */
    if (ORTE_SUCCESS != (ret = orte_dss_pack_buffer(buffer, src, num_vals, ORTE_DAEMON_CMD_T))) {
        ORTE_ERROR_LOG(ret);
    }
    
    return ret;
}

