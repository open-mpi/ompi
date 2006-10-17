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
#include "orte/orte_constants.h"

#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/odls/base/odls_private.h"

int orte_odls_copy_daemon_cmd(orte_daemon_cmd_flag_t **dest, orte_daemon_cmd_flag_t *src, orte_data_type_t type)
{
    size_t datasize;
    
    datasize = sizeof(orte_daemon_cmd_flag_t);
   
    *dest = (orte_daemon_cmd_flag_t*)malloc(datasize);
    if (NULL == *dest) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    memcpy(*dest, src, datasize);
    
    return ORTE_SUCCESS;
}

