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

/*
 * ORTE_DAEMON_CMD
 */
int orte_odls_print_daemon_cmd(char **output, char *prefix, orte_daemon_cmd_flag_t *src, orte_data_type_t type)
{
    char *prefx;
    
    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;
    
    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: ORTE_DAEMON_CMD\tValue: NULL pointer", prefx);
        return ORTE_SUCCESS;
    }
    
    asprintf(output, "%sData type: ORTE_DAEMON_CMD\tValue: %lu", prefx, (unsigned long) *src);
    
    return ORTE_SUCCESS;
}

