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
/** @file:
 *
 * The Open MPI general purpose registry - implementation.
 *
 */

/*
 * includes
 */

#include "orte_config.h"

#include "include/orte_constants.h"
#include "include/orte_types.h"
#include "dps/dps.h"

#include "mca/errmgr/errmgr.h"

#include "mca/gpr/base/base.h"

int orte_gpr_base_pack_put(orte_buffer_t *cmd,
                int cnt, orte_gpr_value_t **values)
{
    orte_gpr_cmd_flag_t command;
    int rc;

    command = ORTE_GPR_PUT_CMD;

    if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, &command, 1, ORTE_GPR_CMD))) {
	   return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, values, (size_t)cnt, ORTE_GPR_VALUE))) {
	   return rc;
    }

    return ORTE_SUCCESS;
}


int orte_gpr_base_pack_get(orte_buffer_t *cmd,
			  orte_gpr_addr_mode_t mode,
			  char *segment, char **tokens, char **keys)
{
    orte_gpr_cmd_flag_t command;
    char **ptr;
    int rc;
    int n;

    command = ORTE_GPR_GET_CMD;

    if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, &command, 1, ORTE_GPR_CMD))) {
       return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, &mode, 1, ORTE_GPR_ADDR_MODE))) {
    return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, &segment, 1, ORTE_STRING))) {
    return rc;
    }

    /* compute number of tokens */
    n = 0;
    if (NULL != tokens) {
        ptr = tokens;
        while (NULL != ptr[n]) {
           n++;
        }
    }

    /* pack number of tokens */
    if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, &n, 1, ORTE_INT))) {
      return rc;
    }
    
    if (n > 0) {
         if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, tokens, (size_t)n, ORTE_STRING))) {
           return rc;
         }
    }

    /* compute number of keys */
    n = 0;
    if (NULL != keys) {
        ptr = keys;
        while (NULL != ptr[n]) {
           n++;
        }
    }

    /* pack number of keys */
    if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, &n, 1, ORTE_INT))) {
      return rc;
    }
    
    if (n > 0) {
        if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, keys, (size_t)n, ORTE_STRING))) {
           return rc;
        }
    }

    return ORTE_SUCCESS;
}
