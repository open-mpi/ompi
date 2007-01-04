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
/** @file:
 */

/*
 * includes
 */
#include "orte_config.h"

#include <string.h>

#include "orte/mca/schema/schema.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/ns/ns.h"

#include "orte/mca/smr/base/smr_private.h"

int orte_smr_base_get_node_state(orte_node_state_t *state,
                                 orte_cellid_t cell,
                                 char *nodename)
{
    orte_gpr_value_t **values=NULL;
    int rc;
    orte_std_cntr_t cnt, num_tokens, i;
    char **tokens;
    char *keys[] = {
        ORTE_NODE_STATE_KEY,
        NULL
    };
    orte_node_state_t *sptr;

    if (ORTE_SUCCESS != (rc = orte_schema.get_node_tokens(&tokens, &num_tokens, cell, nodename))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr.get(ORTE_GPR_TOKENS_XAND, ORTE_NODE_SEGMENT,
                                tokens, keys, &cnt, &values))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }

    /** there should be one - and only one - value returned. if cnt is anything else,
     * we have a problem
     */
    if (1 != cnt) {
        if (0 == cnt) {  /** check for special case - didn't find the node's container */
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            rc = ORTE_ERR_NOT_FOUND;
            goto CLEANUP;
        }
        /** if not 0, then we have too many - report that */
        ORTE_ERROR_LOG(ORTE_ERR_INDETERMINATE_STATE_INFO);
        rc = ORTE_ERR_INDETERMINATE_STATE_INFO;
        goto CLEANUP;
    }

    /* there should only be one keyval returned - if not, got a problem */
    if (1 != values[0]->cnt) {
        ORTE_ERROR_LOG(ORTE_ERR_INDETERMINATE_STATE_INFO);
        rc = ORTE_ERR_INDETERMINATE_STATE_INFO;
        goto CLEANUP;
    }

    if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&sptr, values[0]->keyvals[0]->value, ORTE_NODE_STATE))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    *state = *sptr;

CLEANUP:
    for (i=0; i < num_tokens; i++) {
        if (NULL != tokens[i]) free(tokens[i]);
    }
    free(tokens);

    if (NULL != values) {
        for (i=0; i < cnt; i++) {
            OBJ_RELEASE(values[i]);
        }
        if (NULL != values) free(values);
    }

    return rc;
}
