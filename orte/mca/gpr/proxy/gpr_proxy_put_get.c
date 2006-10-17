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
 *
 * The Open MPI general purpose registry - implementation.
 *
 */

/*
 * includes
 */

#include "orte_config.h"

#include "orte/orte_constants.h"
#include "orte/dss/dss_types.h"
#include "opal/util/output.h"
#include "opal/util/trace.h"
#include "orte/util/proc_info.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/oob/oob_types.h"
#include "orte/mca/rml/rml.h"

#include "gpr_proxy.h"

int orte_gpr_proxy_put(orte_std_cntr_t cnt, orte_gpr_value_t **values)
{
    orte_buffer_t *cmd;
    orte_buffer_t *answer;
    int rc, ret;

    OPAL_TRACE(1);

    if (orte_gpr_proxy_globals.compound_cmd_mode) {
        if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_put(orte_gpr_proxy_globals.compound_cmd, cnt, values))) {
            ORTE_ERROR_LOG(rc);
        }
        return rc;
    }

    cmd = OBJ_NEW(orte_buffer_t);
    if (NULL == cmd) { /* got a problem */
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_put(cmd, cnt, values))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }

    if (0 > orte_rml.send_buffer(orte_process_info.gpr_replica, cmd, ORTE_RML_TAG_GPR, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        return ORTE_ERR_COMM_FAILURE;
    }
    OBJ_RELEASE(cmd);

    answer = OBJ_NEW(orte_buffer_t);
    if (NULL == answer) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (0 > orte_rml.recv_buffer(orte_process_info.gpr_replica, answer, ORTE_RML_TAG_GPR)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        return ORTE_ERR_COMM_FAILURE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_base_unpack_put(answer, &ret))) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_RELEASE(answer);

    return ret;
}

int orte_gpr_proxy_put_nb(orte_std_cntr_t cnt, orte_gpr_value_t **values,
                          orte_gpr_notify_cb_fn_t cbfunc, void *user_tag)
{
    OPAL_TRACE(1);

    return ORTE_ERR_NOT_IMPLEMENTED;
}

int orte_gpr_proxy_get(orte_gpr_addr_mode_t mode,
                       char *segment, char **tokens, char **keys,
                       orte_std_cntr_t *cnt, orte_gpr_value_t ***values)

{
    orte_buffer_t *cmd;
    orte_buffer_t *answer;
    int rc, ret;

    OPAL_TRACE(1);

    *values = NULL;
    *cnt = 0;

    /* need to protect against errors */
    if (NULL == segment) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    if (orte_gpr_proxy_globals.compound_cmd_mode) {
        if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_get(orte_gpr_proxy_globals.compound_cmd,
                                        mode, segment, tokens, keys))) {
            ORTE_ERROR_LOG(rc);
        }
        return rc;
    }

    cmd = OBJ_NEW(orte_buffer_t);
    if (NULL == cmd) { /* got a problem */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_get(cmd, mode, segment, tokens, keys))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (0 > orte_rml.send_buffer(orte_process_info.gpr_replica, cmd, ORTE_RML_TAG_GPR, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        return ORTE_ERR_COMM_FAILURE;
    }

    answer = OBJ_NEW(orte_buffer_t);
    if (NULL == answer) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (0 > orte_rml.recv_buffer(orte_process_info.gpr_replica, answer, ORTE_RML_TAG_GPR)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        return ORTE_ERR_COMM_FAILURE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_base_unpack_get(answer, &ret, cnt, values))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return rc;
    }
    OBJ_RELEASE(answer);

    return ret;
}

int orte_gpr_proxy_get_conditional(orte_gpr_addr_mode_t mode,
                       char *segment, char **tokens, char **keys,
                       orte_std_cntr_t num_conditions, orte_gpr_keyval_t **conditions,
                       orte_std_cntr_t *cnt, orte_gpr_value_t ***values)

{
    orte_buffer_t *cmd;
    orte_buffer_t *answer;
    int rc, ret;

    OPAL_TRACE(1);

    *values = NULL;
    *cnt = 0;

    /* need to protect against errors */
    if (NULL == segment) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    if (orte_gpr_proxy_globals.compound_cmd_mode) {
        if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_get_conditional(orte_gpr_proxy_globals.compound_cmd,
                                        mode, segment, tokens, keys, num_conditions, conditions))) {
            ORTE_ERROR_LOG(rc);
        }
        return rc;
    }

    cmd = OBJ_NEW(orte_buffer_t);
    if (NULL == cmd) { /* got a problem */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_get_conditional(cmd, mode, segment, tokens, keys, num_conditions, conditions))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (0 > orte_rml.send_buffer(orte_process_info.gpr_replica, cmd, ORTE_RML_TAG_GPR, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        return ORTE_ERR_COMM_FAILURE;
    }

    answer = OBJ_NEW(orte_buffer_t);
    if (NULL == answer) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (0 > orte_rml.recv_buffer(orte_process_info.gpr_replica, answer, ORTE_RML_TAG_GPR)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        return ORTE_ERR_COMM_FAILURE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_base_unpack_get(answer, &ret, cnt, values))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return rc;
    }
    OBJ_RELEASE(answer);

    return ret;
}

int orte_gpr_proxy_get_nb(orte_gpr_addr_mode_t addr_mode,
                          char *segment, char **tokens, char **keys,
                          orte_gpr_notify_cb_fn_t cbfunc, void *user_tag)
{
    OPAL_TRACE(1);

    return ORTE_ERR_NOT_IMPLEMENTED;
}
