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
 */

#include "orte_config.h"

#include "orte/orte_constants.h"
#include "orte/orte_types.h"
#include "orte/dss/dss.h"
#include "opal/util/output.h"
#include "opal/util/trace.h"
#include "orte/util/proc_info.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/oob/oob_types.h"
#include "orte/mca/rml/rml.h"

#include "gpr_proxy.h"

/**
 * globals
 */

/*
 * Implemented registry functions
 */


int orte_gpr_proxy_delete_segment(char *segment)
{
    orte_buffer_t *cmd;
    orte_buffer_t *answer;
    int rc, ret;

    OPAL_TRACE(1);

    if (orte_gpr_proxy_globals.compound_cmd_mode) {
       return orte_gpr_base_pack_delete_segment(orte_gpr_proxy_globals.compound_cmd, segment);
    }

    cmd = OBJ_NEW(orte_buffer_t);
    if (NULL == cmd) { /* got a problem */
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
       return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_delete_segment(cmd, segment))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }

    if (0 > orte_rml.send_buffer(orte_process_info.gpr_replica, cmd, ORTE_RML_TAG_GPR, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(cmd);
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
       OBJ_RELEASE(answer);
       return ORTE_ERR_COMM_FAILURE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_base_unpack_delete_segment(answer, &ret))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return rc;
    }

    OBJ_RELEASE(answer);

    return ret;
}


int orte_gpr_proxy_delete_segment_nb(char *segment,
                                orte_gpr_notify_cb_fn_t cbfunc, void *user_tag)
{
    OPAL_TRACE(1);

    return ORTE_ERR_NOT_IMPLEMENTED;
}


int orte_gpr_proxy_delete_entries(orte_gpr_addr_mode_t mode,
                char *segment, char **tokens, char **keys)
{
    orte_buffer_t *cmd;
    orte_buffer_t *answer;
    int rc, ret;

    OPAL_TRACE(1);

    /* need to protect against errors */
    if (NULL == segment) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    if (orte_gpr_proxy_globals.compound_cmd_mode) {
        if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_delete_entries(orte_gpr_proxy_globals.compound_cmd,
                           mode, segment, tokens, keys))) {
            ORTE_ERROR_LOG(rc);
        }
        return rc;
    }

    cmd = OBJ_NEW(orte_buffer_t);
    if (NULL == cmd) { /* got a problem */
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
       return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_delete_entries(cmd,
                            mode, segment, tokens, keys))) {
        ORTE_ERROR_LOG(rc);
       OBJ_RELEASE(cmd);
       return rc;
    }

    if (0 > orte_rml.send_buffer(orte_process_info.gpr_replica, cmd, ORTE_RML_TAG_GPR, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(cmd);
       return ORTE_ERR_COMM_FAILURE;
    }
    OBJ_RELEASE(cmd);

    answer = OBJ_NEW(orte_buffer_t);
    if (NULL == answer) { /* got a problem */
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
       return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (0 > orte_rml.recv_buffer(orte_process_info.gpr_replica, answer, ORTE_RML_TAG_GPR)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
       return ORTE_ERR_COMM_FAILURE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_base_unpack_delete_entries(answer, &ret))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return rc;
    }

    OBJ_RELEASE(answer);

    return ret;
}


int orte_gpr_proxy_delete_entries_nb(
                            orte_gpr_addr_mode_t addr_mode,
                            char *segment, char **tokens, char **keys,
                            orte_gpr_notify_cb_fn_t cbfunc, void *user_tag)
{
    OPAL_TRACE(1);

    return ORTE_ERR_NOT_IMPLEMENTED;
}


int orte_gpr_proxy_index(char *segment, orte_std_cntr_t *cnt, char ***index)
{
    orte_buffer_t *cmd;
    orte_buffer_t *answer;
    int rc, ret;

    OPAL_TRACE(1);

    if (NULL == index || NULL == cnt) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    *cnt = 0;
    *index = NULL;

    if (orte_gpr_proxy_globals.compound_cmd_mode) {
        if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_index(orte_gpr_proxy_globals.compound_cmd, segment))) {
            ORTE_ERROR_LOG(rc);
        }
        return rc;
    }

    cmd = OBJ_NEW(orte_buffer_t);
    if (NULL == cmd) { /* got a problem */
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_index(cmd, segment))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }

    if (0 > orte_rml.send_buffer(orte_process_info.gpr_replica, cmd, ORTE_RML_TAG_GPR, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(cmd);
        return ORTE_ERR_COMM_FAILURE;
    }
    OBJ_RELEASE(cmd);

    answer = OBJ_NEW(orte_buffer_t);
    if (NULL == answer) { /* got a problem */
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (0 > orte_rml.recv_buffer(orte_process_info.gpr_replica, answer, ORTE_RML_TAG_GPR)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_base_unpack_index(answer, &ret, cnt, index))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return rc;
    }

    OBJ_RELEASE(answer);

    return ret;
}

int orte_gpr_proxy_index_nb(char *segment,
                        orte_gpr_notify_cb_fn_t cbfunc, void *user_tag)
{
    OPAL_TRACE(1);

    return ORTE_ERR_NOT_IMPLEMENTED;
}
