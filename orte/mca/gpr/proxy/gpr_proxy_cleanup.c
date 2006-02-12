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
#include "orte/orte_types.h"
#include "opal/util/trace.h"
#include "orte/dss/dss_types.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/oob/oob_types.h"
#include "orte/mca/rml/rml.h"

#include "gpr_proxy.h"


int orte_gpr_proxy_cleanup_job(orte_jobid_t jobid)
{
    orte_buffer_t *cmd, *answer;
    int rc, ret;

    OPAL_TRACE(1);

    if (orte_gpr_proxy_globals.compound_cmd_mode) {
        if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_cleanup_job(orte_gpr_proxy_globals.compound_cmd, jobid))) {
            ORTE_ERROR_LOG(rc);
        }
        return rc;
    }

    cmd = OBJ_NEW(orte_buffer_t);
    if (NULL == cmd) { /* got a problem */
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_cleanup_job(cmd, jobid))) {
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

    if (ORTE_SUCCESS != (rc = orte_gpr_base_unpack_cleanup_job(answer, &ret))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return rc;
    }

    OBJ_RELEASE(answer);

    return ret;

}


int orte_gpr_proxy_cleanup_proc(orte_process_name_t *proc)
{
    orte_buffer_t *cmd, *answer;
    int rc, ret;

    OPAL_TRACE(1);

    if (orte_gpr_proxy_globals.compound_cmd_mode) {
        if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_cleanup_proc(orte_gpr_proxy_globals.compound_cmd, proc))) {
            ORTE_ERROR_LOG(rc);
        }
        return rc;
    }

    cmd = OBJ_NEW(orte_buffer_t);
    if (NULL == cmd) { /* got a problem */
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
     return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_cleanup_proc(cmd, proc))) {
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

    if (ORTE_SUCCESS != (rc = orte_gpr_base_unpack_cleanup_proc(answer, &ret))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return rc;
    }

    OBJ_RELEASE(answer);

    return ret;
}
