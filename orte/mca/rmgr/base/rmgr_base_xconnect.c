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

#include "orte/orte_constants.h"
#include "orte/orte_types.h"

#include "opal/util/output.h"
#include "opal/util/trace.h"

#include "orte/dss/dss.h"
#include "orte/mca/gpr/gpr_types.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/grpcomm/grpcomm.h"

#include "orte/mca/rmgr/base/rmgr_private.h"

int orte_rmgr_base_xconnect(orte_jobid_t child, orte_jobid_t parent)
{
    orte_rml_cmd_flag_t command;
    orte_gpr_notify_data_t *data=NULL;
    orte_process_name_t name;
    orte_buffer_t *buf;
    int rc;
    
    /* init the name fields */
    name.cellid = ORTE_PROC_MY_NAME->cellid;
    name.vpid = ORTE_VPID_WILDCARD;  /* we want data from everyone in the job */
    
    /* get the child's contact info */
    name.jobid = child;
    if (ORTE_SUCCESS != (rc = orte_rml_base_get_contact_info(&name, &data))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* send that info to everyone in the parent */
    if (NULL != data) {
        buf = OBJ_NEW(orte_buffer_t);
        /* pack the update-RML command */
        command = ORTE_RML_UPDATE_CMD;
        if (ORTE_SUCCESS != (rc = orte_dss.pack(buf, &command, 1, ORTE_RML_CMD))) {
            ORTE_ERROR_LOG(rc);
        }
        /* pack the data for xmission */
        if (ORTE_SUCCESS != (rc = orte_dss.pack(buf, &data, 1, ORTE_GPR_NOTIFY_DATA))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(buf);
            OBJ_RELEASE(data);
            return rc;
        }
        /* now send it */
        if (ORTE_SUCCESS != (rc = orte_grpcomm.xcast(parent, buf, ORTE_RML_TAG_RML_INFO_UPDATE))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(buf);
            OBJ_RELEASE(data);
            return rc;
        }
        /* done with the buffer */
        OBJ_RELEASE(buf);
        /* cleanup the data */
        OBJ_RELEASE(data);
    }
    
    /* get the parent's contact info */
    name.jobid = parent;
    if (ORTE_SUCCESS != (rc = orte_rml_base_get_contact_info(&name, &data))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* send that info to everyone in the child */
    if (NULL != data) {
        buf = OBJ_NEW(orte_buffer_t);
        /* pack the update-RML command */
        command = ORTE_RML_UPDATE_CMD;
        if (ORTE_SUCCESS != (rc = orte_dss.pack(buf, &command, 1, ORTE_RML_CMD))) {
            ORTE_ERROR_LOG(rc);
        }
        /* pack the data for xmission */
        if (ORTE_SUCCESS != (rc = orte_dss.pack(buf, &data, 1, ORTE_GPR_NOTIFY_DATA))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(buf);
            OBJ_RELEASE(data);
            return rc;
        }
        /* now send it */
        if (ORTE_SUCCESS != (rc = orte_grpcomm.xcast(child, buf, ORTE_RML_TAG_RML_INFO_UPDATE))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(buf);
            OBJ_RELEASE(data);
            return rc;
        }
        /* done with the buffer */
        OBJ_RELEASE(buf);
        /* cleanup the data */
        OBJ_RELEASE(data);
    }
    
    return ORTE_SUCCESS;
}
