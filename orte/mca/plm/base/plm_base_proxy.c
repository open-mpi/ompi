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
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.
 *                         All rights reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/dss/dss.h"
#include "orte/util/name_fns.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/plm/base/plm_private.h"

int orte_plm_proxy_init(void)
{
    return ORTE_SUCCESS;
}

typedef struct {
    opal_object_t super;
    orte_jobid_t jobid;
    int32_t rc;
    bool active;
} orte_proxy_spawn_t;
static void proxy_const(orte_proxy_spawn_t *p)
{
    p->jobid = ORTE_JOBID_INVALID;
    p->rc = ORTE_ERROR;
    p->active = false;
}
OBJ_CLASS_INSTANCE(orte_proxy_spawn_t, opal_object_t, proxy_const, NULL);

static void proxy_spawn_response(int status, orte_process_name_t* sender,
                                 opal_buffer_t* buffer, orte_rml_tag_t tag,
                                 void* cbdata)
{
    int rc;
    orte_std_cntr_t count;
    orte_proxy_spawn_t *ps = (orte_proxy_spawn_t*)cbdata;

    OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                         "%s plm:base:proxy recvd spawn response",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* get the returned status code for the launch request */
    count = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &ps->rc, &count, OPAL_INT32))) {
        ORTE_ERROR_LOG(rc);
        ps->rc = rc;
        goto done;
    }

    /* get the new jobid back in case the caller wants it */
    count = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &ps->jobid, &count, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        ps->rc = rc;
    }

 done:
    /* release the waiting call */
    ps->active = false;
}

int orte_plm_proxy_spawn(orte_job_t *jdata)
{
    opal_buffer_t *buf;
    orte_plm_cmd_flag_t command;
    int rc;
    orte_proxy_spawn_t *ps;

    OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                         "%s plm:base:proxy spawn child job",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* setup the buffer */
    buf = OBJ_NEW(opal_buffer_t);
    
    /* tell the recipient we are sending a launch request */
    command = ORTE_PLM_LAUNCH_JOB_CMD;
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &command, 1, ORTE_PLM_CMD))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* pack the jdata object */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &jdata, 1, ORTE_JOB))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
        
    }
    
    /* create the proxy spawn object */
    ps = OBJ_NEW(orte_proxy_spawn_t);
    /* post the recv the HNP's response */
    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                            ORTE_RML_TAG_PLM_PROXY,
                            ORTE_RML_NON_PERSISTENT,
                            proxy_spawn_response,
                            ps);
    
    /* tell the HNP to launch the job */
    if (0 > (rc = orte_rml.send_buffer_nb(ORTE_PROC_MY_HNP, buf,
                                          ORTE_RML_TAG_PLM,
                                          orte_rml_send_callback, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        goto CLEANUP;
    }
    
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                         "%s plm:base:proxy waiting for response",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    ps->active = true;
    ORTE_WAIT_FOR_COMPLETION(ps->active);

    /* return the values */
    jdata->jobid = ps->jobid;
    rc = ps->rc;
    /* cleanup the memory */
    OBJ_RELEASE(ps);

CLEANUP:    
    return rc;
}

int orte_plm_proxy_finalize(void)
{
    return ORTE_SUCCESS;
}
