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
#include "ompi_config.h"
#include <errno.h>
#include <unistd.h>
#include <string.h>

#include "include/constants.h"
#include "dps/dps.h"
#include "mca/rmgr/base/base.h"
#include "mca/errmgr/errmgr.h"


/*
 * 
 */

static int orte_rmgr_base_cmd_query(orte_buffer_t* req, orte_buffer_t* rsp)
{
    int32_t rc = orte_rmgr.query();
    return orte_dps.pack(rsp, &rc, 1, ORTE_INT32);
}

static int orte_rmgr_base_cmd_create(orte_buffer_t* req, orte_buffer_t* rsp)
{
    int rc;
    int32_t ret;
    orte_app_context_t** context;
    orte_jobid_t jobid;
    size_t i, cnt, num_context;

    cnt = 1;
    if(ORTE_SUCCESS != (rc = orte_dps.unpack(req, &num_context, &cnt, ORTE_UINT32))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if(NULL == (context = malloc(sizeof(orte_app_context_t*)*num_context))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
   
    cnt = num_context;
    if(ORTE_SUCCESS != (rc = orte_dps.unpack(req, context, &cnt, ORTE_APP_CONTEXT))) {
        ORTE_ERROR_LOG(rc);
        free(context);
        return rc;
    }

    ret = orte_rmgr.create(context, num_context, &jobid);
    if(ORTE_SUCCESS != (rc = orte_dps.pack(rsp, &jobid, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    if(ORTE_SUCCESS != (rc = orte_dps.pack(rsp, &ret, 1, ORTE_INT32))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

cleanup:
    for(i=0; i<num_context; i++) {
        OBJ_RELEASE(context[i]);
    }
    free(context);
    return ret;
}


static int orte_rmgr_base_cmd_allocate(orte_buffer_t* req, orte_buffer_t* rsp)
{
    int32_t rc;
    orte_jobid_t jobid;
    size_t cnt = 1;

    if(ORTE_SUCCESS != (rc = orte_dps.unpack(req, &jobid, &cnt, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
    } else {
        rc = orte_rmgr.allocate(jobid);
    }
    return orte_dps.pack(rsp, &rc, 1, ORTE_INT32);
}

static int orte_rmgr_base_cmd_deallocate(orte_buffer_t* req, orte_buffer_t* rsp)
{
    int32_t rc;
    orte_jobid_t jobid;
    size_t cnt = 1;

    if(ORTE_SUCCESS != (rc = orte_dps.unpack(req, &jobid, &cnt, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
    } else {
        rc = orte_rmgr.deallocate(jobid);
    }
    return orte_dps.pack(rsp, &rc, 1, ORTE_INT32);
}

static int orte_rmgr_base_cmd_map(orte_buffer_t* req, orte_buffer_t* rsp)
{
    int rc;
    orte_jobid_t jobid;
    size_t cnt = 1;

    if(ORTE_SUCCESS != (rc = orte_dps.unpack(req, &jobid, &cnt, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
    } else {
        rc = orte_rmgr.map(jobid);
    }
    return orte_dps.pack(rsp, &rc, 1, ORTE_INT32);
}

static int orte_rmgr_base_cmd_launch(orte_buffer_t* req, orte_buffer_t* rsp)
{
    int rc;
    orte_jobid_t jobid;
    size_t cnt = 1;

    if(ORTE_SUCCESS != (rc = orte_dps.unpack(req, &jobid, &cnt, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
    } else {
        rc = orte_rmgr.launch(jobid);
    }
    return orte_dps.pack(rsp, &rc, 1, ORTE_INT32);
}


static int orte_rmgr_base_cmd_term_job(orte_buffer_t* req, orte_buffer_t* rsp)
{
    int rc;
    orte_jobid_t jobid;
    size_t cnt = 1;

    if(ORTE_SUCCESS != (rc = orte_dps.unpack(req, &jobid, &cnt, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
    } else {
        rc = orte_rmgr.terminate_job(jobid);
    }
    return orte_dps.pack(rsp, &rc, 1, ORTE_INT32);
}


static int orte_rmgr_base_cmd_term_proc(orte_buffer_t* req, orte_buffer_t* rsp)
{
    int rc;
    orte_process_name_t name;
    size_t cnt = 1;

    if(ORTE_SUCCESS != (rc = orte_dps.unpack(req, &name, &cnt, ORTE_NAME))) {
        ORTE_ERROR_LOG(rc);
    } else {
        rc = orte_rmgr.terminate_proc(&name);
    }
    return orte_dps.pack(rsp, &rc, 1, ORTE_INT32);
}



int orte_rmgr_base_cmd_dispatch(orte_buffer_t* req, orte_buffer_t* rsp)
{
    orte_rmgr_cmd_t cmd;
    size_t cnt = 1;
    int rc;

    rc = orte_dps.unpack(req, &cmd, &cnt, ORTE_RMGR_CMD);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    switch(cmd) {
        case ORTE_RMGR_CMD_QUERY:
            return orte_rmgr_base_cmd_query(req,rsp);
        case ORTE_RMGR_CMD_CREATE:
            return orte_rmgr_base_cmd_create(req,rsp);
        case ORTE_RMGR_CMD_ALLOCATE:
            return orte_rmgr_base_cmd_allocate(req,rsp);
        case ORTE_RMGR_CMD_DEALLOCATE:
            return orte_rmgr_base_cmd_deallocate(req,rsp);
        case ORTE_RMGR_CMD_MAP:
            return orte_rmgr_base_cmd_map(req,rsp);
        case ORTE_RMGR_CMD_LAUNCH:
            return orte_rmgr_base_cmd_launch(req,rsp);
        case ORTE_RMGR_CMD_TERM_JOB:
            return orte_rmgr_base_cmd_term_job(req,rsp);
        case ORTE_RMGR_CMD_TERM_PROC:
            return orte_rmgr_base_cmd_term_proc(req,rsp);
        default:
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
            return ORTE_ERR_BAD_PARAM;
    }
}


