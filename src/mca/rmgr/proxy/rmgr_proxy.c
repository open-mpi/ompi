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
#include "util/output.h"
#include "mca/errmgr/errmgr.h"
#include "mca/rmgr/base/base.h"
#include "mca/rml/rml.h"
#include "mca/iof/iof.h"
#include "rmgr_proxy.h"


static int orte_rmgr_proxy_query(void);

static int orte_rmgr_proxy_create(
    orte_app_context_t** app_context,
    size_t num_context,
    orte_jobid_t* jobid);

static int orte_rmgr_proxy_allocate(
    orte_jobid_t jobid);

static int orte_rmgr_proxy_deallocate(
    orte_jobid_t jobid);

static int orte_rmgr_proxy_map(
    orte_jobid_t jobid);

static int orte_rmgr_proxy_launch(
    orte_jobid_t jobid);

static int orte_rmgr_proxy_terminate_job(
    orte_jobid_t jobid);

static int orte_rmgr_proxy_terminate_proc(
    const orte_process_name_t* proc_name);

static int orte_rmgr_proxy_spawn(
    orte_app_context_t** app_context,
    size_t num_context,
    orte_jobid_t* jobid,
    orte_rmgr_cb_fn_t cbfn);

orte_rmgr_base_module_t orte_rmgr_proxy_module = {
    orte_rmgr_proxy_query,
    orte_rmgr_proxy_create,
    orte_rmgr_proxy_allocate,
    orte_rmgr_proxy_deallocate,
    orte_rmgr_proxy_map,
    orte_rmgr_proxy_launch,
    orte_rmgr_proxy_terminate_job,
    orte_rmgr_proxy_terminate_proc,
    orte_rmgr_proxy_spawn,
    orte_rmgr_base_proc_stage_gate_init,
    orte_rmgr_base_proc_stage_gate_mgr,
    NULL, /* finalize */
};


/*
 *  Create the job segment and initialize the application context. Could
 *  do this in the proxy - but allowing the seed to do this moves responsibility
 *  for the stage gates to the seed. This allows the client to disconnect.
 */

static int orte_rmgr_proxy_create(
    orte_app_context_t** app_context,
    size_t num_context,
    orte_jobid_t* jobid)
{
    orte_buffer_t cmd;
    orte_buffer_t rsp;
    int rc;

    /* construct command */
    OBJ_CONSTRUCT(&cmd, orte_buffer_t);
    rc = orte_rmgr_base_pack_create_cmd(&cmd, app_context, num_context);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&cmd);
        return rc;
    }

    if(0 > (rc = orte_rml.send_buffer(ORTE_RML_NAME_SEED, &cmd, ORTE_RML_TAG_RMGR_SVC, 0))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&cmd);
        return rc;
    }
    OBJ_DESTRUCT(&cmd);

    /* wait for response */
    OBJ_CONSTRUCT(&rsp, orte_buffer_t);
    if(0 > (rc = orte_rml.recv_buffer(ORTE_RML_NAME_SEED, &rsp, ORTE_RML_TAG_RMGR_CLNT))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&rsp);
        return rc;
    }

    rc = orte_rmgr_base_unpack_create_rsp(&rsp, jobid);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&rsp);
        return rc;
    }
    OBJ_DESTRUCT(&rsp);
    return rc;
}

static int orte_rmgr_proxy_cmd(orte_rmgr_cmd_t cmd_id, orte_jobid_t jobid)
{
    orte_buffer_t cmd;
    orte_buffer_t rsp;
    int rc;

    /* construct command */
    OBJ_CONSTRUCT(&cmd, orte_buffer_t);
    rc = orte_rmgr_base_pack_cmd(&cmd, cmd_id, jobid);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&cmd);
        return rc;
    }

    if(0 > (rc = orte_rml.send_buffer(ORTE_RML_NAME_SEED, &cmd, ORTE_RML_TAG_RMGR_SVC, 0))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&cmd);
        return rc;
    }
    OBJ_DESTRUCT(&cmd);

    /* wait for response */
    OBJ_CONSTRUCT(&rsp, orte_buffer_t);
    if(0 > (rc = orte_rml.recv_buffer(ORTE_RML_NAME_SEED, &rsp, ORTE_RML_TAG_RMGR_CLNT))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&rsp);
        return rc;
    }

    rc = orte_rmgr_base_unpack_rsp(&rsp);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&rsp);
        return rc;
    }
    OBJ_DESTRUCT(&rsp);
    return rc;
}


static int orte_rmgr_proxy_query(void)
{
    return orte_rmgr_proxy_cmd(ORTE_RMGR_CMD_QUERY, 0);
}

static int orte_rmgr_proxy_allocate(orte_jobid_t jobid)
{
    return orte_rmgr_proxy_cmd(ORTE_RMGR_CMD_ALLOCATE, jobid);
}

static int orte_rmgr_proxy_deallocate(orte_jobid_t jobid)
{
    return orte_rmgr_proxy_cmd(ORTE_RMGR_CMD_DEALLOCATE, jobid);
}

static int orte_rmgr_proxy_map(orte_jobid_t jobid)
{
    return orte_rmgr_proxy_cmd(ORTE_RMGR_CMD_MAP, jobid);
}

static int orte_rmgr_proxy_launch(orte_jobid_t jobid)
{
    return orte_rmgr_proxy_cmd(ORTE_RMGR_CMD_LAUNCH, jobid);
}

static int orte_rmgr_proxy_terminate_job(orte_jobid_t jobid)
{
    return orte_rmgr_proxy_cmd(ORTE_RMGR_CMD_TERM_JOB, jobid);
}

static int orte_rmgr_proxy_terminate_proc(const orte_process_name_t* proc_name)
{
    orte_buffer_t cmd;
    orte_buffer_t rsp;
    int rc;

    /* construct command */
    OBJ_CONSTRUCT(&cmd, orte_buffer_t);
    rc = orte_rmgr_base_pack_terminate_proc_cmd(&cmd, proc_name);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&cmd);
        return rc;
    }

    if(0 > (rc = orte_rml.send_buffer(ORTE_RML_NAME_SEED, &cmd, ORTE_RML_TAG_RMGR_SVC, 0))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&cmd);
        return rc;
    }
    OBJ_DESTRUCT(&cmd);

    /* wait for response */
    OBJ_CONSTRUCT(&rsp, orte_buffer_t);
    if(0 > (rc = orte_rml.recv_buffer(ORTE_RML_NAME_SEED, &rsp, ORTE_RML_TAG_RMGR_CLNT))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&rsp);
        return rc;
    }

    rc = orte_rmgr_base_unpack_rsp(&rsp);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&rsp);
        return rc;
    }
    OBJ_DESTRUCT(&rsp);
    return ORTE_SUCCESS;
}

static void orte_rmgr_proxy_callback(orte_gpr_notify_data_t *data, void *cbdata)
{
    orte_rmgr_cb_fn_t cbfunc = (orte_rmgr_cb_fn_t)cbdata;
    orte_gpr_keyval_t** keyvals;
    orte_jobid_t jobid;
    int i, j, rc;

    /* get the jobid from the segment name */
    if (ORTE_SUCCESS != (rc = orte_schema.extract_jobid_from_segment_name(&jobid, data->segment))) {
        ORTE_ERROR_LOG(rc);
        ompi_output(0, "[%d,%d,%d] orte_rmgr_proxy_callback: %s\n", 
            ORTE_NAME_ARGS(orte_process_info.my_name), data->segment);
        return;
    }

    /* determine the state change */
    for(i=0; i<data->cnt; i++) {
        orte_gpr_value_t* value = data->values[i];
        keyvals = value->keyvals;
        for(j=0; j<value->cnt; j++) {
            orte_gpr_keyval_t* keyval = keyvals[j];
            if(strcmp(keyval->key, ORTE_PROC_NUM_AT_STG1) == 0) {
                (*cbfunc)(jobid,ORTE_PROC_STATE_AT_STG1);
                continue;
            }
            if(strcmp(keyval->key, ORTE_PROC_NUM_AT_STG2) == 0) {
                (*cbfunc)(jobid,ORTE_PROC_STATE_AT_STG1);
                continue;
            }
            if(strcmp(keyval->key, ORTE_PROC_NUM_AT_STG3) == 0) {
                (*cbfunc)(jobid,ORTE_PROC_STATE_AT_STG3);
                continue;
            }
            if(strcmp(keyval->key, ORTE_PROC_NUM_FINALIZED) == 0) {
                (*cbfunc)(jobid,ORTE_PROC_STATE_FINALIZED);
                continue;
            }
            if(strcmp(keyval->key, ORTE_PROC_NUM_TERMINATED) == 0) {
                (*cbfunc)(jobid,ORTE_PROC_STATE_TERMINATED);
                continue;
            }
            if(strcmp(keyval->key, ORTE_PROC_NUM_ABORTED) == 0) {
                (*cbfunc)(jobid,ORTE_PROC_STATE_ABORTED);
                continue;
            }
        }
    }
}


/*
 *  Shortcut for the multiple steps involved in spawning a new job.
 */


static int orte_rmgr_proxy_spawn(
    orte_app_context_t** app_context,
    size_t num_context,
    orte_jobid_t* jobid,
    orte_rmgr_cb_fn_t cbfunc)
{
    int rc;
    orte_process_name_t* name;
 
    /* 
     * Perform resource discovery.
     */
    if (ORTE_SUCCESS != (rc = orte_rmgr_proxy_query())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /*
     * Initialize job segment and allocate resources
     */
    if (ORTE_SUCCESS != 
        (rc = orte_rmgr_proxy_create(app_context,num_context,jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_rmgr_proxy_allocate(*jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_rmgr_proxy_map(*jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /*
     * setup I/O forwarding 
     */

    if (ORTE_SUCCESS != (rc = orte_ns.create_process_name(&name, 0, *jobid, 0))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_iof.iof_pull(name, ORTE_NS_CMP_JOBID, ORTE_IOF_STDOUT, 1))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_iof.iof_pull(name, ORTE_NS_CMP_JOBID, ORTE_IOF_STDERR, 2))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /*
     * setup callback
     */

    if(NULL != cbfunc) {
        rc = orte_rmgr_base_proc_stage_gate_subscribe(*jobid, orte_rmgr_proxy_callback, (void*)cbfunc);
        if(ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    /*
     * launch the job
     */
    if (ORTE_SUCCESS != (rc = orte_rmgr_proxy_launch(*jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    orte_ns.free_name(&name);
    return ORTE_SUCCESS;
}



