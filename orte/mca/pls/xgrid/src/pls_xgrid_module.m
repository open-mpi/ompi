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
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#import "ompi_config.h"
#import <stdlib.h>
#import <unistd.h>
#import <errno.h>
#import <string.h>
#import <sys/types.h>
#import <sys/stat.h>
#import <sys/wait.h>
#import <fcntl.h>

#import "include/orte_constants.h"
#import "util/argv.h"
#import "util/output.h"
#import "util/session_dir.h"
#import "opal/event/event.h"
#import "runtime/orte_wait.h"
#import "mca/ns/ns.h"
#import "mca/pls/pls.h"
#import "mca/rml/rml.h"
#import "mca/gpr/gpr.h"
#import "mca/errmgr/errmgr.h"
#import "mca/ras/base/ras_base_node.h"
#import "mca/rmaps/base/rmaps_base_map.h"
#import "mca/rmgr/base/base.h"
#import "mca/soh/soh.h"
#import "mca/soh/base/base.h"
#import "pls_xgrid.h"

int orte_pls_xgrid_launch(orte_jobid_t jobid);
int orte_pls_xgrid_terminate_job(orte_jobid_t jobid);
int orte_pls_xgrid_terminate_proc(const orte_process_name_t* proc);
int orte_pls_xgrid_finalize(void);


orte_pls_base_module_1_0_0_t orte_pls_xgrid_module = {
    orte_pls_xgrid_launch,
    orte_pls_xgrid_terminate_job,
    orte_pls_xgrid_terminate_proc,
    orte_pls_xgrid_finalize
};

/**
 * Launch a daemon (bootproxy) on each node. The daemon will be responsible
 * for launching the application.
 */
int
orte_pls_xgrid_launch(orte_jobid_t jobid)
{
    return [mca_pls_xgrid_component.client launchJob:jobid];
}


/**
 * Wait for a pending job to complete.
 */

static void
orte_pls_xgrid_terminate_job_rsp(int status,
                                 orte_process_name_t* peer,
                                 orte_buffer_t* rsp,
                                 orte_rml_tag_t tag,
                                 void* cbdata)
{
    int rc;
    if (ORTE_SUCCESS != (rc = orte_rmgr_base_unpack_rsp(rsp))) {
        ORTE_ERROR_LOG(rc);
    }
}


static void
orte_pls_xgrid_terminate_job_cb(int status,
                                orte_process_name_t* peer,
                                orte_buffer_t* req,
                                orte_rml_tag_t tag,
                                void* cbdata)
{
    /* wait for response */
    int rc;
    if(status < 0) {
        ORTE_ERROR_LOG(status);
        OBJ_RELEASE(req);
        return;
    }

    if(0 > (rc = orte_rml.recv_buffer_nb(peer, ORTE_RML_TAG_RMGR_CLNT,
                                         0, orte_pls_xgrid_terminate_job_rsp, NULL))) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_RELEASE(req);
}


/**
 * Query the registry for all nodes participating in the job
 */
int
orte_pls_xgrid_terminate_job(orte_jobid_t jobid)
{
    char *keys[2];
    char *jobid_string;
    orte_gpr_value_t** values = NULL;
    size_t i, j, num_values = 0;
    int rc;

    if(ORTE_SUCCESS != (rc = orte_ns.convert_jobid_to_string(&jobid_string, jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    asprintf(&keys[0], "%s-%s", ORTE_NODE_BOOTPROXY_KEY, jobid_string);
    keys[1] = NULL;

    rc = orte_gpr.get(
        ORTE_GPR_KEYS_OR|ORTE_GPR_TOKENS_OR,
        ORTE_NODE_SEGMENT,
        NULL,
        keys,
        &num_values,
        &values
        );
    if(rc != ORTE_SUCCESS) {
        free(jobid_string);
        return rc;
    }
    if(0 == num_values) {
        rc = ORTE_ERR_NOT_FOUND;
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    for(i=0; i<num_values; i++) {
        orte_gpr_value_t* value = values[i];
        for(j=0; j<value->cnt; j++) {
            orte_gpr_keyval_t* keyval = value->keyvals[j];
            orte_buffer_t *cmd = OBJ_NEW(orte_buffer_t);
            int ret;
            if(cmd == NULL) {
                rc = ORTE_ERR_OUT_OF_RESOURCE;
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            if(strcmp(keyval->key, keys[0]) != 0) 
                continue;

            /* construct command */
            ret = orte_rmgr_base_pack_cmd(cmd, ORTE_RMGR_CMD_TERM_JOB, jobid);
            if(ORTE_SUCCESS != ret) {
                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(cmd);
                rc = ret;
                continue;
            }

            /* send a terminate message to the bootproxy on each node
	       */
            if(0 > (ret = orte_rml.send_buffer_nb(
                &keyval->value.proc, 
                cmd, 
                ORTE_RML_TAG_RMGR_SVC, 
                0, 
                orte_pls_xgrid_terminate_job_cb, 
                NULL))) {

                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(cmd);
                rc = ret;
                continue;
            }
        }
    }

cleanup:

    free(jobid_string);
    free(keys[0]);

    if(NULL != values) {
        for(i=0; i<num_values; i++) {
            if(NULL != values[i]) {
                OBJ_RELEASE(values[i]);
            }
        }
        free(values);
    }

    if (ORTE_SUCCESS != rc) {
	/* ok, now that we've given the orted a chance to clean everything
	   up nicely, kill everything not so nicely */
	return [mca_pls_xgrid_component.client terminateJob: jobid];
    } else {
	return rc;
    }
}


int
orte_pls_xgrid_terminate_proc(const orte_process_name_t* proc)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}


int
orte_pls_xgrid_finalize(void)
{
    [mca_pls_xgrid_component.client release];
    [mca_pls_xgrid_component.pool release];

    opal_progress_unregister(orte_pls_xgrid_progress);

    /* cleanup any pending recvs */
    orte_rml.recv_cancel(ORTE_RML_NAME_ANY, ORTE_RML_TAG_RMGR_CLNT);
    return ORTE_SUCCESS;
}

