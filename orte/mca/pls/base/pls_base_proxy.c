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
 */

#include "orte_config.h"

#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/mca/base/mca_base_param.h"
#include "orte/mca/pls/base/base.h"
#include "orte/include/orte_constants.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/pls/pls.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ras/base/ras_base_node.h"
#include "orte/mca/rmgr/base/base.h"


int
orte_pls_base_proxy_set_node_name(orte_ras_node_t* node, 
                                  orte_jobid_t jobid, 
                                  orte_process_name_t* name)
{
    orte_gpr_value_t* values[1];
    orte_gpr_value_t value;
    orte_gpr_keyval_t kv_name = {{OBJ_CLASS(orte_gpr_keyval_t),0},ORTE_NODE_BOOTPROXY_KEY,ORTE_NAME};
    orte_gpr_keyval_t* keyvals[1];
    char* jobid_string;
    size_t i;
    int rc;
                                                                                                                  
    if (ORTE_SUCCESS != (rc = orte_ns.convert_jobid_to_string(&jobid_string, jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
                                                                                                                  
    if (ORTE_SUCCESS != (rc = orte_schema.get_node_tokens(&value.tokens, &value.num_tokens, 
        node->node_cellid, node->node_name))) {
        ORTE_ERROR_LOG(rc);
        free(jobid_string);
        return rc;
    }
                                                                                                                  
    asprintf(&kv_name.key, "%s-%s", ORTE_NODE_BOOTPROXY_KEY, jobid_string);
    kv_name.value.proc = *name;
    keyvals[0] = &kv_name;
    value.keyvals = keyvals;
    value.cnt = 1;
    value.addr_mode = ORTE_GPR_OVERWRITE;
    value.segment = ORTE_NODE_SEGMENT;
    values[0] = &value;

    rc = orte_gpr.put(1, values);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }

    free(kv_name.key);
    free(jobid_string);
    for(i=0; i<value.num_tokens; i++)
        free(value.tokens[i]);
    free(value.tokens);
    return rc;
}



static int lookup_set(char *a, char *b, char *c, int default_val,
                      char *token, int *argc, char ***argv)
{
    int id, rc;

    id = mca_base_param_find(a, b, c);
    if (id < 0) {
        id = mca_base_param_register_int(a, b, c, NULL, default_val);
    }
    mca_base_param_lookup_int(id, &rc);
    if (rc) {
        opal_argv_append(argc, argv, token);
    }

    return ORTE_SUCCESS;
}


int orte_pls_base_proxy_mca_argv(int *argc, char ***argv)
{
    lookup_set("orte", "debug", NULL, 0, "--debug", argc, argv);
    lookup_set("orte", "debug", "daemons", 0, "--debug-daemons", argc, argv);
    lookup_set("orte", "debug", "daemons_file", 0, "--debug-daemons-file", argc, argv);

    return ORTE_SUCCESS;
}


/**
 * Wait for a pending job to complete.
 */
static void orte_pls_rsh_terminate_job_rsp(
    int status,
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


static void orte_pls_rsh_terminate_job_cb(
    int status,
    orte_process_name_t* peer,
    orte_buffer_t* req,
    orte_rml_tag_t tag,
    void* cbdata)
{
    /* wait for response */
    int rc;
    if (status < 0) {
        ORTE_ERROR_LOG(status);
        OBJ_RELEASE(req);
        return;
    }

    if (0 > (rc = orte_rml.recv_buffer_nb(peer, ORTE_RML_TAG_RMGR_CLNT, 0, orte_pls_rsh_terminate_job_rsp, NULL))) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_RELEASE(req);
}


int
orte_pls_base_proxy_terminate_job(orte_jobid_t jobid)
{
    char *keys[2];
    char *jobid_string;
    orte_gpr_value_t** values = NULL;
    size_t i, j, num_values = 0;
    int rc;
                                                                                                                           
    if (ORTE_SUCCESS != (rc = orte_ns.convert_jobid_to_string(&jobid_string, jobid))) {
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
    if (rc != ORTE_SUCCESS) {
        free(jobid_string);
        return rc;
    }
    if (0 == num_values) {
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
            if (cmd == NULL) {
                rc = ORTE_ERR_OUT_OF_RESOURCE;
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            if (strcmp(keyval->key, keys[0]) != 0) 
                continue;

            /* construct command */
            ret = orte_rmgr_base_pack_cmd(cmd, ORTE_RMGR_CMD_TERM_JOB, jobid);
            if (ORTE_SUCCESS != ret) {
                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(cmd);
                rc = ret;
                continue;
            }

            /* send a terminate message to the bootproxy on each node */
            if (0 > (ret = orte_rml.send_buffer_nb(
                &keyval->value.proc, 
                cmd, 
                ORTE_RML_TAG_RMGR_SVC, 
                0, 
                orte_pls_rsh_terminate_job_cb, 
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
                                                                                                                           
    if (NULL != values) {
        for(i=0; i<num_values; i++) {
            if (NULL != values[i]) {
                OBJ_RELEASE(values[i]);
            }
        }
        free(values);
    }
    return rc;
}


int
orte_pls_base_proxy_terminate_proc(const orte_process_name_t *proc)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}
