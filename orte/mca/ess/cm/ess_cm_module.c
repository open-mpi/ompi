/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <sys/types.h>
#include <stdio.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif


#include "orte/util/show_help.h"
#include "orte/mca/iof/base/base.h"
#include "orte/mca/plm/base/base.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/ras/base/base.h"
#include "orte/mca/rml/base/base.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/routed/base/base.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/grpcomm/base/base.h"
#include "orte/mca/odls/base/base.h"
#include "orte/mca/notifier/base/base.h"
#include "orte/mca/rmaps/base/base.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/base/base.h"
#include "orte/mca/ess/cm/ess_cm.h"

static int rte_init(void);
static int rte_finalize(void);
static void rte_abort(int status, bool report) __opal_attribute_noreturn__;
static orte_vpid_t proc_get_daemon(orte_process_name_t *proc);
static orte_local_rank_t proc_get_local_rank(orte_process_name_t *proc);
static orte_node_rank_t proc_get_node_rank(orte_process_name_t *proc);
static int update_pidmap(opal_byte_object_t *bo);
static int update_nidmap(opal_byte_object_t *bo);


orte_ess_base_module_t orte_ess_cm_module = {
    rte_init,
    rte_finalize,
    rte_abort,
    NULL, /* don't need a local procs fn */
    proc_get_daemon,
    NULL, /* don't need a proc_get_hostname fn */
    NULL, /* don't need a proc_get_arch fn */
    proc_get_local_rank,
    proc_get_node_rank,
    NULL, /* don't need to update_arch */
    update_pidmap,
    update_nidmap,
    NULL /* ft_event */
};

static int cm_set_name(void);

static int rte_init(void)
{
    orte_job_t *jdata;
    orte_node_t *node;
    orte_proc_t *proc;
    int ret;
    char *error = NULL;
    
    if (ORTE_PROC_IS_CM_APP) {
        /* get our name out of the environment */
        if (ORTE_SUCCESS != (ret = cm_set_name())) {
            error = "orte_ess_cm_set_name";
            goto error;
        }
        
        /* run the prolog */
        if (ORTE_SUCCESS != (ret = orte_ess_base_std_prolog())) {
            error = "orte_ess_base_std_prolog";
            goto error;
        }
        
        /* Setup the communication infrastructure */
        /*
         * Runtime Messaging Layer
         */
        if (ORTE_SUCCESS != (ret = orte_rml_base_open())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_rml_base_open";
            goto error;
        }
        if (ORTE_SUCCESS != (ret = orte_rml_base_select())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_rml_base_select";
            goto error;
        }
        /*
         * Routed system
         */
        if (ORTE_SUCCESS != (ret = orte_routed_base_open())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_routed_base_open";
            goto error;
        }
        if (ORTE_SUCCESS != (ret = orte_routed_base_select())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_routed_base_select";
            goto error;
        }
        /*
         * Group communications
         */
        if (ORTE_SUCCESS != (ret = orte_grpcomm_base_open())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_grpcomm_base_open";
            goto error;
        }
        if (ORTE_SUCCESS != (ret = orte_grpcomm_base_select())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_grpcomm_base_select";
            goto error;
        }
        
        /* enable communication with the rml */
        if (ORTE_SUCCESS != (ret = orte_rml.enable_comm())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_rml.enable_comm";
            goto error;
        }
        
        /* setup the routed info - the selected routed component
         * will know what to do. 
         */
        if (ORTE_SUCCESS != (ret = orte_routed.init_routes(ORTE_PROC_MY_NAME->jobid, NULL))) {
            ORTE_ERROR_LOG(ret);
            error = "orte_routed.init_routes";
            goto error;
        }
        
        /* setup the notifier system */
        if (ORTE_SUCCESS != (ret = orte_notifier_base_open())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_notifer_open";
            goto error;
        }
        if (ORTE_SUCCESS != (ret = orte_notifier_base_select())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_notifer_select";
            goto error;
        }
        
        return ORTE_SUCCESS;        
    }
    
    
    /* if we are not an APP, then we are the CM itself, so setup
     * for that role
     */
    
    /* initialize the global list of local children and job data */
    OBJ_CONSTRUCT(&orte_local_children, opal_list_t);
    OBJ_CONSTRUCT(&orte_local_jobdata, opal_list_t);
    
    /* run the prolog */
    if (ORTE_SUCCESS != (ret = orte_ess_base_std_prolog())) {
        error = "orte_ess_base_std_prolog";
        goto error;
    }
    
    /* Open the PLM so we can start processes */
    if (ORTE_SUCCESS != (ret = orte_plm_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_plm_base_open";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_plm_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_plm_base_select";
        goto error;
    }
    
    /* give ourselves a unique name */
    if (ORTE_SUCCESS != (ret = orte_plm.set_hnp_name())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_plm_set_hnp_name";
        goto error;
    }
    
    /* Setup the communication infrastructure */
    /*
     * Runtime Messaging Layer
     */
    if (ORTE_SUCCESS != (ret = orte_rml_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rml_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_rml_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rml_base_select";
        goto error;
    }
    /*
     * Routed system
     */
    if (ORTE_SUCCESS != (ret = orte_routed_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_routed_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_routed_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_routed_base_select";
        goto error;
    }
    /*
     * Group communications
     */
    if (ORTE_SUCCESS != (ret = orte_grpcomm_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_grpcomm_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_grpcomm_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_grpcomm_base_select";
        goto error;
    }
    
    /* Now provide a chance for the PLM
     * to perform any module-specific init functions. This
     * needs to occur AFTER the communications are setup
     * as it may involve starting a non-blocking recv
     */
    if (ORTE_SUCCESS != (ret = orte_plm.init())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_plm_init";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_ras_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_ras_base_open";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_ras_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_ras_base_find_available";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_rmaps_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rmaps_base_open";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_rmaps_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rmaps_base_find_available";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_errmgr_base_open())) {
        error = "orte_errmgr_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_errmgr_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_errmgr_base_select";
        goto error;
    }
    
    /* Open/select the odls */
    if (ORTE_SUCCESS != (ret = orte_odls_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_odls_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_odls_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_odls_base_select";
        goto error;
    }
    
    /* enable communication with the rml */
    if (ORTE_SUCCESS != (ret = orte_rml.enable_comm())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rml.enable_comm";
        goto error;
    }
    
    /* setup the global job and node arrays */
    orte_job_data = OBJ_NEW(opal_pointer_array_t);
    if (ORTE_SUCCESS != (ret = opal_pointer_array_init(orte_job_data,
                                                       1,
                                                       ORTE_GLOBAL_ARRAY_MAX_SIZE,
                                                       1))) {
        ORTE_ERROR_LOG(ret);
        error = "setup job array";
        goto error;
    }
    
    orte_node_pool = OBJ_NEW(opal_pointer_array_t);
    if (ORTE_SUCCESS != (ret = opal_pointer_array_init(orte_node_pool,
                                                       ORTE_GLOBAL_ARRAY_BLOCK_SIZE,
                                                       ORTE_GLOBAL_ARRAY_MAX_SIZE,
                                                       ORTE_GLOBAL_ARRAY_BLOCK_SIZE))) {
        ORTE_ERROR_LOG(ret);
        error = "setup node array";
        goto error;
    }
    
    /* Setup the job data object for the daemons */        
    /* create and store the job data object */
    jdata = OBJ_NEW(orte_job_t);
    jdata->jobid = ORTE_PROC_MY_NAME->jobid;
    opal_pointer_array_set_item(orte_job_data, 0, jdata);
    
    /* create and store a node object where we are */
    node = OBJ_NEW(orte_node_t);
    node->name = strdup(orte_process_info.nodename);
    node->arch = orte_process_info.arch;
    node->index = opal_pointer_array_add(orte_node_pool, node);
    
    /* create and store a proc object for us */
    proc = OBJ_NEW(orte_proc_t);
    proc->name.jobid = ORTE_PROC_MY_NAME->jobid;
    proc->name.vpid = ORTE_PROC_MY_NAME->vpid;
    proc->pid = orte_process_info.pid;
    proc->rml_uri = orte_rml.get_contact_info();
    proc->state = ORTE_PROC_STATE_RUNNING;
    OBJ_RETAIN(node);  /* keep accounting straight */
    proc->node = node;
    proc->nodename = node->name;
    opal_pointer_array_add(jdata->procs, proc);
    
    /* record that the daemon (i.e., us) is on this node 
     * NOTE: we do not add the proc object to the node's
     * proc array because we are not an application proc.
     * Instead, we record it in the daemon field of the
     * node object
     */
    OBJ_RETAIN(proc);   /* keep accounting straight */
    node->daemon = proc;
    node->daemon_launched = true;
    node->state = ORTE_NODE_STATE_UP;
    
    /* record that the daemon job is running */
    jdata->num_procs = 1;
    jdata->state = ORTE_JOB_STATE_RUNNING;
    
    /* setup the routed info - the selected routed component
     * will know what to do. 
     */
    if (ORTE_SUCCESS != (ret = orte_routed.init_routes(ORTE_PROC_MY_NAME->jobid, NULL))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_routed.init_routes";
        goto error;
    }
    
    /* setup I/O forwarding system - must come after we init routes */
    if (ORTE_SUCCESS != (ret = orte_iof_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_iof_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_iof_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_iof_base_select";
        goto error;
    }
    
    /* setup the notifier system */
    if (ORTE_SUCCESS != (ret = orte_notifier_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_notifer_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_notifier_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_notifer_select";
        goto error;
    }
    
    return ORTE_SUCCESS;        

error:
    orte_show_help("help-orte-runtime.txt",
                   "orte_init:startup:internal-failure",
                   true, error, ORTE_ERROR_NAME(ret), ret);
    
    return ret;
}

static int rte_finalize(void)
{
    opal_list_item_t *item;
    orte_node_t *node;
    orte_job_t *job;
    int i;
    
    orte_notifier_base_close();
    
    orte_odls_base_close();
    
    orte_wait_finalize();
    
    /* finalize selected modules so they can de-register
     * any receives
     */
    orte_rmaps_base_close();
    orte_plm_base_close();
    orte_errmgr_base_close();
    
    /* now can close the rml and its friendly group comm */
    orte_grpcomm_base_close();
    orte_routed_base_close();
    orte_rml_base_close();
    
    /* cleanup the global list of local children and job data */
    while (NULL != (item = opal_list_remove_first(&orte_local_children))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&orte_local_children);
    while (NULL != (item = opal_list_remove_first(&orte_local_jobdata))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&orte_local_jobdata);
    
    /* cleanup the job and node info arrays */
    if (NULL != orte_node_pool) {
        for (i=0; i < orte_node_pool->size; i++) {
            if (NULL != (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool,i))) {
                OBJ_RELEASE(node);
            }
        }
        OBJ_RELEASE(orte_node_pool);
    }
    if (NULL != orte_job_data) {
        for (i=0; i < orte_job_data->size; i++) {
            if (NULL != (job = (orte_job_t*)opal_pointer_array_get_item(orte_job_data,i))) {
                OBJ_RELEASE(job);
            }
        }
        OBJ_RELEASE(orte_job_data);
    }
    
    /* clean out the global structures */
    orte_proc_info_finalize();
    
    return ORTE_SUCCESS;    
}

/*
 * If we are a cm, it could be beneficial to get a core file, so
 * we call abort.
 */
static void rte_abort(int status, bool report)
{
    /* do NOT do a normal finalize as this will very likely
     * hang the process. We are aborting due to an abnormal condition
     * that precludes normal cleanup 
     *
     * We do need to do the following bits to make sure we leave a 
     * clean environment. Taken from orte_finalize():
     * - Assume errmgr cleans up child processes before we exit.
     */
    
    /* - Clean out the global structures 
     * (not really necessary, but good practice)
     */
    orte_proc_info_finalize();
    
    /* Now exit/abort */
    if (report) {
        abort();
    }
    
    /* otherwise, just exit */
    exit(status);
}

static int cm_set_name(void)
{
    char *jobid_str, *procid_str;
    int id, rc;
    orte_jobid_t jobid;
    orte_vpid_t vpid;
    
    id = mca_base_param_register_string("orte", "ess", "jobid", NULL, NULL);
    mca_base_param_lookup_string(id, &jobid_str);
    if (NULL == jobid_str) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    if (ORTE_SUCCESS != (rc = orte_util_convert_string_to_jobid(&jobid, jobid_str))) {
        ORTE_ERROR_LOG(rc);
        return(rc);
    }
    free(jobid_str);
    
    id = mca_base_param_register_string("orte", "ess", "vpid", NULL, NULL);
    mca_base_param_lookup_string(id, &procid_str);
    if (NULL == procid_str) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    if (ORTE_SUCCESS != (rc = orte_util_convert_string_to_vpid(&vpid, procid_str))) {
        ORTE_ERROR_LOG(rc);
        return(rc);
    }
    free(procid_str);
    
    ORTE_PROC_MY_NAME->jobid = jobid;
    ORTE_PROC_MY_NAME->vpid = vpid;
    
    OPAL_OUTPUT_VERBOSE((1, orte_ess_base_output,
                         "ess:cm set name to %s", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* get the non-name common environmental variables */
    if (ORTE_SUCCESS != (rc = orte_ess_env_get())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ORTE_SUCCESS;
}

static orte_proc_t* find_proc(orte_process_name_t *proc)
{
    orte_job_t *jdata;
    
    if (NULL == (jdata = orte_get_job_data_object(proc->jobid))) {
        return NULL;
    }
    
    return (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, (int)proc->vpid);
}

static orte_vpid_t proc_get_daemon(orte_process_name_t *proc)
{
    orte_proc_t *pdata;
    
    /* get the job data */
    if (NULL == (pdata = find_proc(proc))) {
        return ORTE_VPID_INVALID;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:hnp: proc %s is hosted by daemon %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         ORTE_VPID_PRINT(pdata->node->daemon->name.vpid)));
    
    return pdata->node->daemon->name.vpid;
}

static orte_local_rank_t proc_get_local_rank(orte_process_name_t *proc)
{
    orte_proc_t *pdata;
    
    if (NULL == (pdata = find_proc(proc))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_LOCAL_RANK_INVALID;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:hnp: proc %s has local rank %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         (int)pdata->local_rank));
    
    return pdata->local_rank;
}

static orte_node_rank_t proc_get_node_rank(orte_process_name_t *proc)
{
    orte_proc_t *pdata;
    
    if (NULL == (pdata = find_proc(proc))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_NODE_RANK_INVALID;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:hnp: proc %s has node rank %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         (int)pdata->node_rank));
    
    return pdata->node_rank;
}

static int update_pidmap(opal_byte_object_t *bo)
{
    /* there is nothing to do here - the HNP can resolve
     * all requests directly from its internal data. However,
     * we do need to free the data in the byte object to
     * be consistent with other modules
     */
    if (NULL != bo && NULL != bo->bytes) {
        free(bo->bytes);
    }
    return ORTE_SUCCESS;
}

static int update_nidmap(opal_byte_object_t *bo)
{
    /* there is nothing to do here - the HNP can resolve
     * all requests directly from its internal data. However,
     * we do need to free the data in the byte object to
     * be consistent with other modules
     */
    if (NULL != bo && NULL != bo->bytes) {
        free(bo->bytes);
    }
    return ORTE_SUCCESS;
}
