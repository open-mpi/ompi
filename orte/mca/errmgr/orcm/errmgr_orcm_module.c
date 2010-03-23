/*
 * Copyright (c) 2009-2010 The Trustees of Indiana University.
 *                         All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "opal/util/output.h"
#include "opal/util/opal_environ.h"
#include "opal/util/basename.h"
#include "opal/util/argv.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/orte_globals.h"
#include "opal/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/base.h"
#include "orte/mca/plm/base/plm_private.h"
#include "orte/mca/filem/filem.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/runtime/orte_wait.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/errmgr/base/base.h"

#include "errmgr_orcm.h"


/*
 * Module functions: Global
 */
static int init(void);
static int finalize(void);

static int predicted_fault(char ***proc_list,
                           char ***node_list,
                           char ***suggested_nodes);

static int process_fault(orte_job_t *jdata,
                         orte_process_name_t *proc_name,
                         orte_proc_state_t state,
                         int *stack_state);

static int suggest_map_targets(orte_proc_t *proc,
                               orte_node_t *oldnode,
                               opal_list_t *node_list);

static int ft_event(int state);



/******************
 * ORCM module
 ******************/
orte_errmgr_base_module_t orte_errmgr_orcm_module = {
    NULL, /* proc_aborted     (old interface) */
    NULL, /* incomplete_start (old interface) */
    NULL, /* comm_failed      (old interface) */
    NULL, /* abort            (old interface) */
    init,
    finalize,
    predicted_fault,
    process_fault,
    suggest_map_targets,
    ft_event
};

/************************
 * API Definitions
 ************************/
static int init(void)
{
    return ORTE_SUCCESS;
}

static int finalize(void)
{
    return ORTE_SUCCESS;
}

static int predicted_fault(char ***proc_list,
                           char ***node_list,
                           char ***suggested_nodes)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

static int process_fault(orte_job_t *jdata,
                         orte_process_name_t *proc,
                         orte_proc_state_t state,
                         int *stack_state)
{
    orte_job_t *jnew;
    orte_proc_t *pdata;
    orte_app_context_t *app=NULL;
    orte_node_t *node, *newnode;
    orte_proc_t *daemon, *nodeproc;
    opal_value_array_t jobs;
    bool found;
    int i;
    size_t j;

    *stack_state ^= ORTE_ERRMGR_STACK_STATE_JOB_ABORT;

    OPAL_OUTPUT_VERBOSE((2, orte_errmgr_base_output,
                         "errmgr:orcm:process_fault() "
                         "------- %s fault reported! proc %s (0x%x)",
                         (proc->jobid == ORTE_PROC_MY_NAME->jobid ? "Daemon" : "App. Process"),
                         ORTE_NAME_PRINT(proc),
                         state ));
    /* get the app - just for output purposes in case of error */
    app = opal_pointer_array_get_item(jdata->apps, 0);

    /* Remove the route to this process since it is dead */
    orte_routed.delete_route(proc);

    /****    NON-DAEMON PROC FAILED    ****/
    if (proc->jobid != ORTE_PROC_MY_NAME->jobid) {
        /* if the proc failed to start or we killed it by cmd,
         * don't attempt to restart it as this can lead to an
         * infinite loop
         */
        if (ORTE_PROC_STATE_FAILED_TO_START == state) {
            opal_output(0, "APPLICATION %s FAILED TO START", app->app);
            return ORTE_SUCCESS;
        }
        
        /* if the proc was terminated by cmd, then do nothing */
        if (ORTE_PROC_STATE_KILLED_BY_CMD == state) {
            opal_output(0, "APPLICATION %s KILLED BY COMMAND", app->app);
            return ORTE_SUCCESS;
        }
        
        /* get the proc_t object for this process */
        pdata = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, proc->vpid);
        if (NULL == pdata) {
            opal_output(0, "Data for proc %s could not be found", ORTE_NAME_PRINT(proc));
            return ORTE_ERR_NOT_FOUND;
        }
        /* proc just died - save the node where this proc was located */
        node = pdata->node;
        /* increment restarts */
        pdata->restarts++;
        /* have we exceeded #restarts? */
        if (jdata->max_restarts < pdata->restarts) {
            opal_output(0, "Max restarts for proc %s of app %s has been exceeded - process will not be restarted",
                        ORTE_NAME_PRINT(proc), app->app);
            return ORTE_SUCCESS;
        }
        /* reset the job params for restart */
        orte_plm_base_reset_job(jdata);
        
        /* restart the job - the spawn function will remap and
         * launch the replacement proc(s)
         */
        OPAL_OUTPUT_VERBOSE((2, orte_errmgr_base_output,
                             "%s RESTARTING APP: %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(proc)));
        
        if (ORTE_SUCCESS != orte_plm.spawn(jdata)) {
            opal_output(0, "FAILED TO RESTART APP %s", app->app);
            orte_trigger_event(&orte_exit);
            return ORTE_ERROR;
        }
        /* get the new node */
        newnode = pdata->node;
        /* report what we did */
        opal_output(0, "Proc %s:%s aborted on node %s and was restarted on node %s\n\n",
                    app->app, ORTE_NAME_PRINT(proc), node->name, newnode->name);
        
        
        return ORTE_SUCCESS;
    }
    
    /* if it was a daemon that failed, then we have to
     * treat it differently
     */
    if (ORTE_PROC_MY_NAME->jobid == proc->jobid) {
        OPAL_OUTPUT_VERBOSE((2, orte_errmgr_base_output,
                             "%s Daemon %s failed",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_VPID_PRINT(proc->vpid)));
        /* need to relaunch all the apps that were on
         * the node where this daemon was running as
         * they either died along with the node, or will
         * have self-terminated when the daemon died
         */
        if (NULL == (daemon = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, proc->vpid))) {
            /* nothing we can do - abort things */
            opal_output(0, "FAILED TO GET DAEMON OBJECT");
            return ORTE_ERROR;
        }
        /* flag the daemon state to indicate it terminated - this will
         * cause the daemon to be restarted IF required for starting
         * procs on that node
         */
        daemon->state = ORTE_PROC_STATE_ABORTED;
        /* identify the node where the daemon was running */
        node = daemon->node;
        /* release the contact info, if not already done */
        if (NULL != daemon->rml_uri) {
            free(daemon->rml_uri);
            daemon->rml_uri = NULL;
        }
        /* setup to track the jobs on this node */
        OBJ_CONSTRUCT(&jobs, opal_value_array_t);
        opal_value_array_init(&jobs, sizeof(orte_jobid_t));
        /* cycle through the node's procs */
        for (i=0; i < node->procs->size; i++) {
            if (NULL == (nodeproc = (orte_proc_t*)opal_pointer_array_get_item(node->procs, i))) {
                continue;
            }
            /* set the proc to abnormally terminated */
            nodeproc->state = ORTE_PROC_STATE_ABORTED;
            /* increment restarts */
            nodeproc->restarts++;
            /* check if this proc's jobid is already in array */
            found = false;
            for (j=0; j < opal_value_array_get_size(&jobs); j++) {
                if (nodeproc->name.jobid == OPAL_VALUE_ARRAY_GET_ITEM(&jobs, orte_jobid_t, j)) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                /* add it */
                opal_value_array_append_item(&jobs, &nodeproc->name.jobid);
            }
        }
        OPAL_OUTPUT_VERBOSE((2, orte_errmgr_base_output,
                             "%s RESTARTING APPS FROM NODE: %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             node->name));
        for (j=0; j < opal_value_array_get_size(&jobs); j++) {
            if (NULL == (jnew = orte_get_job_data_object(OPAL_VALUE_ARRAY_GET_ITEM(&jobs, orte_jobid_t, j)))) {
                /* nothing we can do - abort things */
                opal_output(0, "FAILED TO GET JOB OBJECT TO BE RESTARTED");
                return ORTE_ERROR;
            }
            /* reset the job params for restart */
            orte_plm_base_reset_job(jnew);
            /* restart the job - the spawn function will remap and
             * launch the replacement proc(s)
             */
            OPAL_OUTPUT_VERBOSE((2, orte_errmgr_base_output,
                                 "%s RESTARTING JOB %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_JOBID_PRINT(jnew->jobid)));
            if (ORTE_SUCCESS != orte_plm.spawn(jnew)) {
                opal_output(0, "FAILED TO RESTART APPS FROM NODE: %s", node->name);
                return ORTE_ERROR;
            }    
        }
        opal_output(0, "Daemon %s on node %s aborted - procs were restarted elsewhere\n\n",
                    ORTE_NAME_PRINT(proc), node->name);
        /* all done - cleanup and leave */
        OBJ_DESTRUCT(&jobs);
        return ORTE_ERROR;
    }
    
    /* save */
    return ORTE_SUCCESS;
}

static int suggest_map_targets(orte_proc_t *proc,
                               orte_node_t *oldnode,
                               opal_list_t *node_list)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

int ft_event(int state)
{
    return ORTE_SUCCESS;
}

/*****************
 * Local Functions
 *****************/
