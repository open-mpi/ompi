/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/* Much of the code in this file is taken from the file ll_get_machine_list.c, 
 * which is provided by IBM as part of their sample programs for LoadLeveler
 * in the samples/llmpich directory.
 * 
 * IBM has approved the release of the sample code for Loadleveler under the 
 * BSD license.  Consequently, a more restrictive licensing clause that was
 * originally associated with the sample code and replicated here has been
 * removed.
 */
 
#include "orte_config.h"

#include <errno.h>
#include <unistd.h>
#include <string.h>

#include <llapi.h>

#include "opal/util/argv.h"
#include "opal/util/output.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/orte_globals.h"
#include "orte/constants.h"

#include "orte/mca/ras/base/ras_private.h"
#include "ras_loadleveler.h"


/*
 * Local functions
 */
static int orte_ras_loadleveler_allocate(opal_list_t *nodes);
static int orte_ras_loadleveler_finalize(void);
static int orte_ras_loadleveler_get_hostlist(int * num_hosts, char*** hostlist);


/*
 * Global variable
 */
orte_ras_base_module_t orte_ras_loadleveler_module = {
    orte_ras_loadleveler_allocate,
    orte_ras_loadleveler_finalize
};


/*
 * Discover available (pre-allocated) nodes.  Allocate the
 * requested number of nodes/process slots to the job.
 */
static int orte_ras_loadleveler_allocate(opal_list_t *nodes)
{
    int i, ret=ORTE_SUCCESS;
    opal_list_item_t* item;
    orte_node_t* node;
    char ** hostlist = NULL;
    int num_hosts = 0;

    ret = orte_ras_loadleveler_get_hostlist(&num_hosts, &hostlist);
    if(ORTE_SUCCESS != ret) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    for (i = 0; i < num_hosts; i++) {
        /* check for duplicated nodes */
        for (item = opal_list_get_first(nodes); 
             opal_list_get_end(nodes) != item;
             item = opal_list_get_next(item)) {
             node = (orte_node_t*) item;
             if (0 == strcmp(node->name, hostlist[i])) {
                ++node->slots;
                break;
            }
        }
     
        if(opal_list_get_end(nodes) == item) {
            /* we did not find a duplicate, so add a new item to the list */
            node = OBJ_NEW(orte_node_t);
            if (NULL == node) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                ret = ORTE_ERR_OUT_OF_RESOURCE;
                goto cleanup;
            }
            node->name = strdup(hostlist[i]);
            node->state = ORTE_NODE_STATE_UP;
            node->slots_inuse = 0;
            node->slots_max = 0;
            node->slots = 1;
            opal_list_append(nodes, &node->super);
        }   
    }
    
cleanup:
    opal_argv_free(hostlist);
            
    return ret;
}

/*
 * There's really nothing to do here
 */
static int orte_ras_loadleveler_finalize(void)
{
    opal_output(orte_ras_base.ras_output, 
                "ras:loadleveler:finalize: success (nothing to do)");
    return ORTE_SUCCESS;
}

/*
 * get the hostlist from LoadLeveler
 * *hostlist should either by NULL or a valid argv and *num_hosts
 * should be 0 or the number of elements in the hostlist argv
 */
static int orte_ras_loadleveler_get_hostlist(int* num_hosts, char*** hostlist)
{
    LL_element *queryObject = NULL, *job = NULL, *step = NULL;
    LL_element *node = NULL, *task = NULL, *task_instance = NULL;
    int rc, obj_count, err_code, ll_master_task, job_step_count;
    char *ll_step_id= NULL, *job_step_list[2], *task_machine_name = NULL;
    char *schedd_host_name = NULL;
    int  step_mode;
 
    /* Get the step ID from LOADL_STEP_ID environment variable. */
    if(NULL == (ll_step_id = getenv("LOADL_STEP_ID"))) {
        opal_output(orte_ras_base.ras_output,
                    "ras:loadleveler:get:hostlist: could not get LOADL_STEP_ID "
                    "from environment!");
        return ORTE_ERROR;
    }

    job_step_list[0] = ll_step_id;
    job_step_list[1] = NULL;

    /* STEP 1: Get Job object from Central Manager to find out the name of the 
     * Schedd daemon that handles this job. In a Multicluster environment we 
     * can not get the schedd name from the job step id. */

    /* Initialize the LL API. Specify that query type is JOBS. */
    if(NULL == (queryObject = ll_query(JOBS))) {
        opal_output(orte_ras_base.ras_output,
                    "ras:loadleveler:get:hostlist: 1 ll_query faild on JOBS!");
        return ORTE_ERROR;
    }

    /* Specify that this is a QUERY_STEPID type of query. */
    rc = ll_set_request(queryObject, QUERY_STEPID, job_step_list, ALL_DATA);
    if(0 > rc) {
        opal_output(orte_ras_base.ras_output,
                    "ras:loadleveler:get:hostlist: 1 ll_set_request failed: "
                    "error %d!", rc);
        return ORTE_ERROR;
    }

    /* Get a Job object from LoadL_schedd that contains the relevant job step */
    job = ll_get_objs(queryObject, LL_CM, NULL, &obj_count, &err_code);
    if(NULL == job) {
        opal_output(orte_ras_base.ras_output,
                    "ras:loadleveler:get:hostlist: ll_get_objs LL_CM "
                    "failed: err_code=%d", err_code);
        return ORTE_ERROR;
    }

    if (obj_count != 1) {  /* Only 1 Job object is expected. */
        opal_output(orte_ras_base.ras_output,
                    "ras:loadleveler:get:hostlist: ll_get_objs LL_CM "
                    "expected one job to match, got %d!", obj_count);
        return ORTE_ERROR;
    }

    if(0 != (rc = ll_get_data(job, LL_JobSchedd, &schedd_host_name))) {
        opal_output(orte_ras_base.ras_output,
                    "ras:loadleveler:get:hostlist: ll_get_data LL_JobSchedd"
                    " failure, RC= %d!", rc);
        return ORTE_ERROR;
    }
    if (schedd_host_name != NULL) {
        job_step_list[0] = ll_step_id;
        job_step_list[1] = NULL;
    } else {
        opal_output(orte_ras_base.ras_output,
                    "ras:loadleveler:get:hostlist: ll_get_data() Error: Could "
                    "not determine managing schedd for job %s.\n",
                    job_step_list[0]);
        return ORTE_ERROR;
    }
    ll_free_objs(queryObject);
    ll_deallocate(queryObject);

    /* STEP 2: Get Job object from Schedd that manages this job step.   */
    /* Only schedd query gives us all the relevant task instance info.  */

    /* Initialize the LL API. Specify that query type is JOBS. */
    if(NULL == (queryObject = ll_query(JOBS))) {
        opal_output(orte_ras_base.ras_output,
                    "ras:loadleveler:get:hostlist: 2 ll_query faild on JOBS!");
        return ORTE_ERROR;
    }

    /* Specify that this is a QUERY_STEPID type of query. */
    rc = ll_set_request(queryObject, QUERY_STEPID, job_step_list, ALL_DATA);
    if(0 != rc) {
        opal_output(orte_ras_base.ras_output,
                    "ras:loadleveler:get:hostlist: 2 ll_set_request failed: "
                    "error %d!", rc);
        return ORTE_ERROR;
    }

    /* Get a Job object from LoadL_schedd that contains the relevant job step */
    job = ll_get_objs(queryObject, LL_SCHEDD, schedd_host_name, &obj_count, 
                      &err_code);
    if(NULL == job) {
        opal_output(orte_ras_base.ras_output,
                    "ras:loadleveler:get:hostlist: ll_get_objs LL_SCHEDD "
                    "failed: err_code=%d", err_code);
        return ORTE_ERROR;
    }

    if (obj_count != 1) {  /* Only 1 Job object is expected. */
        opal_output(orte_ras_base.ras_output,
                    "ras:loadleveler:get:hostlist: ll_get_objs LL_SCHEDD "
                    "expected one job to match, got %d!", obj_count);
        return ORTE_ERROR;
    }

    if(0 != (rc = ll_get_data(job, LL_JobStepCount, &job_step_count))) {
        opal_output(orte_ras_base.ras_output,
                    "ras:loadleveler:get:hostlist: ll_get_data LL_JobStepCount"
                    " failure, RC= %d!", rc);
        return ORTE_ERROR;
    }
    if (job_step_count != 1) { /* Only 1 Job Step object is expected. */
        opal_output(orte_ras_base.ras_output,
                    "ras:loadleveler:get:hostlist: ll_get_data LL_JobStepCount"
                    " expected one jobstep to match, got %d!", job_step_count);
        return ORTE_ERROR;
    }

    step = NULL;
    if(0 != (rc = ll_get_data(job, LL_JobGetFirstStep, &step))) {
        opal_output(orte_ras_base.ras_output,
                    "ras:loadleveler:get:hostlist: 3 ll_get_data: failure on "
                    "LL_JobGetFirstStep. RC= %d!", rc);
        return ORTE_ERROR;
    }
    if(NULL == step) {
        opal_output(orte_ras_base.ras_output,
                    "ras:loadleveler:get:hostlist: 3 ll_get_data: Error: "
                    "Unable to obtain Job Step information.\n");
        return ORTE_ERROR;
    }

    step_mode = -1;
    if(0 != (rc = ll_get_data(step, LL_StepParallelMode, &step_mode))) {
        opal_output(orte_ras_base.ras_output,
                    "ras:loadleveler:get:hostlist: 4 ll_get_data: failure on "
                    "LL_StepParallelMode. RC= %d!", rc);
        return ORTE_ERROR;
    }
    
    /* Serial job step: step_mode==0; Parallel: step_mode==1; Others:2,3,4. */
    if ((step_mode != 0) && (step_mode != 1)) {
        opal_output(orte_ras_base.ras_output,
                    "ras:loadleveler:get:hostlist: We support only Serial and "
                    "Parallel LoadLeveler job types. PVM, NQS, and Blue Gene"
                    "jobs are not supported by the LoadLeveler RAS!");
        return ORTE_ERROR;
    }
                        
    if(step_mode == 0) { /* serial job */
        node = NULL;
        if(0 != (rc = ll_get_data(step, LL_StepGetFirstNode, &node))) {
            opal_output(orte_ras_base.ras_output,
                        "ras:loadleveler:get:hostlist: ll_get_data: failure "
                        "on serial LL_StepGetFirstNode. RC= %d!", rc);
            return ORTE_ERROR;
        }
        task = NULL;
        if(0 != (rc = ll_get_data(node, LL_NodeGetFirstTask, &task))) {
            opal_output(orte_ras_base.ras_output,
                        "ras:loadleveler:get:hostlist: ll_get_data: failure "
                        "on serial LL_NodeGetFirstTask. RC= %d!", rc);
            return ORTE_ERROR;
        }
        task_instance = NULL;
        rc = ll_get_data(task, LL_TaskGetFirstTaskInstance, &task_instance);
        if(0 != rc) {
            opal_output(orte_ras_base.ras_output,
                        "ras:loadleveler:get:hostlist: ll_get_data: failure "
                        "on serial LL_TaskGetFirstInstance. RC= %d!", rc);
            return ORTE_ERROR;
        }
        task_machine_name = NULL;
        if(0 != (rc = ll_get_data(task_instance, LL_TaskInstanceMachineName, 
                                  &task_machine_name))) {
            opal_output(orte_ras_base.ras_output,
                        "ras:loadleveler:get:hostlist: ll_get_data: failure "
                        "on serial LL_TaskInstanceMachineName. RC= %d!", rc);
            return ORTE_ERROR;
        }
        opal_argv_append(num_hosts, hostlist, task_machine_name);

    } else { /* parallel job */

        node = NULL;
        if(0 != (rc = ll_get_data(step, LL_StepGetFirstNode, &node))) {
            opal_output(orte_ras_base.ras_output,
                        "ras:loadleveler:get:hostlist: ll_get_data: failure "
                        "on LL_StepGetFirstNode. RC= %d!", rc);
            return ORTE_ERROR;
        }
    
        while(NULL != node) {     /* Loop through the "Node" objects. */
            task = NULL;
            if(0 != (rc = ll_get_data(node, LL_NodeGetFirstTask, &task))) {
                opal_output(orte_ras_base.ras_output,
                            "ras:loadleveler:get:hostlist: ll_get_data: failure "
                            "on LL_NodeGetFirstTask. RC= %d!", rc);
                return ORTE_ERROR;
            }
        
            while(task) {  /* Loop through the "Task" objects. */
                ll_master_task = 0;
                rc = ll_get_data(task, LL_TaskIsMaster, &ll_master_task);
                if(0 != rc) {
                    opal_output(orte_ras_base.ras_output,
                                "ras:loadleveler:get:hostlist: ll_get_data: " 
                                "failure on LL_TaskIsMaster. RC= %d!", rc);
                    return ORTE_ERROR;
                }
            
                /* The "master task" Task object is a LoadLeveler abstraction 
                 * and is not relevant here. Look at only Task objects that 
                 * are not "master".*/
                if (!ll_master_task) {
                    task_instance = NULL;
                    if(0 != (rc = ll_get_data(task, LL_TaskGetFirstTaskInstance,
                                              &task_instance))) {
                        opal_output(orte_ras_base.ras_output,
                                    "ras:loadleveler:get:hostlist: ll_get_data:"
                                    " failure on LL_TaskGetFirstTaskInstance."
                                    " RC= %d!", rc);
                        return ORTE_ERROR;
                    }
                
                    /* Loop through the "Task Instance" objects. */
                    while (task_instance) {
                        task_machine_name = NULL;
                        rc = ll_get_data(task_instance, 
                                         LL_TaskInstanceMachineName, 
                                         &task_machine_name);
                        if(0 != rc) {
                            opal_output(orte_ras_base.ras_output,
                                        "ras:loadleveler:get:hostlist: ll_get_data:"
                                        " failure on LL_TaskInstanceMachineName"
                                        "RC= %d!", rc);
                            return ORTE_ERROR;
                        }
                        opal_argv_append(num_hosts, hostlist, task_machine_name);
                        task_instance = NULL;
                        rc = ll_get_data(task, LL_TaskGetNextTaskInstance, 
                                         &task_instance);
                        if(0 != rc) {
                            opal_output(orte_ras_base.ras_output,
                                        "ras:loadleveler:get:hostlist: ll_get_data:"
                                        " failure on LL_TaskGetNextTaskInstance. "
                                        "RC= %d!", rc);
                            return ORTE_ERROR;
                        }
                    }
                }
                task = NULL;
                if(0 != (rc = ll_get_data(node, LL_NodeGetNextTask, &task))) {
                    opal_output(orte_ras_base.ras_output,
                                "ras:loadleveler:get:hostlist: ll_get_data: " 
                                "failure on LL_NodeGetNextTask. RC= %d!", rc);
                    return ORTE_ERROR;
                }
            }
            node = NULL;
            if(0 != (rc = ll_get_data(step, LL_StepGetNextNode, &node))) {
                opal_output(orte_ras_base.ras_output,
                            "ras:loadleveler:get:hostlist: ll_get_data: "
                            "failure on LL_StepGetNextNode. RC= %d!", rc);
                return ORTE_ERROR;
            }
        }
    }
    ll_free_objs(queryObject);
    ll_deallocate(queryObject);

    return ORTE_SUCCESS;
}
