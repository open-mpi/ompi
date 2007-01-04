/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "orte_config.h"
#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */

#include "orte/orte_constants.h"

#include "opal/class/opal_list.h"
#include "opal/util/output.h"
#include "opal/util/trace.h"

#include "orte/mca/rds/rds.h"
#include "orte/mca/ras/ras.h"
#include "orte/mca/rmaps/rmaps.h"
#include "orte/mca/pls/pls.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/iof/iof.h"
#include "orte/mca/smr/smr.h"

#include "orte/mca/rmgr/base/rmgr_private.h"
#include "orte/mca/rmgr/proxy/rmgr_proxy.h"


static int orte_rmgr_proxy_setup_job(orte_app_context_t** app_context,
                                     orte_std_cntr_t num_context,
                                     orte_jobid_t* jobid,
                                     opal_list_t *attributes);

static int orte_rmgr_proxy_setup_stage_gates(orte_jobid_t jobid);

static int orte_rmgr_proxy_spawn_job(
    orte_app_context_t** app_context,
    orte_std_cntr_t num_context,
    orte_jobid_t* jobid,
    orte_std_cntr_t num_connect,
    orte_process_name_t *connect,
    orte_rmgr_cb_fn_t cbfn,
    orte_proc_state_t cb_conditions,
    opal_list_t *attributes);

orte_rmgr_base_module_t orte_rmgr_proxy_module = {
    NULL, /* don't need special init */
    orte_rmgr_proxy_setup_job,
    orte_rmgr_proxy_spawn_job,
    orte_rmgr_base_connect,
    orte_rmgr_base_disconnect,
    NULL, /* finalize */
    /**   SUPPORT FUNCTIONS   ***/
    orte_rmgr_base_find_attribute,
    orte_rmgr_base_add_attribute,
    orte_rmgr_base_merge_attributes,
    orte_rmgr_base_delete_attribute,
    orte_rmgr_base_get_app_context,
    orte_rmgr_base_put_app_context,
    orte_rmgr_base_check_context_cwd,
    orte_rmgr_base_check_context_app,
    orte_rmgr_base_set_vpid_range,
    orte_rmgr_base_get_vpid_range
};


/*
 *  Setup the job segment and initialize the application context. Could
 *  do this in the proxy - but allowing the HNP to do this moves the registry
 *  and name service actions to the HNP for efficiency.
 */

static int orte_rmgr_proxy_setup_job(orte_app_context_t** app_context,
                                     orte_std_cntr_t num_context,
                                     orte_jobid_t* jobid,
                                     opal_list_t *attrs)
{
    orte_buffer_t cmd;
    orte_buffer_t rsp;
    orte_std_cntr_t count;
    orte_rmgr_cmd_t command=ORTE_RMGR_SETUP_JOB_CMD;
    int rc;

    OPAL_TRACE(1);

    /* construct command */
    OBJ_CONSTRUCT(&cmd, orte_buffer_t);

    /* pack the command */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, &command, 1, ORTE_RMGR_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&cmd);
        return rc;
    }
    
    /* pack the number of app_contexts */
    if(ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, &num_context, 1, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&cmd);
        return rc;
    }
    
    /* and pack them */
    if(ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, app_context, num_context, ORTE_APP_CONTEXT))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&cmd);
        return rc;
    }

    /* pack the attributes */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, attrs, 1, ORTE_ATTR_LIST))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&cmd);
        return rc;
    }
    
    /* send the command */
    if(0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_HNP, &cmd, ORTE_RML_TAG_RMGR, 0))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&cmd);
        return rc;
    }
    OBJ_DESTRUCT(&cmd);

    /* wait for response */
    OBJ_CONSTRUCT(&rsp, orte_buffer_t);
    if(0 > (rc = orte_rml.recv_buffer(ORTE_PROC_MY_HNP, &rsp, ORTE_RML_TAG_RMGR))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&rsp);
        return rc;
    }

    /* get the returned command */
    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(&rsp, &command, &count, ORTE_RMGR_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&rsp);
        return rc;
    }
    /* and check it to ensure valid comm */
    if (ORTE_RMGR_SETUP_JOB_CMD != command) {
        OBJ_DESTRUCT(&rsp);
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        return ORTE_ERR_COMM_FAILURE;
    }
    
    /* get the jobid */
    count = 1;
    if(ORTE_SUCCESS != (rc = orte_dss.unpack(&rsp, jobid, &count, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
    }
    
    OBJ_DESTRUCT(&rsp);
    return rc;
}

static int orte_rmgr_proxy_setup_stage_gates(orte_jobid_t jobid)
{
    orte_buffer_t cmd;
    orte_buffer_t rsp;
    orte_std_cntr_t count;
    orte_rmgr_cmd_t command=ORTE_RMGR_SETUP_GATES_CMD;
    int rc;

    OPAL_TRACE(1);

    /* construct command */
    OBJ_CONSTRUCT(&cmd, orte_buffer_t);

    /* pack the command */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, &command, 1, ORTE_RMGR_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&cmd);
        return rc;
    }

    /* pack the jobid */
    if(ORTE_SUCCESS != (rc = orte_dss.pack(&cmd, &jobid, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&cmd);
        return rc;
    }

    /* send the command */
    if(0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_HNP, &cmd, ORTE_RML_TAG_RMGR, 0))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&cmd);
        return rc;
    }
    OBJ_DESTRUCT(&cmd);

    /* wait for response */
    OBJ_CONSTRUCT(&rsp, orte_buffer_t);
    if(0 > (rc = orte_rml.recv_buffer(ORTE_PROC_MY_HNP, &rsp, ORTE_RML_TAG_RMGR))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&rsp);
        return rc;
    }

    /* get the returned command */
    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(&rsp, &command, &count, ORTE_RMGR_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&rsp);
        return rc;
    }
    /* and check it to ensure valid comm */
    if (ORTE_RMGR_SETUP_GATES_CMD != command) {
        OBJ_DESTRUCT(&rsp);
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        return ORTE_ERR_COMM_FAILURE;
    }

    OBJ_DESTRUCT(&rsp);
    return rc;
}


static void orte_rmgr_proxy_wireup_stdin(orte_jobid_t jobid)
{
    int rc;
    orte_process_name_t* name;

    OPAL_TRACE(1);

    if (ORTE_SUCCESS != (rc = orte_ns.create_process_name(&name, 0, jobid, 0))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    if (ORTE_SUCCESS != (rc = orte_iof.iof_push(name, ORTE_NS_CMP_JOBID, ORTE_IOF_STDIN, 0))) {
        ORTE_ERROR_LOG(rc);
    }
}


static void orte_rmgr_proxy_callback(orte_gpr_notify_data_t *data, void *cbdata)
{
    orte_rmgr_cb_fn_t cbfunc;
    union {
        orte_rmgr_cb_fn_t func;
        void * ptr;
    } cbfunc_union;
    orte_gpr_value_t **values, *value;
    orte_gpr_keyval_t** keyvals;
    orte_jobid_t jobid;
    orte_std_cntr_t i, j, k;
    int rc;

    OPAL_TRACE(1);

    /* ISO C forbids conversion of object pointer to function
       pointer.  So we do this, which is the same thing, but without
       the warning from GCC */
    cbfunc_union.ptr = cbdata;
    cbfunc = cbfunc_union.func;

    /* we made sure in the subscriptions that at least one
     * value is always returned
     * get the jobid from the segment name in the first value
     */
    values = (orte_gpr_value_t**)(data->values)->addr;
    if (ORTE_SUCCESS != (rc =
            orte_schema.extract_jobid_from_segment_name(&jobid,
                        values[0]->segment))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    for(i = 0, k=0; k < data->cnt &&
                    i < (data->values)->size; i++) {
        if (NULL != values[i]) {
            k++;
            value = values[i];
            /* determine the state change */
            keyvals = value->keyvals;
            for(j=0; j<value->cnt; j++) {
                orte_gpr_keyval_t* keyval = keyvals[j];
                if(strcmp(keyval->key, ORTE_PROC_NUM_AT_INIT) == 0) {
                    (*cbfunc)(jobid,ORTE_PROC_STATE_INIT);
                    continue;
                }
                if(strcmp(keyval->key, ORTE_PROC_NUM_LAUNCHED) == 0) {
                    (*cbfunc)(jobid,ORTE_PROC_STATE_LAUNCHED);
                    continue;
                }
                if(strcmp(keyval->key, ORTE_PROC_NUM_RUNNING) == 0) {
                    (*cbfunc)(jobid,ORTE_PROC_STATE_RUNNING);
                    continue;
                }
                if(strcmp(keyval->key, ORTE_PROC_NUM_AT_STG1) == 0) {
                    (*cbfunc)(jobid,ORTE_PROC_STATE_AT_STG1);
                    continue;
                }
                if(strcmp(keyval->key, ORTE_PROC_NUM_AT_STG2) == 0) {
                    (*cbfunc)(jobid,ORTE_PROC_STATE_AT_STG2);
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
            }
        }
    }
}

/**
 * define a callback point for completing the wireup of the stdin for io forwarding
 */
static void orte_rmgr_proxy_wireup_callback(orte_gpr_notify_data_t *data, void *cbdata)
{
    orte_gpr_value_t **values;
    orte_jobid_t jobid;
    int rc;

    OPAL_TRACE(1);

    /* we made sure in the subscriptions that at least one
    * value is always returned
    * get the jobid from the segment name in the first value
    */
    values = (orte_gpr_value_t**)(data->values)->addr;
    if (ORTE_SUCCESS != (rc = orte_schema.extract_jobid_from_segment_name(&jobid, values[0]->segment))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    orte_rmgr_proxy_wireup_stdin(jobid);
}

/*
 *  Shortcut for the multiple steps involved in spawning a new job.
 */


static int orte_rmgr_proxy_spawn_job(
    orte_app_context_t** app_context,
    orte_std_cntr_t num_context,
    orte_jobid_t* jobid,
    orte_std_cntr_t num_connect,
    orte_process_name_t *connect,
    orte_rmgr_cb_fn_t cbfunc,
    orte_proc_state_t cb_conditions,
    opal_list_t *attributes)
{
    int rc;
    orte_process_name_t name = {0, ORTE_JOBID_INVALID, 0};
    orte_attribute_t *flow;
    uint8_t flags, *fptr;

    OPAL_TRACE(1);
    
    /* check for any flow directives to control what we do */
    if (NULL != (flow = orte_rmgr.find_attribute(attributes, ORTE_RMGR_SPAWN_FLOW))) {
        /* something was specified - get the value */
        if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&fptr, flow->value, ORTE_UINT8))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        flags = *fptr;
    } else {
        flags = 0xff;
    }
    
    /*
     * Setup job and allocate resources
     */
    if (flags & ORTE_RMGR_SETUP) {
        if (ORTE_SUCCESS !=
            (rc = orte_rmgr_proxy_setup_job(app_context, num_context, jobid, attributes))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
    if (flags & ORTE_RMGR_RES_DISC) {
        if (ORTE_SUCCESS != (rc = orte_rds.query(*jobid))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
    if (flags & ORTE_RMGR_ALLOC) {
        if (ORTE_SUCCESS != (rc = orte_ras.allocate_job(*jobid, attributes))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
    if (flags & ORTE_RMGR_MAP) {    
        if (ORTE_SUCCESS != (rc = orte_rmaps.map_job(*jobid, attributes))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
    if (flags & ORTE_RMGR_SETUP_TRIGS) {
        /*
         * setup I/O forwarding
         */
        
        name.jobid = *jobid;
        
        if (ORTE_SUCCESS != (rc = orte_iof.iof_pull(&name, ORTE_NS_CMP_JOBID, ORTE_IOF_STDOUT, 1))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        if (ORTE_SUCCESS != (rc = orte_iof.iof_pull(&name, ORTE_NS_CMP_JOBID, ORTE_IOF_STDERR, 2))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* setup the launch system's stage gate counters and subscriptions */
        if (ORTE_SUCCESS != (rc = orte_rmgr_proxy_setup_stage_gates(*jobid))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /** setup the subscription so we can complete the wireup when all processes reach LAUNCHED */
        rc = orte_smr.job_stage_gate_subscribe(*jobid, orte_rmgr_proxy_wireup_callback, NULL, ORTE_PROC_STATE_LAUNCHED);
        if(ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /*
         * Define the ERRMGR's callbacks as required
         */
        if (ORTE_SUCCESS != (rc = orte_errmgr.register_job(*jobid))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /*
         * setup callback
         */

        if(NULL != cbfunc) {
            union {
                orte_rmgr_cb_fn_t func;
                void * ptr;
            } cbfunc_union;
            void *cbdata;

            /* ISO C forbids conversion of object pointer to function
               pointer.  So we do this, which is the same thing, but without
               the warning from GCC */
            cbfunc_union.func = cbfunc;
            cbdata = cbfunc_union.ptr;

            rc = orte_smr.job_stage_gate_subscribe(*jobid, orte_rmgr_proxy_callback, cbdata, cb_conditions);
            if(ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }

    }
    
    /* if we don't want to launch, then just return here */
    if (!(flags & ORTE_RMGR_LAUNCH)) {
        return ORTE_SUCCESS;
    }
        
    /*
     * launch the job
     */
    if (ORTE_SUCCESS != (rc = orte_pls.launch_job(*jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;
}



