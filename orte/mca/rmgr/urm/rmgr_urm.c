/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
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
#include "orte_config.h"
#include "orte/orte_constants.h"

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */
#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */

#include "opal/class/opal_list.h"
#include "opal/util/trace.h"
#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/opal_environ.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rds/rds.h"
#include "orte/mca/ras/ras.h"
#include "orte/mca/rmaps/rmaps.h"
#include "orte/mca/pls/pls.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/iof/iof.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/smr/smr.h"

#include "orte/mca/rmgr/base/rmgr_private.h"
#include "orte/mca/rmgr/base/base.h"
#include "orte/mca/rmgr/urm/rmgr_urm.h"


static int orte_rmgr_urm_setup_job(
    orte_app_context_t** app_context,
    orte_std_cntr_t num_context,
    orte_jobid_t* jobid,
    opal_list_t *attrs);

static int orte_rmgr_urm_spawn_job(
    orte_app_context_t** app_context,
    orte_std_cntr_t num_context,
    orte_jobid_t* jobid,
    orte_std_cntr_t num_connect,
    orte_process_name_t *connect,
    orte_rmgr_cb_fn_t cbfn,
    orte_proc_state_t cb_conditions,
    opal_list_t *attributes);

static int orte_rmgr_urm_module_init(void);

static int orte_rmgr_urm_module_finalize(void);


orte_rmgr_base_module_t orte_rmgr_urm_module = {
    orte_rmgr_urm_module_init,
    orte_rmgr_urm_setup_job,
    orte_rmgr_urm_spawn_job,
    orte_rmgr_base_connect,
    orte_rmgr_base_disconnect,
    orte_rmgr_urm_module_finalize,
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
 * Since we were selected, complete the init
 * by starting the comm system
 */
static int orte_rmgr_urm_module_init(void)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_rmgr_base_comm_start())) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}


/*
 *  Setup the job
 */

static int orte_rmgr_urm_setup_job(orte_app_context_t** app_context,
                                   orte_std_cntr_t num_context,
                                   orte_jobid_t* jobid,
                                   opal_list_t *attrs)
{
    int rc;
    orte_std_cntr_t i;
    orte_attribute_t *attr;
    orte_jobid_t *jptr;
    
    OPAL_TRACE(1);

    /* check for given jobid */
    if (NULL != (attr = orte_rmgr.find_attribute(attrs, ORTE_RMGR_USE_GIVEN_JOBID))) {
        if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&jptr, attr->value, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        *jobid = *jptr;
    } else {
        /* allocate a jobid  */
        if (ORTE_SUCCESS != (rc = orte_ns.create_jobid(jobid, attrs))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }        
    }

    /* for each app_context, we need to purge their environment of HNP
     * MCA component selection directives
     */
    for (i=0; i < num_context; i++) {
        orte_rmgr_base_purge_mca_params(&app_context[i]->env);
    }
    
    /* create and initialize job segment */ /* JJH C/N mapping before this */
    if (ORTE_SUCCESS !=
        (rc = orte_rmgr_base_put_app_context(*jobid, app_context,
                                             num_context))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* set a default job state of INIT. We need this so that
     * orterun doesn't report an error from the registry if
     * the spawn fails for some reason. Otherwise, orterun
     * will try to get the job state (to see why we exited)
     * and will find nothing
     */
    if (ORTE_SUCCESS != (rc = orte_smr.set_job_state(*jobid, ORTE_JOB_STATE_INIT))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;
}


static void orte_rmgr_urm_wireup_stdin(orte_jobid_t jobid)
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
    free(name);

}


static void orte_rmgr_urm_callback(orte_gpr_notify_data_t *data, void *cbdata)
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
static void orte_rmgr_urm_wireup_callback(orte_gpr_notify_data_t *data, void *cbdata)
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
    
    opal_output(orte_rmgr_base.rmgr_output, "rmgr_urm:wireup_callback called for job %ld", (long)jobid);
    
    orte_rmgr_urm_wireup_stdin(jobid);
}

/*
 *  Shortcut for the multiple steps involved in spawning a new job.
 */


static int orte_rmgr_urm_spawn_job(
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
    orte_process_name_t* name;
    struct timeval urmstart, urmstop;
    orte_attribute_t *flow;
    uint8_t flags, *fptr;

    OPAL_TRACE(1);

    /* check for timing request - get start time if so */
    if (mca_rmgr_urm_component.timing) {
        if (0 != gettimeofday(&urmstart, NULL)) {
            opal_output(0, "rmgr_urm: could not obtain start time");
            urmstart.tv_sec = 0;
            urmstart.tv_usec = 0;
        }
    }
    
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
     * Initialize job segment and allocate resources
     */ /* JJH Insert C/N mapping stuff here */
     
    /* Only do this step if we have been asked to do it via the
     * ORTE_RMGR_SPAWN_FLOW attribute
     */
     if (flags & ORTE_RMGR_SETUP) {
             if (ORTE_SUCCESS !=
                 (rc = orte_rmgr_urm_setup_job(app_context, num_context, jobid, attributes))) {
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
        free(name); /* done with this */

        /* setup the launch system's stage gate counters and subscriptions */
        if (ORTE_SUCCESS != (rc = orte_rmgr_base_proc_stage_gate_init(*jobid))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /** setup the subscription so we can complete the wireup when all processes reach LAUNCHED */
        rc = orte_smr.job_stage_gate_subscribe(*jobid, orte_rmgr_urm_wireup_callback, NULL, ORTE_PROC_STATE_LAUNCHED);
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
         * setup caller's callback
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

            rc = orte_smr.job_stage_gate_subscribe(*jobid, orte_rmgr_urm_callback, cbdata, cb_conditions);
            if(ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }

         /* check for timing request - get stop time and report elapsed time if so */
         if (mca_rmgr_urm_component.timing) {
             if (0 != gettimeofday(&urmstop, NULL)) {
                 opal_output(0, "rmgr_urm: could not obtain stop time");
             } else {
                 opal_output(0, "rmgr_urm: job setup time is %ld usec",
                             (long int)((urmstop.tv_sec - urmstart.tv_sec)*1000000 +
                                        (urmstop.tv_usec - urmstart.tv_usec)));
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

     /* check for timing request - get start time if so */
     if (mca_rmgr_urm_component.timing) {
         if (0 != gettimeofday(&urmstart, NULL)) {
             opal_output(0, "rmgr_urm: could not obtain launch stop time");
         } else {
             opal_output(0, "rmgr_urm: launch time is %ld usec",
                         (long int)((urmstart.tv_sec - urmstop.tv_sec)*1000000 +
                                    (urmstart.tv_usec - urmstop.tv_usec)));             
         }
     }
     
     return ORTE_SUCCESS;
}


static int orte_rmgr_urm_module_finalize(void)
{
    int rc;

    /* Cancel pending receive. */
    if (ORTE_SUCCESS != (rc = orte_rmgr_base_comm_stop())) {
        ORTE_ERROR_LOG(rc);
    }

    return rc;
}

