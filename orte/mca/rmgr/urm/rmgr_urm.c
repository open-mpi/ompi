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

#include "opal/util/trace.h"

#include "orte/orte_constants.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rds/base/base.h"
#include "orte/mca/ras/base/base.h"
#include "orte/mca/rmaps/base/base.h"
#include "orte/mca/rmgr/base/base.h"
#include "orte/mca/pls/base/base.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/iof/iof.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/soh/soh.h"

#include "orte/mca/rmgr/urm/rmgr_urm.h"


static int orte_rmgr_urm_query(void);

static int orte_rmgr_urm_create(
    orte_app_context_t** app_context,
    orte_std_cntr_t num_context,
    orte_jobid_t* jobid);

static int orte_rmgr_urm_allocate(
    orte_jobid_t jobid);

static int orte_rmgr_urm_deallocate(
    orte_jobid_t jobid);

static int orte_rmgr_urm_map(
    orte_jobid_t jobid);

static int orte_rmgr_urm_launch(
    orte_jobid_t jobid);

static int orte_rmgr_urm_terminate_job(
    orte_jobid_t jobid);

static int orte_rmgr_urm_terminate_proc(
    const orte_process_name_t* proc_name);

static int orte_rmgr_urm_signal_job(
        orte_jobid_t jobid, int32_t signal);

static int orte_rmgr_urm_signal_proc(
        const orte_process_name_t* proc_name,
        int32_t signal);

static int orte_rmgr_urm_spawn(
    orte_app_context_t** app_context,
    orte_std_cntr_t num_context,
    orte_jobid_t* jobid,
    orte_rmgr_cb_fn_t cbfn,
    orte_proc_state_t cb_conditions);

static int orte_rmgr_urm_finalize(void);


orte_rmgr_base_module_t orte_rmgr_urm_module = {
    orte_rmgr_urm_query,
    orte_rmgr_urm_create,
    orte_rmgr_urm_allocate,
    orte_rmgr_urm_deallocate,
    orte_rmgr_urm_map,
    orte_rmgr_urm_launch,
    orte_rmgr_urm_terminate_job,
    orte_rmgr_urm_terminate_proc,
    orte_rmgr_urm_signal_job,
    orte_rmgr_urm_signal_proc,
    orte_rmgr_urm_spawn,
    orte_rmgr_base_proc_stage_gate_init,
    orte_rmgr_base_proc_stage_gate_mgr,
    orte_rmgr_urm_finalize
};


/*
 * Resource discovery
 */

static int orte_rmgr_urm_query(void)
{
    int rc;

    OPAL_TRACE(1);

    if(ORTE_SUCCESS != (rc = orte_rds_base_query())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    return ORTE_SUCCESS;
}


/*
 *  Create the job segment and initialize the application context.
 */

static int orte_rmgr_urm_create(
    orte_app_context_t** app_context,
    orte_std_cntr_t num_context,
    orte_jobid_t* jobid)
{
    int rc;

    OPAL_TRACE(1);

    /* allocate a jobid  */
    if (ORTE_SUCCESS != (rc = orte_ns.create_jobid(jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* create and initialize job segment */ /* JJH C/N mapping before this */
    if (ORTE_SUCCESS !=
        (rc = orte_rmgr_base_put_app_context(*jobid, app_context,
                                             num_context))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;
}


static int orte_rmgr_urm_allocate(orte_jobid_t jobid)
{
    OPAL_TRACE(1);

    return orte_ras_base_allocate(jobid, &mca_rmgr_urm_component.urm_ras);
}

static int orte_rmgr_urm_deallocate(orte_jobid_t jobid)
{
    OPAL_TRACE(1);

    return mca_rmgr_urm_component.urm_ras->deallocate(jobid);
}

static int orte_rmgr_urm_map(orte_jobid_t jobid)
{
    OPAL_TRACE(1);

    return mca_rmgr_urm_component.urm_rmaps->map(jobid);
}

static int orte_rmgr_urm_launch(orte_jobid_t jobid)
{
    int ret, ret2;

    OPAL_TRACE(1);

    if (ORTE_SUCCESS !=
        (ret = mca_rmgr_urm_component.urm_pls->launch(jobid))) {
        ORTE_ERROR_LOG(ret);
        ret2 = orte_soh.set_job_soh(jobid, ORTE_JOB_STATE_ABORTED);
        if (ORTE_SUCCESS != ret2) {
            ORTE_ERROR_LOG(ret2);
            return ret2;
        }
    }

    return ret;
}

static int orte_rmgr_urm_terminate_job(orte_jobid_t jobid)
{
    int ret;
    orte_jobid_t my_jobid;

    OPAL_TRACE(1);

    ret = orte_ns.get_jobid(&my_jobid, orte_process_info.my_name);
    if (ORTE_SUCCESS == ret) {
        /* if our jobid is the one we're trying to kill AND we're a
           singleton, then calling the urm_pls isn't going to be able
           to do anything.  Just call exit. */
        if (orte_process_info.singleton && jobid == my_jobid) {
            exit(1);
        }
    }

    return mca_rmgr_urm_component.urm_pls->terminate_job(jobid);
}

static int orte_rmgr_urm_terminate_proc(const orte_process_name_t* proc_name)
{
    OPAL_TRACE(1);

    if ((0 == orte_ns.compare(ORTE_NS_CMP_ALL, proc_name,
                              orte_process_info.my_name)) &&
        (orte_process_info.singleton)) {
        /* if we're trying to get ourselves killed and we're a
           singleton, calling terminate_proc isn't going to work
           properly -- there's no pls setup properly for us.  Just
           call exit and be done. */
        exit(1);
    }

    return mca_rmgr_urm_component.urm_pls->terminate_proc(proc_name);
}


static int orte_rmgr_urm_signal_job(orte_jobid_t jobid, int32_t signal)
{
    int ret;
    orte_jobid_t my_jobid;

    OPAL_TRACE(1);

    ret = orte_ns.get_jobid(&my_jobid, orte_process_info.my_name);
    if (ORTE_SUCCESS == ret) {
        /** if our jobid is the one we're trying to signal AND we're a
         * singleton, then calling the urm_pls isn't going to be able
         * to do anything - we already have the signal! */
        if (orte_process_info.singleton && jobid == my_jobid) {
            return ORTE_SUCCESS;
        }
    }

    return mca_rmgr_urm_component.urm_pls->signal_job(jobid, signal);
}

static int orte_rmgr_urm_signal_proc(const orte_process_name_t* proc_name, int32_t signal)
{
    OPAL_TRACE(1);

    if ((0 == orte_ns.compare(ORTE_NS_CMP_ALL, proc_name,
         orte_process_info.my_name)) &&
         (orte_process_info.singleton)) {
        /** if we're trying to signal ourselves and we're a
         * singleton, calling signal_proc isn't going to work
         * properly -- there's no pls setup properly for us. Besides, we
         * already have the signal!
         */
        return ORTE_SUCCESS;
    }

    return mca_rmgr_urm_component.urm_pls->signal_proc(proc_name, signal);
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

    /* stupid ISO C forbids conversion of object pointer to function
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
                if(strcmp(keyval->key, ORTE_PROC_NUM_ABORTED) == 0) {
                    (*cbfunc)(jobid,ORTE_PROC_STATE_ABORTED);
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
    orte_rmgr_urm_wireup_stdin(jobid);
}

/*
 *  Shortcut for the multiple steps involved in spawning a new job.
 */


static int orte_rmgr_urm_spawn(
    orte_app_context_t** app_context,
    orte_std_cntr_t num_context,
    orte_jobid_t* jobid,
    orte_rmgr_cb_fn_t cbfunc,
    orte_proc_state_t cb_conditions)
{
    int rc;
    orte_process_name_t* name;

    OPAL_TRACE(1);

    /*
     * Perform resource discovery.
     */
    if (mca_rmgr_urm_component.urm_rds == false &&
        ORTE_SUCCESS != (rc = orte_rds_base_query())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    } else {
        mca_rmgr_urm_component.urm_rds = true;
    }

    /*
     * Initialize job segment and allocate resources
     */ /* JJH Insert C/N mapping stuff here */
    if (ORTE_SUCCESS !=
        (rc = orte_rmgr_urm_create(app_context,num_context,jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_rmgr_urm_allocate(*jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_rmgr_urm_map(*jobid))) {
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
  
    /* setup the launch system's stage gate counters and subscriptions */
    if (ORTE_SUCCESS != (rc = orte_rmgr_base_proc_stage_gate_init(*jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /** setup the subscription so we can complete the wireup when all processes reach LAUNCHED */
    rc = orte_rmgr_base_proc_stage_gate_subscribe(*jobid, orte_rmgr_urm_wireup_callback, NULL, ORTE_PROC_STATE_LAUNCHED);
    if(ORTE_SUCCESS != rc) {
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

        /* stupid ISO C forbids conversion of object pointer to function
           pointer.  So we do this, which is the same thing, but without
           the warning from GCC */
        cbfunc_union.func = cbfunc;
        cbdata = cbfunc_union.ptr;

        rc = orte_rmgr_base_proc_stage_gate_subscribe(*jobid, orte_rmgr_urm_callback, cbdata, cb_conditions);
        if(ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    /*
     * launch the job
     */
    if (ORTE_SUCCESS != (rc = orte_rmgr_urm_launch(*jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    orte_ns.free_name(&name);
    return ORTE_SUCCESS;
}


static int orte_rmgr_urm_finalize(void)
{
    int rc;

    OPAL_TRACE(1);

    /**
     * Finalize Process Launch Subsystem (PLS)
     */
    if (ORTE_SUCCESS != (rc = orte_pls_base_finalize())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /**
     * Finalize Resource Mapping Subsystem (RMAPS)
     */
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_finalize())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /**
     * Finalize Resource Allocation Subsystem (RAS)
     */
    if (ORTE_SUCCESS != (rc = orte_ras_base_finalize())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /**
     * Finalize Resource Discovery Subsystem (RDS)
     */
    if (ORTE_SUCCESS != (rc = orte_rds_base_finalize())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* Cancel pending receive. */

    orte_rml.recv_cancel(ORTE_RML_NAME_ANY, ORTE_RML_TAG_RMGR_SVC);

    return ORTE_SUCCESS;
}

