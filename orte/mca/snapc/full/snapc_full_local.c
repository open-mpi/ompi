/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University.
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
 */

#include "orte_config.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */

#include "opal/runtime/opal_progress.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/util/argv.h"
#include "opal/util/opal_environ.h"
#include "opal/util/os_dirpath.h"
#include "opal/util/basename.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

#include "orte/orte_constants.h"
#include "orte/runtime/orte_wait.h"
#include "orte/util/univ_info.h"
#include "orte/util/sys_info.h"
#include "orte/util/proc_info.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/pls/pls.h"
#include "orte/mca/rmgr/rmgr.h"
#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"

#include "snapc_full.h"

/************************************
 * Locally Global vars & functions :)
 ************************************/
static int snapc_full_local_reg_vpid_state_updates(void);
static int snapc_full_local_reg_job_state_updates( void);

static void snapc_full_local_vpid_state_callback(orte_gpr_notify_data_t *data, void *cbdata);
static void snapc_full_local_job_state_callback( orte_gpr_notify_data_t *data, void *cbdata);

static int snapc_full_local_get_vpids(void);
static int snapc_full_local_get_updated_vpids(void);

static int snapc_full_local_setup_snapshot_dir(char * snapshot_ref, char * sugg_dir, char **actual_dir);

static int snapc_full_local_start_checkpoint(orte_snapc_base_snapshot_t *vpid_snapshot, bool term);

static bool local_checkpoint_finished(void);

static void snapc_full_local_wait_ckpt_cb(pid_t pid, int status, void* cbdata);

static opal_list_t snapc_local_vpids;
static orte_jobid_t snapc_local_jobid;

/************************
 * Function Definitions
 ************************/
int local_coord_init( void )
{
    snapc_local_jobid = -1;

    return ORTE_SUCCESS;
}

int local_coord_finalize( void )
{
    if( snapc_local_jobid >= 0 ) {
        return local_coord_release_job(snapc_local_jobid);
    }

    return ORTE_SUCCESS;
}

int local_coord_setup_job(orte_jobid_t jobid)
{
    int ret, exit_status = ORTE_SUCCESS;

    /*
     * Set the jobid that we are responsible for
     */
    snapc_local_jobid = jobid;
    opal_output_verbose(10, mca_snapc_full_component.super.output_handle,
                        "local) Monitor local jobid (%d)\n", snapc_local_jobid);

    /*
     * Get the list of vpid's that we care about
     */
    if( ORTE_SUCCESS != (ret = snapc_full_local_get_vpids()) ) {
        exit_status = ret;
        goto cleanup;
    }
    
    /*
     * Setup GPR Callbacks triggered by Global Snapshot Coordinator
     * These include:
     *  - Upon request for a checkpoint to be taken
     *    Received by the Local Snapshot Coordinator, then relayed to the 
     *    Application Snapshot Coordinators
     *  - Upon request for the local snapshots to be transfered to the 
     *    Global Snapshot Coordinator.
     */
    if( ORTE_SUCCESS != (ret = snapc_full_local_reg_job_state_updates( ) ) ) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Setup GPR Callbacks triggered by the Application Snapshot Coordiantor
     * This indicates the completion of a checkpoint, and it's availablity
     * to be reaped by the Local Snapshot Coordinator
     */
    if( ORTE_SUCCESS != (ret = snapc_full_local_reg_vpid_state_updates( ) ) ) {
        exit_status = ret;
        goto cleanup;
    }

    ret = exit_status;

 cleanup:
    return exit_status;
}

int local_coord_release_job(orte_jobid_t jobid)
{
    int ret, exit_status = ORTE_SUCCESS;
    opal_list_item_t* item = NULL;
    bool is_done = true;

    /*
     * Wait around until we hear back from the checkpoint requests that
     * we have outstanding.
     */
    do {
        is_done = true;

        for(item  = opal_list_get_first(&snapc_local_vpids);
            item != opal_list_get_end(&snapc_local_vpids);
            item  = opal_list_get_next(item) ) {
            orte_snapc_base_snapshot_t *vpid_snapshot;
            vpid_snapshot = (orte_snapc_base_snapshot_t*)item;
            
            if(ORTE_SNAPC_CKPT_STATE_NONE     != vpid_snapshot->state &&
               ORTE_SNAPC_CKPT_STATE_ERROR    != vpid_snapshot->state &&
               ORTE_SNAPC_CKPT_STATE_FINISHED != vpid_snapshot->state ) {
                is_done = false;
                break;
            }
        }
        if( !is_done )
            opal_progress();
    } while(!is_done);

    /*
     * Cleanup all GPR registered callbacks
     */

    OBJ_DESTRUCT(&snapc_local_vpids);

    ret = exit_status;

    return exit_status;
}

/******************
 * Local functions
 ******************/
static int snapc_full_local_reg_vpid_state_updates(void)
{
    int ret, exit_status = ORTE_SUCCESS;
    char *segment = NULL, *trig_name = NULL;
    orte_gpr_subscription_id_t id;
    char* keys[] = {
        ORTE_PROC_CKPT_STATE_KEY,
        ORTE_PROC_CKPT_SNAPSHOT_REF_KEY,
        ORTE_PROC_CKPT_SNAPSHOT_LOC_KEY,
        NULL
    };
    char* trig_names[] = {
        ORTE_PROC_CKPT_STATE_TRIGGER,
        NULL
    };
    opal_list_item_t* item = NULL;

    /*
     * Identify the segment for this job
     */
    if( ORTE_SUCCESS != (ret = orte_schema.get_job_segment_name(&segment, snapc_local_jobid))) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Attach to the standard trigger
     */
    if( ORTE_SUCCESS != (ret = orte_schema.get_std_trigger_name(&trig_name, trig_names[0], snapc_local_jobid))) {
        exit_status = ret;
        goto cleanup;
    }
    
    /*
     * For each process that we are tasked with:
     */
    for(item  = opal_list_get_first(&snapc_local_vpids);
        item != opal_list_get_end(&snapc_local_vpids);
        item  = opal_list_get_next(item) ) {
        orte_snapc_base_snapshot_t *vpid_snapshot;
        char **tokens;
        orte_std_cntr_t num_tokens;

        vpid_snapshot = (orte_snapc_base_snapshot_t*)item;

        /*
         * Setup the tokens
         */
        if (ORTE_SUCCESS != (ret = orte_schema.get_proc_tokens(&tokens,
                                                               &num_tokens,
                                                               &vpid_snapshot->process_name) )) {
            exit_status = ret;
            goto cleanup;
        }
        
        /*
         * Subscribe to the GPR
         */
        if( ORTE_SUCCESS != (ret = orte_gpr.subscribe_N(&id,
                                                        trig_name,
                                                        NULL,
                                                        ORTE_GPR_NOTIFY_VALUE_CHG,
                                                        ORTE_GPR_TOKENS_OR | ORTE_GPR_KEYS_OR,
                                                        segment,
                                                        tokens,
                                                        3,
                                                        keys,
                                                        snapc_full_local_vpid_state_callback,
                                                        NULL))) {
            exit_status = ret;
            goto cleanup;
        }
    }

 cleanup:
    if(NULL != segment)
        free(segment);

    if(NULL != trig_name)
        free(trig_name);

    return exit_status;
}

static int snapc_full_local_reg_job_state_updates( void )
{
    int ret, exit_status = ORTE_SUCCESS;
    char *segment = NULL, *trig_name = NULL, **tokens;
    orte_gpr_subscription_id_t id;
    char* keys[] = {
        ORTE_JOB_CKPT_STATE_KEY,
        ORTE_JOB_CKPT_SNAPSHOT_REF_KEY,
        ORTE_JOB_CKPT_SNAPSHOT_LOC_KEY,
        NULL
    };
    char* trig_names[] = {
        ORTE_JOB_CKPT_STATE_TRIGGER,
        NULL
    };
    orte_std_cntr_t num_tokens;
    
    /*
     * Identify the segment for this job
     */
    if( ORTE_SUCCESS != (ret = orte_schema.get_job_segment_name(&segment, snapc_local_jobid))) {
        exit_status = ret;
        goto cleanup;
    }
    
    /*
     * Setup the tokens
     */
    if (ORTE_SUCCESS != (ret = orte_schema.get_job_tokens(&tokens,
                                                          &num_tokens,
                                                          snapc_local_jobid) )) {
        exit_status = ret;
        goto cleanup;
    }
    tokens[0] = strdup(ORTE_JOB_GLOBALS);
    tokens[1] = NULL;

    /*
     * Attach to the standard trigger
     */
    if( ORTE_SUCCESS != (ret = orte_schema.get_std_trigger_name(&trig_name, trig_names[0], snapc_local_jobid)) ) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Subscribe to the GPR
     */
    if( ORTE_SUCCESS != (ret = orte_gpr.subscribe_N(&id,
                                                    trig_name,
                                                    NULL,
                                                    ORTE_GPR_NOTIFY_VALUE_CHG,
                                                    ORTE_GPR_TOKENS_OR | ORTE_GPR_KEYS_OR,
                                                    segment,
                                                    tokens,
                                                    3,
                                                    keys,
                                                    snapc_full_local_job_state_callback,
                                                    NULL))) {
        exit_status = ret;
        goto cleanup;
    }
    
 cleanup:
    if(NULL != segment)
        free(segment);

    if(NULL != trig_name)
        free(trig_name);

    return exit_status;
}

static void snapc_full_local_vpid_state_callback(orte_gpr_notify_data_t *data, void *cbdata)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_process_name_t *proc;
    size_t ckpt_state;
    char *ckpt_ref, *ckpt_loc;
    char * actual_local_dir = NULL;
    opal_list_item_t* item = NULL;
    orte_snapc_base_snapshot_t *vpid_snapshot = NULL;
    char * loc_vpid_name = NULL;
    bool ckpt_n_term = false;

    /*
     * Get the checkpoint information
     */
    if( ORTE_SUCCESS != (ret = orte_snapc_base_extract_gpr_vpid_ckpt_info(data, 
                                                                          &proc,
                                                                          &ckpt_state,
                                                                          &ckpt_ref,
                                                                          &ckpt_loc) ) ) {
        exit_status = ret;
        goto cleanup;
    }

    orte_ns.get_proc_name_string(&loc_vpid_name, proc);

    opal_output_verbose(20, mca_snapc_full_component.super.output_handle,
                        "local) Process (%s): Changed to state to:\n", loc_vpid_name);
    opal_output_verbose(20, mca_snapc_full_component.super.output_handle,
                        "local) State:            %d\n",  (int)ckpt_state);
    opal_output_verbose(20, mca_snapc_full_component.super.output_handle,
                        "local) Snapshot Ref:    (%s)\n", ckpt_ref);
    opal_output_verbose(20, mca_snapc_full_component.super.output_handle,
                        "local) Remote Location: (%s)\n", ckpt_loc);

    /*
     * Find the process in the list
     */
    for(item  = opal_list_get_first(&snapc_local_vpids);
        item != opal_list_get_end(&snapc_local_vpids);
        item  = opal_list_get_next(item) ) {
        vpid_snapshot = (orte_snapc_base_snapshot_t*)item;

        if(0 == orte_ns.compare_fields(ORTE_NS_CMP_ALL, proc, &vpid_snapshot->process_name ) ) {
            /*
             * Update it's local information
             */
            vpid_snapshot->state = ckpt_state;

            if( NULL != vpid_snapshot->crs_snapshot_super.reference_name ) 
                free(vpid_snapshot->crs_snapshot_super.reference_name);
            vpid_snapshot->crs_snapshot_super.reference_name = strdup(ckpt_ref);
            
            if( NULL != vpid_snapshot->crs_snapshot_super.local_location )
                free(vpid_snapshot->crs_snapshot_super.local_location);
            vpid_snapshot->crs_snapshot_super.local_location = strdup(ckpt_loc);
            
            if( NULL != vpid_snapshot->crs_snapshot_super.remote_location )
                free(vpid_snapshot->crs_snapshot_super.remote_location);
            vpid_snapshot->crs_snapshot_super.remote_location = strdup(ckpt_loc);

            break;
        }
    }
    
    /*
     * This process has finished their checkpoint, see if we are done yet
     */
    if( ORTE_SNAPC_CKPT_STATE_FINISHED == ckpt_state ||
        ORTE_SNAPC_CKPT_STATE_ERROR    == ckpt_state ) {
        if(local_checkpoint_finished()) {
            /*
             * Currently we don't need to do anything when done
             */
        }
    }
    /*
     * If we have been asked to checkpoint this process do so
     */
    else if( ORTE_SNAPC_CKPT_STATE_PENDING      == ckpt_state ||
             ORTE_SNAPC_CKPT_STATE_PENDING_TERM == ckpt_state) {
        
        if(ORTE_SNAPC_CKPT_STATE_PENDING_TERM == ckpt_state ) {
            ckpt_n_term = true;
        }

        /*
         * Set up the snapshot directory per suggestion from 
         * the Global Snapshot Coordinator
         * If we can't create the suggested local directory, do what we can and update
         * local directory reference in the GPR
         */
        if( ORTE_SUCCESS != (ret = snapc_full_local_setup_snapshot_dir(vpid_snapshot->crs_snapshot_super.reference_name,
                                                                       vpid_snapshot->crs_snapshot_super.local_location,
                                                                       &actual_local_dir) ) ) {
            exit_status = ret;
            goto cleanup;
        }
        
        if( NULL != vpid_snapshot->crs_snapshot_super.local_location )
            free(vpid_snapshot->crs_snapshot_super.local_location);
        vpid_snapshot->crs_snapshot_super.local_location = strdup(ckpt_loc);
        
        opal_output_verbose(20, mca_snapc_full_component.super.output_handle,
                            "local) Using directory (%s)\n",vpid_snapshot->crs_snapshot_super.local_location);
        
        /*
         * Update so that folks know that we are working on it
         */
        if( ORTE_SUCCESS != (ret = orte_snapc_base_set_vpid_ckpt_info( vpid_snapshot->process_name,
                                                                       ORTE_SNAPC_CKPT_STATE_RUNNING,
                                                                       vpid_snapshot->crs_snapshot_super.reference_name,
                                                                       vpid_snapshot->crs_snapshot_super.local_location ) ) ) {
            exit_status = ret;
            goto cleanup;
        }
        
        if( ORTE_SUCCESS != (ret = snapc_full_local_start_checkpoint(vpid_snapshot, ckpt_n_term) ) ) {
            exit_status = ret;
            goto cleanup;
        }
        
    }
    
 cleanup:
    if( NULL != actual_local_dir)
        free(actual_local_dir);
    if( NULL != loc_vpid_name)
        free(loc_vpid_name);

    return;
}

static void snapc_full_local_job_state_callback( orte_gpr_notify_data_t *data, void *cbdata )
{
    int ret, exit_status = ORTE_SUCCESS;
    size_t ckpt_state;
    char *ckpt_ref = NULL;
    opal_list_item_t* item = NULL;
    char *global_ckpt_dir = NULL;

    /*
     * Get jobid from the segment name in the first value
     */
    if( ORTE_SUCCESS != (ret = orte_snapc_base_extract_gpr_job_ckpt_info(data, 
                                                                         &ckpt_state,
                                                                         &ckpt_ref,
                                                                         &global_ckpt_dir) ) ) {
        exit_status = ret;
        goto cleanup;
    }

    opal_output_verbose(20, mca_snapc_full_component.super.output_handle,
                        "local) State:            %d\n",  (int)ckpt_state);
    opal_output_verbose(20, mca_snapc_full_component.super.output_handle,
                        "local) Snapshot Ref:    (%s)\n", ckpt_ref);
    opal_output_verbose(20, mca_snapc_full_component.super.output_handle,
                        "local) Remote Location: (%s)\n", global_ckpt_dir);

    /*
     * Update the vpid structure if we need to.
     * Really only need to if we don't have valid information (PID) 
     * for the application.
     */
    if( ORTE_SUCCESS != (ret = snapc_full_local_get_updated_vpids() ) ) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * If we have been asked to checkpoint do so
     */
    if( ORTE_SNAPC_CKPT_STATE_PENDING      == ckpt_state ||
        ORTE_SNAPC_CKPT_STATE_PENDING_TERM == ckpt_state ) {
        /*
         * For each of the processes we are tasked with, start their checkpoints
         */
        for(item  = opal_list_get_first(&snapc_local_vpids);
            item != opal_list_get_end(&snapc_local_vpids);
            item  = opal_list_get_next(item) ) {
            orte_snapc_base_snapshot_t *vpid_snapshot;
            vpid_snapshot = (orte_snapc_base_snapshot_t*)item;

            if( ORTE_SNAPC_CKPT_STATE_PENDING_TERM == ckpt_state ) {
                vpid_snapshot->term = true;
            }
            else {
                vpid_snapshot->term = false;
            }

            /*
             * Update it's local information
             */
            if( NULL != vpid_snapshot->crs_snapshot_super.reference_name ) 
                free(vpid_snapshot->crs_snapshot_super.reference_name);
            vpid_snapshot->crs_snapshot_super.reference_name = opal_crs_base_unique_snapshot_name(vpid_snapshot->process_name.vpid);
            
            /* global_directory/local_snapshot_vpid/... */
            if( NULL != vpid_snapshot->crs_snapshot_super.local_location )
                free(vpid_snapshot->crs_snapshot_super.local_location);
            if( orte_snapc_base_store_in_place ) {
                asprintf(&(vpid_snapshot->crs_snapshot_super.local_location), 
                         "%s/%s", 
                         global_ckpt_dir,
                         vpid_snapshot->crs_snapshot_super.reference_name);
            }
            else {
                /* Use the OPAL CRS base snapshot dir
                 * JJH: Do we want to do something more interesting?
                 */
                asprintf(&(vpid_snapshot->crs_snapshot_super.local_location), 
                         "%s/%s",
                         opal_crs_base_snapshot_dir,
                         vpid_snapshot->crs_snapshot_super.reference_name);
            }
            
            if( NULL != vpid_snapshot->crs_snapshot_super.remote_location )
                free(vpid_snapshot->crs_snapshot_super.remote_location);

            asprintf(&(vpid_snapshot->crs_snapshot_super.remote_location), 
                     "%s/%s", 
                     global_ckpt_dir,
                     vpid_snapshot->crs_snapshot_super.reference_name);

            /*
             * Update the information in the GPR, then we will pop out in the vpid callback
             */
            if( ORTE_SUCCESS != (ret = orte_snapc_base_set_vpid_ckpt_info( vpid_snapshot->process_name,
                                                                           ckpt_state,
                                                                           vpid_snapshot->crs_snapshot_super.reference_name,
                                                                           vpid_snapshot->crs_snapshot_super.local_location ) ) ) {
                exit_status = ret;
                goto cleanup;
            }
        }
    }
    else if( ORTE_SNAPC_CKPT_STATE_FINISHED == ckpt_state ) {
        /* 
         * If the PLS was able to actually allow for local calls to 
         * orte_pls.terminate_proc then we could terminate the processes
         * from there, but since it is not implemented we need to do 
         * it from the HNP. :/
         */
    }
    
 cleanup:
    if( NULL != global_ckpt_dir)
        free(global_ckpt_dir);

    return;
}

static int snapc_full_local_setup_snapshot_dir(char * snapshot_ref, char * sugg_dir, char **actual_dir)
{
    int ret, exit_status = ORTE_SUCCESS;
    mode_t my_mode = S_IRWXU;

    /* See if we can use the suggested directory */
    if(OPAL_SUCCESS != (ret = opal_os_dirpath_create(sugg_dir, my_mode) ) ) {
        /* Can't use that directory, try the default directory from OPAL CRS */
        *actual_dir = strdup(opal_crs_base_get_snapshot_directory(snapshot_ref));

        if(OPAL_SUCCESS != (ret = opal_os_dirpath_create(*actual_dir, my_mode) ) ) {
            /* Can't use that either, so let's give up */
            exit_status = ret;
            goto cleanup;
        }
    }
    else {
        /* We are able to use that directory */
        *actual_dir = strdup(sugg_dir);
    }

 cleanup:
    return exit_status;
}

static int snapc_full_local_get_vpids(void)
{
    int ret, exit_status = ORTE_SUCCESS;
    char *segment = NULL;
    char *keys[3];
    orte_gpr_value_t** values = NULL;
    orte_std_cntr_t i, k, num_values = 0;

    OBJ_CONSTRUCT(&snapc_local_vpids, opal_list_t);

    /*
     * Get the listing off the GPR
     */
    if(ORTE_SUCCESS != (ret = orte_schema.get_job_segment_name(&segment, snapc_local_jobid))) {
        exit_status = ret;
        return ret;
    }

    keys[0] = ORTE_NODE_NAME_KEY;
    keys[1] = ORTE_PROC_NAME_KEY;
    keys[2] = NULL;

    if(ORTE_SUCCESS != (ret = orte_gpr.get(ORTE_GPR_KEYS_AND|ORTE_GPR_TOKENS_OR,
                                           segment,
                                           NULL,
                                           keys,
                                           &num_values,
                                           &values) ) ) {
        exit_status = ret;
        return ret;
    }
    
    /*
     * Parse the results
     */
    for(i = 0; i < num_values; ++i) {
        orte_gpr_value_t* value = values[i];
        orte_process_name_t *proc_name, *name_ptr;
        orte_snapc_base_snapshot_t *vpid_snapshot;

        for(k = 0; k < value->cnt; ++k) {
            orte_gpr_keyval_t* keyval = value->keyvals[k];
            
            /* If a node, then check if it is our node.
             * If not, then disregard this vpid
             */
            if(0 == strncmp(keyval->key, ORTE_NODE_NAME_KEY, strlen(ORTE_NODE_NAME_KEY)) ) {
                if( ORTE_EQUAL != orte_dss.compare(keyval->value->data, orte_system_info.nodename, ORTE_STRING) ) {
                    goto get_next_value;
                }
            }
            /* Grab the Process Name */
            else if (0 == strncmp(keyval->key, ORTE_PROC_NAME_KEY, strlen(ORTE_PROC_NAME_KEY)) ) {
                if (ORTE_SUCCESS != (ret = orte_dss.get((void**) &name_ptr, keyval->value, ORTE_NAME))) {
                    exit_status = ret;
                    goto cleanup;
                }
                orte_dss.copy((void**)&proc_name, name_ptr, ORTE_NAME);
            }
        }

        vpid_snapshot = OBJ_NEW(orte_snapc_base_snapshot_t);

        /* The pid is not known at this time, we will update it later */
        vpid_snapshot->process_pid = 0;
        vpid_snapshot->process_name.cellid = proc_name->cellid;
        vpid_snapshot->process_name.jobid  = proc_name->jobid;
        vpid_snapshot->process_name.vpid   = proc_name->vpid;

        
        opal_list_append(&snapc_local_vpids, &(vpid_snapshot->crs_snapshot_super.super));
        
    get_next_value:
        ;/* */
    }

 cleanup:
    if( NULL != segment)
        free(segment);

    return exit_status;
}

static int snapc_full_local_get_updated_vpids(void)
{
    int ret, exit_status = ORTE_SUCCESS;
    char *segment = NULL;
    char *keys[4];
    orte_gpr_value_t** values = NULL;
    orte_std_cntr_t i, k, num_values = 0;
    opal_list_item_t* item = NULL;
    orte_snapc_base_snapshot_t *vpid_snapshot;

    /*
     * Do we have to update?
     * check one of the vpids
     */
    item  = opal_list_get_first(&snapc_local_vpids);
    if( item == opal_list_get_end(&snapc_local_vpids) ) {
        /* Nothing to see here... No VPID's in structure */
        exit_status = ORTE_SUCCESS;
        goto cleanup;
    }
    /* Check the first element, if it has a pid, then keep going */
    vpid_snapshot = (orte_snapc_base_snapshot_t*)item;
    if( 0 < vpid_snapshot->process_pid ) {
        exit_status = ORTE_SUCCESS;
        goto cleanup;
    }

    /*
     * Get the listing off the GPR
     */
    if(ORTE_SUCCESS != (ret = orte_schema.get_job_segment_name(&segment, snapc_local_jobid))) {
        exit_status = ret;
        return ret;
    }

    keys[0] = ORTE_NODE_NAME_KEY;
    keys[1] = ORTE_PROC_NAME_KEY;
    keys[2] = ORTE_PROC_LOCAL_PID_KEY;
    keys[3] = NULL;

    if(ORTE_SUCCESS != (ret = orte_gpr.get(ORTE_GPR_KEYS_AND|ORTE_GPR_TOKENS_OR,
                                           segment,
                                           NULL,
                                           keys,
                                           &num_values,
                                           &values) ) ) {
        exit_status = ret;
        return ret;
    }
    
    /*
     * Parse the results
     */
    for(i = 0; i < num_values; ++i) {
        orte_gpr_value_t* value = values[i];
        orte_process_name_t *proc_name, *name_ptr;
        pid_t pid = 0, *pid_ptr;

        for(k = 0; k < value->cnt; ++k) {
            orte_gpr_keyval_t* keyval = value->keyvals[k];
            
            /* If a node, then check if it is our node.
             * If not, then disregard this vpid
             */
            if(0 == strncmp(keyval->key, ORTE_NODE_NAME_KEY, strlen(ORTE_NODE_NAME_KEY)) ) {
                if( ORTE_EQUAL != orte_dss.compare(keyval->value->data, orte_system_info.nodename, ORTE_STRING) ) {
                    goto get_next_value;
                }
            }
            /* Grab the Process PID */
            else if (0 == strncmp(keyval->key, ORTE_PROC_LOCAL_PID_KEY, strlen(ORTE_PROC_LOCAL_PID_KEY)) ) {
                if (ORTE_SUCCESS != (ret = orte_dss.get((void**) &pid_ptr, keyval->value, ORTE_PID))) {
                    exit_status = ret;
                    goto cleanup;
                }

                pid = *pid_ptr;
            }
            /* Grab the Process Name */
            else if (0 == strncmp(keyval->key, ORTE_PROC_NAME_KEY, strlen(ORTE_PROC_NAME_KEY)) ) {
                if (ORTE_SUCCESS != (ret = orte_dss.get((void**) &name_ptr, keyval->value, ORTE_NAME))) {
                    exit_status = ret;
                    goto cleanup;
                }
                orte_dss.copy((void**)&proc_name, name_ptr, ORTE_NAME);
            }
        }

        /*
         * Find and update the appropriate vpid
         */
        for(item  = opal_list_get_first(&snapc_local_vpids);
            item != opal_list_get_end(&snapc_local_vpids);
            item  = opal_list_get_next(item) ) {
            vpid_snapshot = (orte_snapc_base_snapshot_t*)item;

            if(0 == orte_ns.compare_fields(ORTE_NS_CMP_ALL, proc_name, &vpid_snapshot->process_name) ) { 
                vpid_snapshot->process_pid = pid;
                break;
            }
        }
        
    get_next_value:
        ;/* */
    }

 cleanup:
    if( NULL != segment)
        free(segment);

    return exit_status;
}

static int snapc_full_local_start_checkpoint(orte_snapc_base_snapshot_t *vpid_snapshot, bool term)
{
    int ret, exit_status = ORTE_SUCCESS;
    char * command   = NULL;
    char * local_dir = NULL;
    pid_t child_pid;
    
    if( vpid_snapshot->process_pid == 0 ) {
        ret = snapc_full_local_get_updated_vpids();
        if( ORTE_SUCCESS != ret || vpid_snapshot->process_pid == 0 ) {
            opal_output( mca_snapc_full_component.super.output_handle,
                         "local) Cannot checkpoint an invalid pid (%d)\n", 
                         vpid_snapshot->process_pid);
            return ORTE_ERROR;
        }        
    }
    /*
     * Cannot let opal-checkpoint be passed the --term flag
     * since the HNP needs to talk to the app to get
     * information for FileM. HNP will issue the termination.
     * JJH: Eventually release the contraint that the app needs to 
     *      be alive for FileM to properly work.
     *      However if we are storing in place, then we don't use
     *      the FileM framework and can terminate the application
     *      from this command.
     */
    if ( !orte_snapc_base_store_in_place ) {
        term = false;
    }
    
    child_pid = fork();
    if(0 > child_pid) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }
    /* Child */
    else if(0 == child_pid) {
        char **argv = NULL;
        char * term_str = NULL;

        if( term )
            term_str = strdup(" --term ");
        else 
            term_str = strdup("");

        local_dir = strdup(vpid_snapshot->crs_snapshot_super.local_location);
        local_dir = opal_dirname(local_dir);

        asprintf(&command, "opal-checkpoint --where %s --name %s %s %d ", 
                 local_dir,
                 vpid_snapshot->crs_snapshot_super.reference_name,
                 term_str, /* If we are to checkpoint then terminate */
                 vpid_snapshot->process_pid);
        
        if( NULL == (argv = opal_argv_split(command, ' ')) ){
            exit_status = ORTE_ERROR;
            exit(exit_status);
        }
        
        ret = execvp(argv[0], argv);

        exit_status = ret;

        free(term_str);
        exit(exit_status);
    }
    /* Parent */
    else {
        orte_wait_cb(child_pid, snapc_full_local_wait_ckpt_cb, &vpid_snapshot->process_name);
    }

 cleanup:
    if( NULL != command)
        free(command);
    if( NULL != local_dir)
        free(local_dir);
    
    return exit_status;
}

static bool local_checkpoint_finished(void)
{
    opal_list_item_t* item = NULL;
    bool is_done = true;

    for(item  = opal_list_get_first(&snapc_local_vpids);
        item != opal_list_get_end(&snapc_local_vpids);
        item  = opal_list_get_next(item) ) {
        orte_snapc_base_snapshot_t *vpid_snapshot;

        vpid_snapshot = (orte_snapc_base_snapshot_t*)item;
        
        /* Searching for any vpid's that have not completed */
        if(ORTE_SNAPC_CKPT_STATE_FINISHED != vpid_snapshot->state &&
           ORTE_SNAPC_CKPT_STATE_ERROR    != vpid_snapshot->state ) {
            is_done = false;
            break;
        }
    }
    
    return is_done;
}

static void snapc_full_local_wait_ckpt_cb(pid_t pid, int status, void* cbdata)
{
    orte_process_name_t *proc_name = (orte_process_name_t *)cbdata;
    opal_list_item_t* item = NULL;
    orte_snapc_base_snapshot_t *vpid_snapshot = NULL;
    int ret, exit_status = ORTE_SUCCESS;
    size_t loc_state = ORTE_SNAPC_CKPT_STATE_FINISHED;

    if( status == OPAL_SUCCESS ) {
        loc_state = ORTE_SNAPC_CKPT_STATE_FINISHED;
    }
    else {
        loc_state = ORTE_SNAPC_CKPT_STATE_ERROR;
    }

    /*
     * Find the process in the list
     */
    for(item  = opal_list_get_first(&snapc_local_vpids);
        item != opal_list_get_end(&snapc_local_vpids);
        item  = opal_list_get_next(item) ) {
        vpid_snapshot = (orte_snapc_base_snapshot_t*)item;

        if( 0 == orte_ns.compare_fields(ORTE_NS_CMP_ALL, proc_name, &vpid_snapshot->process_name) ) {
            /* Update it's state */
            vpid_snapshot->state = loc_state;
            break;
        }
    }

    /*
     * Now that the checkpoint is finished
     * Update our status information
     */
    if( ORTE_SUCCESS != (ret = orte_snapc_base_set_vpid_ckpt_info( vpid_snapshot->process_name,
                                                                   loc_state,
                                                                   vpid_snapshot->crs_snapshot_super.reference_name,
                                                                   vpid_snapshot->crs_snapshot_super.local_location ) ) ) {
        exit_status = ret;
        goto cleanup;
    }
    
 cleanup:
    return;
}
