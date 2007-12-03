/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Evergrid, Inc. All rights reserved.
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
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif  /* HAVE_FCNTL_H */
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>  /* for mkfifo */
#endif  /* HAVE_SYS_STAT_H */
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#include "opal/runtime/opal_progress.h"
#include "opal/runtime/opal_cr.h"
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
static int snapc_full_local_reg_job_state_updates( void);

static void snapc_full_local_job_state_callback( orte_gpr_notify_data_t *data, void *cbdata);

static int snapc_full_local_get_vpids(void);
static int snapc_full_local_get_updated_vpids(void);
static int snapc_full_local_send_vpids(void);

static int snapc_full_local_setup_snapshot_dir(char * snapshot_ref, char * sugg_dir, char **actual_dir);

static int snapc_full_local_start_checkpoint_all(size_t ckpt_state);
static int snapc_full_local_start_ckpt_open_comm(orte_snapc_full_local_snapshot_t *vpid_snapshot);
static int snapc_full_local_start_ckpt_handshake_term(orte_snapc_full_local_snapshot_t *vpid_snapshot, bool term);
static int snapc_full_local_start_ckpt_handshake(orte_snapc_full_local_snapshot_t *vpid_snapshot);
static int snapc_full_local_end_ckpt_handshake(orte_snapc_full_local_snapshot_t *vpid_snapshot);
static void snapc_full_local_comm_read_event(int fd, short flags, void *arg);

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

    if(orte_snapc_base_establish_gloabl_snapshot_dir) {
        size_t ckpt_state = ORTE_SNAPC_CKPT_STATE_NONE;
        char * ckpt_snapshot_ref = NULL;
        char * ckpt_snapshot_loc = NULL;

        if( ORTE_SUCCESS != (ret = orte_snapc_base_get_job_ckpt_info(jobid,
                                                                     &ckpt_state,
                                                                     &ckpt_snapshot_ref,
                                                                     &ckpt_snapshot_loc) ) ) {
            exit_status = ret;
            goto cleanup;
        }

        if( NULL != ckpt_snapshot_loc &&
            (0 != strncmp(ckpt_snapshot_loc, "", strlen(""))) ) {
            orte_snapc_base_global_snapshot_loc = strdup(ckpt_snapshot_loc);
        }

        opal_output_verbose(10, mca_snapc_full_component.super.output_handle,
                            "local) The global snapshot directory has been established at [%s]\n",
                            orte_snapc_base_global_snapshot_loc);

        if( NULL != ckpt_snapshot_ref ) {
            free(ckpt_snapshot_ref);
            ckpt_snapshot_ref = NULL;
        }
        if( NULL != ckpt_snapshot_loc ) {
            free(ckpt_snapshot_loc);
            ckpt_snapshot_loc = NULL;
        }
    }

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
            orte_snapc_full_local_snapshot_t *vpid_snapshot;
            vpid_snapshot = (orte_snapc_full_local_snapshot_t*)item;
            
            if(ORTE_SNAPC_CKPT_STATE_NONE     != vpid_snapshot->super.state &&
               ORTE_SNAPC_CKPT_STATE_ERROR    != vpid_snapshot->super.state &&
               ORTE_SNAPC_CKPT_STATE_FINISHED != vpid_snapshot->super.state ) {
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

static void snapc_full_local_job_state_callback( orte_gpr_notify_data_t *data, void *cbdata )
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_snapc_full_local_snapshot_t *vpid_snapshot;
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
                        "local) Job State:        %d\n",  (int)ckpt_state);
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
            vpid_snapshot = (orte_snapc_full_local_snapshot_t*)item;

            if( ORTE_SNAPC_CKPT_STATE_PENDING_TERM == ckpt_state ) {
                vpid_snapshot->super.term = true;
            }
            else {
                vpid_snapshot->super.term = false;
            }

            /*
             * Update it's local information
             */
            if( NULL != vpid_snapshot->super.crs_snapshot_super.reference_name ) 
                free(vpid_snapshot->super.crs_snapshot_super.reference_name);
            vpid_snapshot->super.crs_snapshot_super.reference_name = opal_crs_base_unique_snapshot_name(vpid_snapshot->super.process_name.vpid);
            
            /* global_directory/local_snapshot_vpid/... */
            if( NULL != vpid_snapshot->super.crs_snapshot_super.local_location )
                free(vpid_snapshot->super.crs_snapshot_super.local_location);
            if( orte_snapc_base_store_in_place ) {
                asprintf(&(vpid_snapshot->super.crs_snapshot_super.local_location), 
                         "%s/%s", 
                         global_ckpt_dir,
                         vpid_snapshot->super.crs_snapshot_super.reference_name);
            }
            else {
                /* Use the OPAL CRS base snapshot dir
                 * JJH: Do we want to do something more interesting?
                 */
                asprintf(&(vpid_snapshot->super.crs_snapshot_super.local_location), 
                         "%s/%s",
                         opal_crs_base_snapshot_dir,
                         vpid_snapshot->super.crs_snapshot_super.reference_name);
            }
            
            if( NULL != vpid_snapshot->super.crs_snapshot_super.remote_location )
                free(vpid_snapshot->super.crs_snapshot_super.remote_location);

            asprintf(&(vpid_snapshot->super.crs_snapshot_super.remote_location), 
                     "%s/%s", 
                     global_ckpt_dir,
                     vpid_snapshot->super.crs_snapshot_super.reference_name);

            /*
             * Update the information in the GPR, then we will pop out in the vpid callback
             */
            if( ORTE_SUCCESS != (ret = orte_snapc_base_set_vpid_ckpt_info( vpid_snapshot->super.process_name,
                                                                           ckpt_state,
                                                                           vpid_snapshot->super.crs_snapshot_super.reference_name,
                                                                           vpid_snapshot->super.crs_snapshot_super.local_location ) ) ) {
                exit_status = ret;
                goto cleanup;
            }
        }

        /*
         * Start checkpointing all local processes
         */
        if( ORTE_SUCCESS != (ret = snapc_full_local_start_checkpoint_all(ckpt_state) ) ) {
            exit_status = ret;
            goto cleanup;
        }
    }
    else if( ORTE_SNAPC_CKPT_STATE_FINISHED == ckpt_state ) {
        /*
         * Release all checkpointed processes now that the checkpoint is complete
         */
        for(item  = opal_list_get_first(&snapc_local_vpids);
            item != opal_list_get_end(&snapc_local_vpids);
            item  = opal_list_get_next(item) ) {
            vpid_snapshot = (orte_snapc_full_local_snapshot_t*)item;

            opal_output_verbose(15, mca_snapc_full_component.super.output_handle,
                                "local) Job Ckpt finished tell process %s\n",
                                ORTE_NAME_PRINT(&vpid_snapshot->super.process_name));

            if( ORTE_SUCCESS != (ret = snapc_full_local_end_ckpt_handshake(vpid_snapshot) ) ) {
                opal_output(mca_snapc_full_component.super.output_handle,
                            "local) Error: Unable to finish the handshake with peer %s. %d\n", 
                            ORTE_NAME_PRINT(&vpid_snapshot->super.process_name), ret);
                exit_status = OPAL_ERROR;
                goto cleanup;
            }
        }

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

static int snapc_full_local_send_vpids(void)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_process_name_t hnp_name;
    opal_list_item_t* item = NULL;
    orte_buffer_t loc_buffer;
    size_t num_vpids = 0;

    hnp_name.vpid  = 0;
    hnp_name.jobid = 0;

    num_vpids = opal_list_get_size(&snapc_local_vpids);
    if( num_vpids <= 0 ) {
        return ORTE_SUCCESS;
    }

    OBJ_CONSTRUCT(&loc_buffer, orte_buffer_t);

    if (ORTE_SUCCESS != (ret = orte_dss.pack(&loc_buffer, &num_vpids, 1, ORTE_SIZE))) {
        exit_status = ret;
        goto cleanup;
    }

    for(item  = opal_list_get_first(&snapc_local_vpids);
        item != opal_list_get_end(&snapc_local_vpids);
        item  = opal_list_get_next(item) ) {
        orte_snapc_full_local_snapshot_t *vpid_snapshot;
        vpid_snapshot = (orte_snapc_full_local_snapshot_t*)item;

        if (ORTE_SUCCESS != (ret = orte_dss.pack(&loc_buffer, &(vpid_snapshot->super.process_name), 1, ORTE_NAME))) {
            exit_status = ret;
            goto cleanup;
        }
    }

    if (0 > (ret = orte_rml.send_buffer(&hnp_name, &loc_buffer, ORTE_RML_TAG_SNAPC_FULL, 0))) {
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    OBJ_DESTRUCT(&loc_buffer);

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
        orte_snapc_full_local_snapshot_t *vpid_snapshot;

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

        vpid_snapshot = OBJ_NEW(orte_snapc_full_local_snapshot_t);

        /* The pid is not known at this time, we will update it later */
        vpid_snapshot->super.process_pid = 0;
        vpid_snapshot->super.process_name.jobid  = proc_name->jobid;
        vpid_snapshot->super.process_name.vpid   = proc_name->vpid;

        opal_list_append(&snapc_local_vpids, &(vpid_snapshot->super.crs_snapshot_super.super));
        
    get_next_value:
        ;/* */
    }

    /*
     * Send list to global coordinator
     */
    if( ORTE_SUCCESS != (ret = snapc_full_local_send_vpids() ) ) {
        exit_status = ret;
        goto cleanup;
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
    orte_snapc_full_local_snapshot_t *vpid_snapshot;

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
    vpid_snapshot = (orte_snapc_full_local_snapshot_t*)item;
    if( 0 < vpid_snapshot->super.process_pid ) {
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
            vpid_snapshot = (orte_snapc_full_local_snapshot_t*)item;

            if(0 == orte_ns.compare_fields(ORTE_NS_CMP_ALL, proc_name, &vpid_snapshot->super.process_name) ) { 
                vpid_snapshot->super.process_pid = pid;
                break;
            }
        }
        
    get_next_value:
        ;/* */
    }

    /*
     * Send list to global coordinator
     */
    if( ORTE_SUCCESS != (ret = snapc_full_local_send_vpids() ) ) {
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    if( NULL != segment)
        free(segment);

    return exit_status;
}

/************************
 * Start the checkpoint
 ************************/
static int snapc_full_local_start_checkpoint_all(size_t ckpt_state)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_snapc_full_local_snapshot_t *vpid_snapshot;
    opal_list_item_t* item = NULL;
    char * actual_local_dir = NULL;
    bool ckpt_n_term = false;
    char *tmp_pid = NULL;

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
        ckpt_n_term = false;
    }
    else if( ORTE_SNAPC_CKPT_STATE_PENDING_TERM == ckpt_state ) {
        ckpt_n_term = true;
    }
    else {
        ckpt_n_term = false;
    }

    /*
     * Pass 1: Setup snapshot directory
     */
    for(item  = opal_list_get_first(&snapc_local_vpids);
        item != opal_list_get_end(&snapc_local_vpids);
        item  = opal_list_get_next(item) ) {
        vpid_snapshot = (orte_snapc_full_local_snapshot_t*)item;

        /*
         * Set up the snapshot directory per suggestion from 
         * the Global Snapshot Coordinator
         * If we can't create the suggested local directory, do what we can and update
         * local directory reference in the GPR
         */
        if( ORTE_SUCCESS != (ret = snapc_full_local_setup_snapshot_dir(vpid_snapshot->super.crs_snapshot_super.reference_name,
                                                                       vpid_snapshot->super.crs_snapshot_super.local_location,
                                                                       &actual_local_dir) ) ) {
            exit_status = ret;
            goto cleanup;
        }

        opal_output_verbose(20, mca_snapc_full_component.super.output_handle,
                            "local) Using directory (%s)\n",vpid_snapshot->super.crs_snapshot_super.local_location);

        /* Dummy check */
        if( vpid_snapshot->super.process_pid == 0 ) {
            ret = snapc_full_local_get_updated_vpids();
            if( ORTE_SUCCESS != ret || vpid_snapshot->super.process_pid == 0 ) {
                opal_output( mca_snapc_full_component.super.output_handle,
                             "local) Cannot checkpoint an invalid pid (%d)\n", 
                             vpid_snapshot->super.process_pid);
                exit_status = ORTE_ERROR;
                goto cleanup;
            }
        }
    }

    /*
     * Pass 2: Start process of opening communication channels
     */
    for(item  = opal_list_get_first(&snapc_local_vpids);
        item != opal_list_get_end(&snapc_local_vpids);
        item  = opal_list_get_next(item) ) {
        vpid_snapshot = (orte_snapc_full_local_snapshot_t*)item;

        /*
         * Create named pipe references for this process
         */
        if( NULL == vpid_snapshot->comm_pipe_w ||
            NULL == vpid_snapshot->comm_pipe_r ) {
            if( NULL != tmp_pid ) {
                free(tmp_pid);
                tmp_pid = NULL;
            }
            asprintf(&tmp_pid, "%d", vpid_snapshot->super.process_pid);
            asprintf(&(vpid_snapshot->comm_pipe_w), "%s/%s.%s", opal_cr_pipe_dir, OPAL_CR_NAMED_PROG_R, tmp_pid);
            asprintf(&(vpid_snapshot->comm_pipe_r), "%s/%s.%s", opal_cr_pipe_dir, OPAL_CR_NAMED_PROG_W, tmp_pid);
        }

        /*
         * Signal the application
         */
        if( 0 != (ret = kill(vpid_snapshot->super.process_pid, opal_cr_entry_point_signal) ) ) {
            exit_status = ret;
            goto cleanup;
        }
    }

    /*
     * Pass 3: Wait for channels to open up
     */
    for(item  = opal_list_get_first(&snapc_local_vpids);
        item != opal_list_get_end(&snapc_local_vpids);
        item  = opal_list_get_next(item) ) {
        vpid_snapshot = (orte_snapc_full_local_snapshot_t*)item;

        if( ORTE_SUCCESS != (ret = snapc_full_local_start_ckpt_open_comm(vpid_snapshot) ) ) {
            opal_output(mca_snapc_full_component.super.output_handle,
                        "local) Error: Unable to initiate the handshake with peer %s. %d\n", 
                        ORTE_NAME_PRINT(&vpid_snapshot->super.process_name), ret);
            exit_status = OPAL_ERROR;
            goto cleanup;
        }

        /*
         * Update so that folks know that we are working on it
         */
        if( ORTE_SUCCESS != (ret = orte_snapc_base_set_vpid_ckpt_info( vpid_snapshot->super.process_name,
                                                                       ORTE_SNAPC_CKPT_STATE_RUNNING,
                                                                       vpid_snapshot->super.crs_snapshot_super.reference_name,
                                                                       vpid_snapshot->super.crs_snapshot_super.local_location ) ) ) {
            exit_status = ret;
            goto cleanup;
        }
    }

    /*
     * Pass 3: Start Handshake, send term argument
     */
    for(item  = opal_list_get_first(&snapc_local_vpids);
        item != opal_list_get_end(&snapc_local_vpids);
        item  = opal_list_get_next(item) ) {
        vpid_snapshot = (orte_snapc_full_local_snapshot_t*)item;

        if( ORTE_SUCCESS != (ret = snapc_full_local_start_ckpt_handshake_term(vpid_snapshot, ckpt_n_term) ) ) {
            opal_output(mca_snapc_full_component.super.output_handle,
                        "local) Error: Unable to initiate the handshake with peer %s. %d\n", 
                        ORTE_NAME_PRINT(&vpid_snapshot->super.process_name), ret);
            exit_status = OPAL_ERROR;
            goto cleanup;
        }
    }

    /*
     * Pass 4: Start Handshake, send snapshot reference/location arguments
     */
    for(item  = opal_list_get_first(&snapc_local_vpids);
        item != opal_list_get_end(&snapc_local_vpids);
        item  = opal_list_get_next(item) ) {
        vpid_snapshot = (orte_snapc_full_local_snapshot_t*)item;

        if( ORTE_SUCCESS != (ret = snapc_full_local_start_ckpt_handshake(vpid_snapshot) ) ) {
            opal_output(mca_snapc_full_component.super.output_handle,
                        "local) Error: Unable to initiate the handshake with peer %s. %d\n", 
                        ORTE_NAME_PRINT(&vpid_snapshot->super.process_name), ret);
            exit_status = OPAL_ERROR;
            goto cleanup;
        }
    }

 cleanup:
    if( NULL != tmp_pid ) {
        free(tmp_pid);
        tmp_pid = NULL;
    }

    if( ORTE_SUCCESS != exit_status ) {
        ckpt_state = ORTE_SNAPC_CKPT_STATE_ERROR;
    }

    return exit_status;
}

static int snapc_full_local_start_ckpt_open_comm(orte_snapc_full_local_snapshot_t *vpid_snapshot)
{
    int ret, exit_status = ORTE_SUCCESS;
    int usleep_time = 1000;
    int s_time = 0, max_wait_time;

    max_wait_time = 20 * (1000000/usleep_time); /* wait time before giving up on the checkpoint */

    /*
     * Wait for the named pipes to be created
     */
    opal_output_verbose(10, mca_snapc_full_component.super.output_handle,
                        "local) Waiting for process %s's pipes (%s) (%s)\n",
                        ORTE_NAME_PRINT(&vpid_snapshot->super.process_name),
                        vpid_snapshot->comm_pipe_w,
                        vpid_snapshot->comm_pipe_r);
    for( s_time = 0; s_time < max_wait_time; ++s_time) {
        /*
         * See if the named pipe exists yet for the PID in question
         */
        if( 0 > (ret = access(vpid_snapshot->comm_pipe_r, F_OK) )) {
            /* File doesn't exist yet, keep waiting */
            if( s_time >= max_wait_time - 5 ) {
                opal_output_verbose(15, mca_snapc_full_component.super.output_handle,
                                    "local) File does not exist yet: <%s> rtn = %d (waited %d/%d usec)\n",
                                    vpid_snapshot->comm_pipe_r, ret, s_time, max_wait_time);
            }
            usleep(usleep_time);
            continue;
        }
        else if( 0 > (ret = access(vpid_snapshot->comm_pipe_w, F_OK) )) {
            /* File doesn't exist yet, keep waiting */
            if( s_time >= max_wait_time - 5 ) {
                opal_output_verbose(15, mca_snapc_full_component.super.output_handle,
                                    "local) File does not exist yet: <%s> rtn = %d (waited %d/%d usec)\n",
                                    vpid_snapshot->comm_pipe_w, ret, s_time, max_wait_time);
            }
            usleep(usleep_time);
            continue;
        }
        else {
            break;
        }
    }
    if( s_time == max_wait_time ) { 
        /* The file doesn't exist, 
         * This means that the process didn't open up a named pipe for us
         * to access their checkpoint notification routine. Therefore,
         * the application either:
         *  - Doesn't exist
         *  - Isn't checkpointable
         * In either case there is nothing we can do.
         */
        opal_show_help("help-opal-checkpoint.txt", "pid_does_not_exist", true,
                       vpid_snapshot->super.process_pid,
                       vpid_snapshot->comm_pipe_r,
                       vpid_snapshot->comm_pipe_w);

        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    /*
     * Open Pipes...
     *  - prog_named_write_pipe:
     *    prog makes this file and opens Read Only
     *    this app. opens it Write Only
     *  - prog_named_read_pipe:
     *    prog makes this file and opens Write Only
     *    this app. opens it Read Only
     */
    vpid_snapshot->comm_pipe_w_fd = open(vpid_snapshot->comm_pipe_w, O_WRONLY);
    if(vpid_snapshot->comm_pipe_w_fd < 0) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "local) Error: Unable to open name pipe (%s). %d\n", 
                    vpid_snapshot->comm_pipe_w, vpid_snapshot->comm_pipe_w_fd);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    vpid_snapshot->comm_pipe_r_fd = open(vpid_snapshot->comm_pipe_r, O_RDWR);
    if(vpid_snapshot->comm_pipe_r_fd < 0) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "local) Error: Unable to open name pipe (%s). %d\n", 
                    vpid_snapshot->comm_pipe_r, vpid_snapshot->comm_pipe_r_fd);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

 cleanup:
    return exit_status;
}

static int snapc_full_local_start_ckpt_handshake_term(orte_snapc_full_local_snapshot_t *vpid_snapshot, bool term)
{
    int ret, exit_status = ORTE_SUCCESS;
    int term_rep;

    /*
     * Start the handshake: Send term argument
     */
    term_rep = (int)term;

    if( term ) {
        opal_output_verbose(10, mca_snapc_full_component.super.output_handle,
                            "local) Tell app to TERMINATE after completion of checkpoint. [%s (%d)]\n",
                            (term ? "True" : "False"), term_rep);
    }

    if( sizeof(int) != (ret = write(vpid_snapshot->comm_pipe_w_fd, &term_rep, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "local) Error: Unable to write term (%d) to named pipe (%s), %d\n", 
                    term, vpid_snapshot->comm_pipe_w, ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

 cleanup:
    return exit_status;
}

static int snapc_full_local_start_ckpt_handshake(orte_snapc_full_local_snapshot_t *vpid_snapshot)
{
    int ret, exit_status = ORTE_SUCCESS;
    char *local_dir = NULL;
    int len, value;
    ssize_t tmp_size = 0;

    /*
     * Wait for the appliation to respond
     */
    if( sizeof(int) != (ret = read(vpid_snapshot->comm_pipe_r_fd, &value, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "local) Error: Unable to read length from named pipe (%s). %d\n", 
                    vpid_snapshot->comm_pipe_r, ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    /* Check the response to make sure we can checkpoint this process */
    if( OPAL_CHECKPOINT_CMD_IN_PROGRESS == value ) {
        opal_show_help("help-opal-checkpoint.txt",
                       "ckpt:in_progress", 
                       true,
                       vpid_snapshot->super.process_pid);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }
    else if( OPAL_CHECKPOINT_CMD_NULL == value ) {
        opal_show_help("help-opal-checkpoint.txt",
                       "ckpt:req_null", 
                       true,
                       vpid_snapshot->super.process_pid);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }
    else if ( OPAL_CHECKPOINT_CMD_ERROR == value ) {
        opal_show_help("help-opal-checkpoint.txt",
                       "ckpt:req_error", 
                       true,
                       vpid_snapshot->super.process_pid);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    opal_event_set(&(vpid_snapshot->comm_pipe_r_eh),
                   vpid_snapshot->comm_pipe_r_fd,
                   OPAL_EV_READ|OPAL_EV_PERSIST,
                   snapc_full_local_comm_read_event,
                   vpid_snapshot);
    vpid_snapshot->is_eh_active = true;
    opal_event_add(&(vpid_snapshot->comm_pipe_r_eh), NULL);

    /*
     * Send: Snapshot Name
     */
    len = strlen(vpid_snapshot->super.crs_snapshot_super.reference_name) + 1;
    if( sizeof(int) != (ret = write(vpid_snapshot->comm_pipe_w_fd, &len, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "local) Error: Unable to write snapshot name len (%d) to named pipe (%s). %d\n", 
                    len, vpid_snapshot->comm_pipe_w, ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    tmp_size = sizeof(char) * len;
    if( tmp_size != (ret = write(vpid_snapshot->comm_pipe_w_fd, (vpid_snapshot->super.crs_snapshot_super.reference_name), (sizeof(char) * len))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "local) Error: Unable to write snapshot name (%s) to named pipe (%s). %d\n", 
                    vpid_snapshot->super.crs_snapshot_super.reference_name, vpid_snapshot->comm_pipe_w, ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    /*
     * Send: Snapshot Location
     */
    local_dir = strdup(vpid_snapshot->super.crs_snapshot_super.local_location);
    local_dir = opal_dirname(local_dir);
    len = strlen(local_dir) + 1;
    if( sizeof(int) != (ret = write(vpid_snapshot->comm_pipe_w_fd, &len, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "local) Error: Unable to write snapshot location len (%d) to named pipe (%s). %d\n", 
                    len, vpid_snapshot->comm_pipe_w, ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    tmp_size = sizeof(char) * len;
    if( tmp_size != (ret = write(vpid_snapshot->comm_pipe_w_fd, (local_dir), (sizeof(char) * len))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "local) Error: Unable to write snapshot location (%s) to named pipe (%s). %d\n", 
                    local_dir, vpid_snapshot->comm_pipe_w, ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

 cleanup:
    if( NULL != local_dir ) {
        free(local_dir);
        local_dir = NULL;
    }

    return exit_status;
}

static int snapc_full_local_end_ckpt_handshake(orte_snapc_full_local_snapshot_t *vpid_snapshot)
{
    int ret, exit_status = ORTE_SUCCESS;
    int last_cmd = 0;

    /*
     * Finish the handshake.
     */
    if( sizeof(int) != (ret = write(vpid_snapshot->comm_pipe_w_fd, &last_cmd, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "local) Error: Unable to release process %s (%d)\n", 
                    ORTE_NAME_PRINT(&vpid_snapshot->super.process_name), ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

 cleanup:
    /*
     * Close all pipes
     */
    close(vpid_snapshot->comm_pipe_w_fd);
    close(vpid_snapshot->comm_pipe_r_fd);
    vpid_snapshot->comm_pipe_w_fd = -1;
    vpid_snapshot->comm_pipe_r_fd = -1;

    return exit_status;
}

static void snapc_full_local_comm_read_event(int fd, short flags, void *arg)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_snapc_full_local_snapshot_t *vpid_snapshot = NULL;
    size_t loc_state = ORTE_SNAPC_CKPT_STATE_FINISHED;
    int ckpt_state;

    vpid_snapshot = (orte_snapc_full_local_snapshot_t *)arg;

    opal_output_verbose(10, mca_snapc_full_component.super.output_handle,
                        "local) Read Event: Process %s done...\n",
                        ORTE_NAME_PRINT(&vpid_snapshot->super.process_name));

    /*
     * Get the final state of the checkpoint from the checkpointing process
     */
    if( sizeof(int) != (ret = read(vpid_snapshot->comm_pipe_r_fd, &ckpt_state, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "local) Error: Unable to read state from named pipe (%s). %d\n",
                    vpid_snapshot->comm_pipe_r, ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    if( ckpt_state == OPAL_CRS_ERROR ) {
        loc_state = ORTE_SNAPC_CKPT_STATE_ERROR;
    }

    /*
     * Now that the checkpoint is finished
     * Update our status information
     */
    vpid_snapshot->super.state = loc_state;
    if( ORTE_SUCCESS != (ret = orte_snapc_base_set_vpid_ckpt_info( vpid_snapshot->super.process_name,
                                                                   loc_state,
                                                                   vpid_snapshot->super.crs_snapshot_super.reference_name,
                                                                   vpid_snapshot->super.crs_snapshot_super.local_location ) ) ) {
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    /*
     * Disable events
     */
    opal_event_del(&(vpid_snapshot->comm_pipe_r_eh));
    vpid_snapshot->is_eh_active = false;

    return;
}
