/*
 * Copyright (c) 2009-2010 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
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

#include "opal/util/show_help.h"
#include "opal/util/output.h"
#include "opal/util/opal_environ.h"
#include "opal/util/basename.h"
#include "opal/util/argv.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

#include "orte/util/error_strings.h"
#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/orte_globals.h"
#include "opal/dss/dss.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/iof/iof.h"
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
#include "orte/mca/errmgr/base/errmgr_private.h"

#include "errmgr_hnp.h"

#include MCA_timer_IMPLEMENTATION_HEADER

#if OPAL_ENABLE_FT_CR

/************************************
 * Locally Global vars & functions :)
 ************************************/
static orte_jobid_t current_global_jobid = ORTE_JOBID_INVALID;
static orte_job_t  *current_global_jobdata = NULL;

static bool migrating_underway   = false;
static bool migrating_terminated = false;
static bool migrating_restarted  = false;

static opal_list_t *current_onto_mapping_general = NULL;
static opal_list_t *current_onto_mapping_exclusive = NULL;

/*** Command Line Interactions */
static int current_migration_status = ORTE_ERRMGR_MIGRATE_STATE_NONE;

static int errmgr_crmig_global_migrate(opal_list_t *off_procs, opal_list_t *off_nodes, opal_list_t *onto_map);

static int orte_errmgr_hnp_crmig_global_process_fault(orte_job_t *jdata,
                                                        orte_process_name_t *proc_name,
                                                        orte_proc_state_t state);
static void errmgr_crmig_process_fault_app(orte_job_t *jdata,
                                           orte_process_name_t *proc,
                                           orte_proc_state_t state);
static void errmgr_crmig_process_fault_daemon(orte_job_t *jdata,
                                              orte_process_name_t *proc,
                                              orte_proc_state_t state);

static bool check_if_duplicate_proc(orte_proc_t *proc, opal_pointer_array_t *migrating_procs);
static int check_if_terminated(opal_pointer_array_t *migrating_procs);
static int check_if_restarted(opal_pointer_array_t *migrating_procs);

static int check_and_pre_map(opal_list_t *off_procs,
                             opal_list_t *off_nodes,
                             orte_snapc_base_quiesce_t *cur_datum);

static void display_request(opal_list_t *off_procs,
                            opal_list_t *off_nodes,
                            orte_snapc_base_quiesce_t *cur_datum);

/*
 * Timer stuff
 */
static void errmgr_crmig_set_time(int idx);
static void errmgr_crmig_display_all_timers(void);
static void errmgr_crmig_clear_timers(void);

static double errmgr_crmig_get_time(void);
static void errmgr_crmig_display_indv_timer_core(double diff, char *str);
static double timer_start[OPAL_CR_TIMER_MAX];

#define ERRMGR_CRMIG_TIMER_START    0
#define ERRMGR_CRMIG_TIMER_SETUP    1
#define ERRMGR_CRMIG_TIMER_CKPT     2
#define ERRMGR_CRMIG_TIMER_TERM     3
#define ERRMGR_CRMIG_TIMER_RESETUP  4
#define ERRMGR_CRMIG_TIMER_RESTART  5
#define ERRMGR_CRMIG_TIMER_FINISH   6
#define ERRMGR_CRMIG_TIMER_MAX      7

#define ERRMGR_CRMIG_CLEAR_TIMERS()                                     \
    {                                                                   \
        if(OPAL_UNLIKELY(mca_errmgr_hnp_component.crmig_timing_enabled > 0)) { \
            errmgr_crmig_clear_timers();                                \
        }                                                               \
    }

#define ERRMGR_CRMIG_SET_TIMER(idx)                                     \
    {                                                                   \
        if(OPAL_UNLIKELY(mca_errmgr_hnp_component.crmig_timing_enabled > 0)) { \
            errmgr_crmig_set_time(idx);                                 \
        }                                                               \
    }

#define ERRMGR_CRMIG_DISPLAY_ALL_TIMERS()                               \
    {                                                                   \
        if(OPAL_UNLIKELY(mca_errmgr_hnp_component.crmig_timing_enabled > 0)) { \
            errmgr_crmig_display_all_timers();                          \
        }                                                               \
    }

/************************
 * Function Definitions: Global
 ************************/
int orte_errmgr_hnp_crmig_global_module_init(void)
{
    int ret;

    opal_output_verbose(10, mca_errmgr_hnp_component.super.output_handle,
                        "errmgr:hnp(crmig): init()");

    migrating_underway = false;

    current_global_jobid   = ORTE_JOBID_INVALID;
    current_global_jobdata = NULL;

    /*
     * Initialize the connection to the orte-migrate tool
     */
    if( ORTE_SUCCESS != (ret = orte_errmgr_base_tool_init()) ) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    ERRMGR_CRMIG_CLEAR_TIMERS();

    return ORTE_SUCCESS;
}

int orte_errmgr_hnp_crmig_global_module_finalize(void)
{
    int ret;

    opal_output_verbose(10, mca_errmgr_hnp_component.super.output_handle,
                        "errmgr:hnp(crmig): finalize()");

    /*
     * Finalize the connection to the orte-migrate tool
     */
    if( ORTE_SUCCESS != (ret = orte_errmgr_base_tool_finalize()) ) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    migrating_underway = false;

    current_global_jobid   = ORTE_JOBID_INVALID;
    current_global_jobdata = NULL;

    ERRMGR_CRMIG_CLEAR_TIMERS();

    return ORTE_SUCCESS;
}

int orte_errmgr_hnp_crmig_global_predicted_fault(opal_list_t *proc_list,
                                                   opal_list_t *node_list,
                                                   opal_list_t *suggested_map)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_job_t *jdata = NULL;
    int i;

    /*
     * JJH: RETURN HERE
     * If we are already migrating, then reject this request
     */
    if( migrating_underway ) {
        ;
    }

    /*
     * Determine the jobid for this migration
     * JJH: Assumes only one job active at any one time
     */
    for(i = 0; i < orte_job_data->size; ++i ) {
        if (NULL == (jdata = (orte_job_t*)opal_pointer_array_get_item(orte_job_data, i))) {
            continue;
        }
        /* Exclude outselves */
        if( jdata->jobid == ORTE_PROC_MY_NAME->jobid ) {
            continue;
        }
        current_global_jobdata = jdata;
        current_global_jobid   = jdata->jobid;
        break;
    }
    if( NULL == current_global_jobdata ) {
        opal_output(0, "errmgr:hnp(crmig):predicted_fault(): Global) Error: Cannot find the jdata for the current job.");
        ORTE_ERROR_LOG(ORTE_ERROR);
        return ORTE_ERROR;
    }
    current_global_jobdata->controls |= ORTE_JOB_CONTROL_RECOVERABLE;

    current_migration_status = ORTE_ERRMGR_MIGRATE_STATE_REQUEST;
    if( ORTE_SUCCESS != (ret = orte_errmgr_base_migrate_update(current_migration_status)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*************************
     * Kick off the migration
     *************************/
    if( ORTE_SUCCESS != (ret = errmgr_crmig_global_migrate(proc_list, node_list, suggested_map)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /************************
     * Set up the Command Line listener again
     *************************/
    if( ORTE_ERRMGR_MIGRATE_STATE_ERROR != current_migration_status ) {
        if( ORTE_SUCCESS != (ret = orte_errmgr_base_migrate_update(ORTE_ERRMGR_MIGRATE_STATE_NONE)) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        opal_show_help("help-orte-errmgr-hnp.txt", "crmig_migrated_job", true);
    }
    current_migration_status = ORTE_ERRMGR_MIGRATE_STATE_NONE;

 cleanup:
    return exit_status;
}

int orte_errmgr_hnp_crmig_global_update_state(orte_jobid_t job,
                                                orte_job_state_t jobstate,
                                                orte_process_name_t *proc_name,
                                                orte_proc_state_t state,
                                                pid_t pid,
                                                orte_exit_code_t exit_code)
{
    orte_job_t *jdata = NULL;
    int ret = ORTE_SUCCESS;

    /*
     * if orte is trying to shutdown, just let it
     */
    if( mca_errmgr_hnp_component.term_in_progress ) {
        return ORTE_SUCCESS;
    }

    /*
     * Get the job data object for this process
     */
    if( NULL != proc_name ) { /* Get job from proc's jobid */
        jdata = orte_get_job_data_object(proc_name->jobid);
    } else { /* Get from the general job */
        jdata = orte_get_job_data_object(job);
    }
    if( NULL == jdata ) {
        opal_output(0, "%s errmgr:hnp(crmig):update_state() Error: Cannot find job %s for Process %s",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    ORTE_JOBID_PRINT(job),
                    (NULL == proc_name) ? "NULL" : ORTE_NAME_PRINT(proc_name) );
        ret = ORTE_ERROR;
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /*
     * If this is a tool, ignore
     */
    if( jdata->num_apps == 0 &&
        OPAL_EQUAL != orte_util_compare_name_fields(ORTE_NS_CMP_JOBID, ORTE_PROC_MY_NAME, proc_name) ) {
        OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base.output,
                             "%s errmgr:hnp(crmig): An external tool disconnected. Ignore...",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        return ORTE_SUCCESS;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base.output,
                         "%s errmgr:hnp(crmig): job %s reported state %s"
                         " for proc %s state %s exit_code %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(job),
                         orte_job_state_to_str(jobstate),
                         (NULL == proc_name) ? "NULL" : ORTE_NAME_PRINT(proc_name),
                         orte_proc_state_to_str(state), exit_code));

    if( ORTE_PROC_STATE_ABORTED_BY_SIG == state ||
        ORTE_PROC_STATE_COMM_FAILED    == state ) {
        if( ORTE_SUCCESS != (ret = orte_errmgr_hnp_crmig_global_process_fault(jdata, proc_name, state)) ) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
    }
    else if( ORTE_PROC_STATE_KILLED_BY_CMD == state ) {
        if( migrating_underway ) {
            /* If we are migrating, then we need to mask this to prevent the lower level from terminating us */
            mca_errmgr_hnp_component.ignore_current_update = true;
            orte_errmgr_hnp_update_proc(jdata, proc_name, state, 0, exit_code);
        }
    }

    return ORTE_SUCCESS;
}

int orte_errmgr_hnp_crmig_global_suggest_map_targets(orte_proc_t *proc,
                                                       orte_node_t *oldnode,
                                                       opal_list_t *node_list)
{
    int exit_status = ORTE_SUCCESS;
    opal_list_item_t *item = NULL, *m_item = NULL;
    orte_errmgr_predicted_map_t  *onto_map = NULL, *current_proc_map = NULL;
    orte_node_t *node = NULL;
    bool found = false;
    int num_suggested = 0;
    orte_std_cntr_t i_proc;
    orte_proc_t *peer_proc = NULL;

    /*
     * If not migrating, then suggest nothing
     */
    if( !migrating_underway ) {
        return ORTE_SUCCESS;
    }

    /*
     * First look for an exclusive mapping for this process
     */
    for(item  = opal_list_get_first(current_onto_mapping_exclusive);
        item != opal_list_get_end(current_onto_mapping_exclusive);
        item  = opal_list_get_next(item) ) {
        onto_map = (orte_errmgr_predicted_map_t*) item;
        if( onto_map->proc_name.vpid == proc->name.vpid ) {
            current_proc_map = onto_map;
            break;
        }
    }

    /*
     * If there is an exclusive mapping then...
     */
    if( NULL != current_proc_map ) {
        /*
         * If we made an exclusive mapping during the check_and_pre_map()
         * then honor it here.
         */
        if( NULL != current_proc_map->pre_map_fixed_node ) {
            for( item  = opal_list_get_first(node_list);
                 item != opal_list_get_end(node_list);
                 item  = opal_list_get_next(item) ) {
                node = (orte_node_t*)item;

                /* Exclude all other nodes */
                found = false;

                if( 0 == strncmp(node->name, current_proc_map->pre_map_fixed_node,
                                 strlen(current_proc_map->pre_map_fixed_node)) ) {
                    found = true;
                    break;
                }
                if( !found ) {
                    opal_list_remove_item(node_list, item);
                    OBJ_RELEASE(item);
                    continue;
                } else {
                    OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                                         "errmgr:hnp(crmig):suggest() ------- Fixed use of node [%15s : %10s -> %10s (%10s)] -------",
                                         ORTE_NAME_PRINT(&proc->name), oldnode->name,
                                         current_proc_map->pre_map_fixed_node, node->name));
                }
            }

            /* All done with mapping */
            exit_status = ORTE_SUCCESS;
            goto cleanup;
        }

        /*
         * If 'off_current_node' then exclude current node
         */
        if( current_proc_map->off_current_node ) {
            OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                                 "errmgr:hnp(crmig):suggest() ------- Remove old node (info) [%15s : %10s] -------",
                                 ORTE_NAME_PRINT(&proc->name), oldnode->name));
            for( item  = opal_list_get_first(node_list);
                 item != opal_list_get_end(node_list);
                 item  = opal_list_get_next(item) ) {
                node = (orte_node_t*)item;

                /* Exclude the old node */
                if( node == oldnode ) {
                    opal_list_remove_item(node_list, item);
                    OBJ_RELEASE(item);
                    break;
                }
            }
        }

        /*
         * If 'map_proc_name' then map to the node where this process resides
         * Note: Only do this if there was no 'other' node suggested. If there
         *       was an 'other' node suggested then we need to honor that before
         *       we honor the peer suggestion.
         */
        if( ORTE_VPID_INVALID != current_proc_map->map_proc_name.vpid &&
            current_proc_map->proc_name.vpid != current_proc_map->map_proc_name.vpid &&
            NULL == current_proc_map->map_node_name ) {
            /*
             * Find the node containting the target process
             */
            for(i_proc = 0; i_proc < opal_pointer_array_get_size(current_global_jobdata->procs); ++i_proc) {
                peer_proc = (orte_proc_t*)opal_pointer_array_get_item(current_global_jobdata->procs, i_proc);
                if( NULL == peer_proc ) {
                    continue;
                }
                if( peer_proc->name.vpid == current_proc_map->map_proc_name.vpid ) {
                    current_proc_map->map_node_name = strdup(peer_proc->node->name);
                    break;
                }
            }
            OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                                 "errmgr:hnp(crmig):suggest() ------- Force use of node with proc [%15s -> %15s: %10s -> %10s] -------",
                                 ORTE_NAME_PRINT(&proc->name), ORTE_NAME_PRINT(&peer_proc->name),
                                 oldnode->name, current_proc_map->map_node_name));
        }

        /*
         * If 'map_node_name' then use this node exclusively
         */
        if( NULL != current_proc_map->map_node_name ) {
            for( item  = opal_list_get_first(node_list);
                 item != opal_list_get_end(node_list);
                 item  = opal_list_get_next(item) ) {
                node = (orte_node_t*)item;

                /* Exclude all nodes not in the include list */
                found = false;

                if( 0 == strncmp(node->name, current_proc_map->map_node_name, strlen(current_proc_map->map_node_name)) ) {
                    found = true;
                }
                if( !found ) {
                    opal_list_remove_item(node_list, item);
                    OBJ_RELEASE(item);
                    continue;
                } else {
                    OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                                         "errmgr:hnp(crmig):suggest() ------- Force use of node [%15s : %10s -> %10s (%10s)] -------",
                                         ORTE_NAME_PRINT(&proc->name), oldnode->name,
                                         current_proc_map->map_node_name, node->name));
                }
            }

            /* All done with mapping */
            exit_status = ORTE_SUCCESS;
            goto cleanup;
        }

        /*
         * Otherwise then map as if there was no exclusive mapping
         */
        OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                             "errmgr:hnp(crmig):suggest() ------- Suggesting as if non-exclusive [%15s : 0x%x : %10s] -------",
                             ORTE_NAME_PRINT(&proc->name), proc->state, oldnode->name));
    }
    /*
     * If no exclusive mapping (or exclusive did not yield any results) then...
     */
    else {
        /*
         * Remove the old node from the list, if there are more than 1 nodes available
         */
        if(1 < opal_list_get_size(node_list) ) {
            OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                                 "errmgr:hnp(crmig):suggest() ------- Remove old node [%15s : %10s] -------",
                                 ORTE_NAME_PRINT(&proc->name), oldnode->name));
            for( item  = opal_list_get_first(node_list);
                 item != opal_list_get_end(node_list);
                 item  = opal_list_get_next(item) ) {
                node = (orte_node_t*)item;

                /* Exclude the old node */
                if( node == oldnode ) {
                    opal_list_remove_item(node_list, item);
                    OBJ_RELEASE(item);
                    break;
                }
            }
        }
    }

    /*
     * If we do not have any general suggestions, then just return
     */
    if( opal_list_get_size(current_onto_mapping_general) <= 0 ) {
        OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                             "errmgr:hnp(crmig):suggest() ------- No suggestions for target [%15s : 0x%x : %10s] -------",
                             ORTE_NAME_PRINT(&proc->name), proc->state, oldnode->name));
        exit_status = ORTE_SUCCESS;
        goto cleanup;
    }

    /*
     * Otherwise look through the general suggestions as an include list
     */
    OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                         "errmgr:hnp(crmig):suggest() ------- Suggest a target for [%15s : 0x%x : %10s] -------",
                         ORTE_NAME_PRINT(&proc->name), proc->state, oldnode->name));

    num_suggested = 0;
    for( item  = opal_list_get_first(node_list);
         item != opal_list_get_end(node_list);
         item  = opal_list_get_next(item) ) {
        node = (orte_node_t*)item;

        /* Exclude all nodes not in the include list */
        found = false;

        for(m_item  = opal_list_get_first(current_onto_mapping_general);
            m_item != opal_list_get_end(current_onto_mapping_general);
            m_item  = opal_list_get_next(m_item) ) {
            onto_map = (orte_errmgr_predicted_map_t*) m_item;

            if( 0 == strncmp(node->name, onto_map->map_node_name, strlen(onto_map->map_node_name)) ) {
                found = true;
                break;
            }
        }
        if( !found ) {
            opal_list_remove_item(node_list, item);
            OBJ_RELEASE(item);
            continue;
        }

        ++num_suggested;

        OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                             "errmgr:hnp(crmig):suggest() ------- Suggesting target %2d [%15s : 0x%x : %10s -> %10s] -------",
                             num_suggested, ORTE_NAME_PRINT(&proc->name), proc->state, oldnode->name, node->name));
    }

 cleanup:
    OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                         "errmgr:hnp(crmig):suggest() ------- Suggested %2d nodes for [%15s : 0x%x : %10s] -------",
                         (int)opal_list_get_size(node_list), ORTE_NAME_PRINT(&proc->name), proc->state, oldnode->name));

    return exit_status;
}

int orte_errmgr_hnp_crmig_global_ft_event(int state)
{
    return ORTE_SUCCESS;
}


/************************
 * Function Definitions: Static
 ************************/
static int orte_errmgr_hnp_crmig_global_process_fault(orte_job_t *jdata,
                                                        orte_process_name_t *proc_name,
                                                        orte_proc_state_t state)
{
    /*
     * JJH: Todo
     * The expected logic here is:
     * if( a daemon with children fails ) {
     *   abort migration.
     * }
     * if( a daemon without children fails ) {
     *   continue. No processes lost
     * }
     * if( an application process fails ) {
     *   abort migration. Might be a bad checkpoint, or a process that we were
     *   not migrating that died.
     * }
     * else {
     *   continue;
     * }
     */
    if( proc_name->jobid == ORTE_PROC_MY_NAME->jobid ) {
        errmgr_crmig_process_fault_daemon(jdata, proc_name, state);
    } else {
        errmgr_crmig_process_fault_app(jdata, proc_name, state);
    }

    return ORTE_SUCCESS;
}

static int errmgr_crmig_global_migrate(opal_list_t *off_procs, opal_list_t *off_nodes, opal_list_t *onto_maps)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_std_cntr_t i_node;
    orte_std_cntr_t i_proc;
    orte_node_t *node = NULL;
    orte_proc_t *proc = NULL;
    bool found = false;
    orte_snapc_base_quiesce_t *cur_datum = NULL;
    bool close_iof_stdin = false;
    orte_process_name_t iof_name = {ORTE_JOBID_INVALID, 0};
    char * err_str_procs = NULL;
    char * err_str_nodes = NULL;
    char * tmp_str = NULL;
    orte_errmgr_predicted_proc_t *off_proc = NULL;
    orte_errmgr_predicted_node_t *off_node = NULL;
    orte_errmgr_predicted_map_t  *onto_map = NULL;
    opal_list_item_t *item = NULL;

    ERRMGR_CRMIG_CLEAR_TIMERS();
    ERRMGR_CRMIG_SET_TIMER(ERRMGR_CRMIG_TIMER_START);

    OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                         "errmgr:hnp(crmig):migrate() ------- Migrating (%3d, %3d, %3d) -------",
                         (int)opal_list_get_size(off_procs),
                         (int)opal_list_get_size(off_nodes),
                         (int)opal_list_get_size(onto_maps)));

    /*
     * Modeled after orte_plm_base_reset_job
     */
    cur_datum = OBJ_NEW(orte_snapc_base_quiesce_t);
    cur_datum->migrating = true;
    migrating_underway = true;

    current_migration_status = ORTE_ERRMGR_MIGRATE_STATE_RUNNING;
    if( ORTE_SUCCESS != (ret = orte_errmgr_base_migrate_update(current_migration_status)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Check to make sure that the 'off' and 'onto' nodes exist
     * - if 'onto' nodes do not, then add them (JJH XXX)
     * - if 'off' nodes do not, then return an error (JJH XXX)
     *  JJH TODO...
     */

    /*
     * Copy over the onto_nodes so we can suggest them later
     */
    if( NULL != current_onto_mapping_general ) {
        OBJ_RELEASE(current_onto_mapping_general);
        current_onto_mapping_general = NULL;
    }
    if( NULL != current_onto_mapping_exclusive ) {
        OBJ_RELEASE(current_onto_mapping_exclusive);
        current_onto_mapping_exclusive = NULL;
    }
    current_onto_mapping_general   = OBJ_NEW(opal_list_t);
    current_onto_mapping_exclusive = OBJ_NEW(opal_list_t);
    if( NULL != onto_maps ) {
        while( NULL != (item = opal_list_remove_first(onto_maps)) ) {
            onto_map = (orte_errmgr_predicted_map_t*) item;
            /* Determine if process exclude mapping, or general */
            if( onto_map->proc_name.vpid == ORTE_VPID_INVALID ) {
                opal_list_append(current_onto_mapping_general,   item);
            } else {
                opal_list_append(current_onto_mapping_exclusive, item);
            }
        }
    }

    for(item  = opal_list_get_first(current_onto_mapping_exclusive);
        item != opal_list_get_end(current_onto_mapping_exclusive);
        item  = opal_list_get_next(item) ) {
        onto_map = (orte_errmgr_predicted_map_t*) item;
        /*
         * Find the node currently containing this process
         */
        found = false;
        for(i_proc = 0; i_proc < opal_pointer_array_get_size(current_global_jobdata->procs); ++i_proc) {
            proc = (orte_proc_t*)opal_pointer_array_get_item(current_global_jobdata->procs, i_proc);
            if( NULL == proc ) {
                continue;
            }

            if( proc->name.vpid == onto_map->proc_name.vpid) {
                found = true;
                break;
            }
        }

        /*
         * Check to see if this process hsould be skipped
         */
        if( !onto_map->off_current_node &&
            (ORTE_VPID_INVALID == onto_map->map_proc_name.vpid ||
             onto_map->proc_name.vpid == onto_map->map_proc_name.vpid ) &&
            (NULL == onto_map->map_node_name ||
             0 == strncmp(onto_map->map_node_name, proc->node->name, strlen(proc->node->name))) ) {
            OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                                 "errmgr:hnp(crmig):migrate() ------- Process %15s does not wish to move -------",
                                 ORTE_NAME_PRINT(&proc->name)));

        } else {
            OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                                 "errmgr:hnp(crmig):migrate() ------- Process %15s will be moved -------",
                                 ORTE_NAME_PRINT(&proc->name)));
            /*
             * Set the process to restarting
             */
            proc->state = ORTE_PROC_STATE_MIGRATING;

            opal_pointer_array_add(&(cur_datum->migrating_procs), (void*)proc);
            OBJ_RETAIN(proc);
            (cur_datum->num_migrating)++;

            if( current_global_jobdata->stdin_target == proc->name.vpid ) {
                close_iof_stdin = true;
                iof_name.jobid = proc->name.jobid;
                iof_name.vpid = proc->name.vpid;
            }
        }
    }

    migrating_terminated = false;
    migrating_restarted  = false;

    /*
     * Create a list of processes to migrate, if 'off_nodes' specified
     */
    for(item  = opal_list_get_first(off_nodes);
        item != opal_list_get_end(off_nodes);
        item  = opal_list_get_next(item) ) {
        off_node = (orte_errmgr_predicted_node_t*)item;

        /*
         * Find the node in the job structure
         * - Make sure that 'odin00' doesn't match all 'odin00*'
         */
        found = false;
        for(i_node = 0; i_node < opal_pointer_array_get_size(current_global_jobdata->map->nodes); ++i_node) {
            node = (orte_node_t*)opal_pointer_array_get_item(current_global_jobdata->map->nodes, i_node);
            if( NULL == node ) {
                continue;
            }

            if( 0 == strncmp(node->name, off_node->node_name, strlen(off_node->node_name)) ) {
                found = true;
                break;
            }
        }
        if( !found ) {
            ; /* Warn about invalid node */
        } else {
            /*
             * Add all processes from this node
             */
            for(i_proc = 0; i_proc < opal_pointer_array_get_size(node->procs); ++i_proc) {
                proc = (orte_proc_t*)opal_pointer_array_get_item(node->procs, i_proc);
                if( NULL == proc ) {
                    continue;
                }

                /*
                 * Set the process to restarting
                 */
                proc->state = ORTE_PROC_STATE_MIGRATING;

                opal_pointer_array_add(&(cur_datum->migrating_procs), (void*)proc);
                OBJ_RETAIN(proc);
                (cur_datum->num_migrating)++;

                if( current_global_jobdata->stdin_target == proc->name.vpid ) {
                    close_iof_stdin = true;
                    iof_name.jobid = proc->name.jobid;
                    iof_name.vpid = proc->name.vpid;
                }
            }
        }
    }

    /*
     * Create a list of processes to migrate, if 'off_procs' specified
     */
    for(item  = opal_list_get_first(off_procs);
        item != opal_list_get_end(off_procs);
        item  = opal_list_get_next(item) ) {
        off_proc = (orte_errmgr_predicted_proc_t*)item;

        /*
         * Find the process in the job structure
         */
        found = false;
        for(i_proc = 0; i_proc < opal_pointer_array_get_size(current_global_jobdata->procs); ++i_proc) {
            proc = (orte_proc_t*)opal_pointer_array_get_item(current_global_jobdata->procs, i_proc);
            if( NULL == proc ) {
                continue;
            }

            if( proc->name.vpid == off_proc->proc_name.vpid) {
                found = true;
                break;
            }
        }
        /*
         * Make sure the process is not listed multiple times
         */
        if( found ) {
            found = check_if_duplicate_proc(proc, &(cur_datum->migrating_procs));
            if( !found ) {
                /*
                 * Set the process to restarting
                 */
                proc->state = ORTE_PROC_STATE_MIGRATING;

                opal_pointer_array_add(&(cur_datum->migrating_procs), (void*)proc);
                OBJ_RETAIN(proc);
                (cur_datum->num_migrating)++;

                if( current_global_jobdata->stdin_target == proc->name.vpid ) {
                    close_iof_stdin = true;
                    iof_name.jobid = proc->name.jobid;
                    iof_name.vpid = proc->name.vpid;
                }
            }
        }
    }

    /*
     * If we did not find any processes to migrate, then throw a warning, and skip it.
     */
    if( 0 >= cur_datum->num_migrating ) {
        for(item  = opal_list_get_first(off_nodes);
            item != opal_list_get_end(off_nodes);
            item  = opal_list_get_next(item) ) {
            off_node = (orte_errmgr_predicted_node_t*)item;
            if( NULL != err_str_nodes ) {
                asprintf(&tmp_str, "%s, %s", err_str_nodes, off_node->node_name);
                free(err_str_nodes);
                err_str_nodes = strdup(tmp_str);
                free(tmp_str);
                tmp_str = NULL;
            } else {
                asprintf(&err_str_nodes, "%s", off_node->node_name);
            }
        }

        for(item  = opal_list_get_first(off_procs);
            item != opal_list_get_end(off_procs);
            item  = opal_list_get_next(item) ) {
            off_proc = (orte_errmgr_predicted_proc_t*)item;
            if( NULL != err_str_procs ) {
                asprintf(&tmp_str, "%s, %d", err_str_procs, (int)off_proc->proc_name.vpid);
                free(err_str_procs);
                err_str_procs = strdup(tmp_str);
                free(tmp_str);
                tmp_str = NULL;
            } else {
                asprintf(&err_str_procs, "%d", off_proc->proc_name.vpid);
            }
        }

        opal_show_help("help-orte-errmgr-hnp.txt", "crmig_no_migrating_procs", true,
                       err_str_nodes,
                       err_str_procs);

        current_migration_status = ORTE_ERRMGR_MIGRATE_STATE_ERROR;
        if( ORTE_SUCCESS != (ret = orte_errmgr_base_migrate_update(current_migration_status)) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
        goto cleanup;
    }

    /*
     * Final pass on the migration list to pre-map processes and remove
     * processes that should not be migrated.
     */
    if( ORTE_SUCCESS != (ret = check_and_pre_map(off_procs, off_nodes, cur_datum)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Display the request before processing it.
     */
    display_request(off_procs, off_nodes, cur_datum);

    ERRMGR_CRMIG_SET_TIMER(ERRMGR_CRMIG_TIMER_SETUP);

    /*
     * Checkpoint the job
     * - Hold all non-migrating processes
     * - Abort the marked processes
     * - 
     */
    current_migration_status = ORTE_ERRMGR_MIGRATE_STATE_RUN_CKPT;
    if( ORTE_SUCCESS != (ret = orte_errmgr_base_migrate_update(current_migration_status)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    opal_output_verbose(10, mca_errmgr_hnp_component.super.output_handle,
                        "errmgr:hnp(crmig):migrate() ------- Starting the checkpoint of job %s -------",
                        ORTE_JOBID_PRINT(current_global_jobdata->jobid));

    if( ORTE_SUCCESS != (ret = orte_snapc.start_ckpt(cur_datum)) ) {
        opal_output(0, "errmgr:hnp(crmig):migrate() Error: Unable to start the checkpoint.");
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    ERRMGR_CRMIG_SET_TIMER(ERRMGR_CRMIG_TIMER_CKPT);

    /*
     * Terminate the migrating processes
     */
    opal_output_verbose(10, mca_errmgr_hnp_component.super.output_handle,
                        "errmgr:hnp(crmig):migrate() ------- Terminate old processes in job %s -------",
                        ORTE_JOBID_PRINT(current_global_jobdata->jobid));

    orte_plm.terminate_procs(&cur_datum->migrating_procs);

    /*
     * Clear the IOF stdin target if necessary
     */
    if( close_iof_stdin ) {
        OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                             "errmgr:hnp(crmig):migrate() ------- Closing old STDIN target for job %s (%s)-------",
                             ORTE_JOBID_PRINT(current_global_jobdata->jobid),
                             ORTE_NAME_PRINT(&iof_name) ));

        orte_iof.close(&iof_name, ORTE_IOF_STDIN);
    }

    /*
     * Wait for the processes to finish terminating
     */
    opal_output_verbose(10, mca_errmgr_hnp_component.super.output_handle,
                        "errmgr:hnp(crmig):migrate() ------- Waiting for termination -------");

    while( !migrating_terminated ) {
        opal_progress();
        check_if_terminated(&(cur_datum->migrating_procs));
    }

    ERRMGR_CRMIG_SET_TIMER(ERRMGR_CRMIG_TIMER_TERM);

    /*
     * Start remapping the processes
     */
    opal_output_verbose(10, mca_errmgr_hnp_component.super.output_handle,
                        "errmgr:hnp(crmig):migrate() ------- Checkpoint finished, setting up job %s -------",
                        ORTE_JOBID_PRINT(current_global_jobdata->jobid));

    current_migration_status = ORTE_ERRMGR_MIGRATE_STATE_STARTUP;
    if( ORTE_SUCCESS != (ret = orte_errmgr_base_migrate_update(current_migration_status)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Reset the job parameters for restart
     * This will set the state of the job to 'restart'
     */
    orte_plm_base_reset_job(current_global_jobdata);

    /*
     * Adjust the application context information
     */
    for(i_proc = 0; i_proc < opal_pointer_array_get_size(&(cur_datum->migrating_procs)); ++i_proc) {
        proc = (orte_proc_t*)opal_pointer_array_get_item(&(cur_datum->migrating_procs), i_proc);
        if( NULL == proc ) {
            continue;
        }

        if( ORTE_SUCCESS != (ret = orte_errmgr_base_update_app_context_for_cr_recovery(current_global_jobdata,
                                                                                       proc,
                                                                                       &(cur_datum->ss_snapshot->local_snapshots))) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                             "\tAdjusted: \"%s\" [0x%d] [%s]\n",
                             ORTE_NAME_PRINT(&proc->name), proc->state, proc->node->name));
    }

    ERRMGR_CRMIG_SET_TIMER(ERRMGR_CRMIG_TIMER_RESETUP);

    /*
     * Restart the job
     * - spawn function will remap and launch the replacement proc(s)
     */
    opal_output_verbose(10, mca_errmgr_hnp_component.super.output_handle,
                        "errmgr:hnp(crmig):migrate() ------- Respawning migrating processes in job %s -------",
                        ORTE_JOBID_PRINT(current_global_jobdata->jobid));

    orte_plm.spawn(current_global_jobdata);


    opal_output_verbose(10, mca_errmgr_hnp_component.super.output_handle,
                        "errmgr:hnp(crmig):migrate() ------- Waiting for restart -------");

    migrating_restarted = false;
    while( !migrating_restarted ) {
        opal_progress();
        check_if_restarted(&(cur_datum->migrating_procs));
    }

    ERRMGR_CRMIG_SET_TIMER(ERRMGR_CRMIG_TIMER_RESTART);

    /*
     * Finish the checkpoint
     */
    opal_output_verbose(10, mca_errmgr_hnp_component.super.output_handle,
                        "errmgr:hnp(crmig):migrate() ------- Reconnecting processes in job %s -------",
                        ORTE_JOBID_PRINT(current_global_jobdata->jobid));

    if( ORTE_SUCCESS != (ret = orte_snapc.end_ckpt(cur_datum)) ) {
        opal_output(0, "errmgr:hnp(crmig):migrate() Error: Unable to end the checkpoint.");
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * All done
     */
    opal_output_verbose(10, mca_errmgr_hnp_component.super.output_handle,
                        "errmgr:hnp(crmig):migrate() ------- Finished migrating processes in job %s -------",
                        ORTE_JOBID_PRINT(current_global_jobdata->jobid));

    OBJ_RELEASE(cur_datum);

    current_migration_status = ORTE_ERRMGR_MIGRATE_STATE_FINISH;
    if( ORTE_SUCCESS != (ret = orte_errmgr_base_migrate_update(current_migration_status)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    ERRMGR_CRMIG_SET_TIMER(ERRMGR_CRMIG_TIMER_FINISH);
    ERRMGR_CRMIG_DISPLAY_ALL_TIMERS();

 cleanup:
    migrating_underway   = false;
    migrating_terminated = false;
    migrating_restarted  = false;

    if( NULL != err_str_procs ) {
        free(err_str_procs);
        err_str_procs = NULL;
    }

    if( NULL != err_str_nodes ) {
        free(err_str_nodes);
        err_str_nodes = NULL;
    }

    return exit_status;
}

static bool check_if_duplicate_proc(orte_proc_t *proc, opal_pointer_array_t *migrating_procs)
{
    orte_std_cntr_t i_proc;
    orte_proc_t *loc_proc = NULL;

    for(i_proc = 0; i_proc < opal_pointer_array_get_size(migrating_procs); ++i_proc) {
        loc_proc = (orte_proc_t*)opal_pointer_array_get_item(migrating_procs, i_proc);
        if( NULL == loc_proc ) {
            continue;
        }
        if( loc_proc->name.vpid == proc->name.vpid ) {
            return true;
        }
    }

    return false;
}

static int check_if_terminated(opal_pointer_array_t *migrating_procs)
{
    orte_std_cntr_t i_proc;
    orte_proc_t *proc = NULL;
    bool is_done;

    is_done = true;
    for(i_proc = 0; i_proc < opal_pointer_array_get_size(migrating_procs); ++i_proc) {
        proc = (orte_proc_t*)opal_pointer_array_get_item(migrating_procs, i_proc);
        if( NULL == proc ) {
            continue;
        }

        if( !(ORTE_PROC_STATE_KILLED_BY_CMD & proc->state) ) {
            is_done = false;
            break;
        }
    }

    if( is_done ) {
        migrating_terminated = true;
    }
    else {
        OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                             "\t Still waiting for termination: \"%s\" [0x%x] != [0x%x]\n",
                             ORTE_NAME_PRINT(&proc->name), proc->state, ORTE_PROC_STATE_KILLED_BY_CMD));
    }

    return ORTE_SUCCESS;
}

static int check_if_restarted(opal_pointer_array_t *migrating_procs)
{
    orte_std_cntr_t i_proc;
    orte_proc_t *proc = NULL;
    bool is_done;

    is_done = true;
    for(i_proc = 0; i_proc < opal_pointer_array_get_size(migrating_procs); ++i_proc) {
        proc = (orte_proc_t*)opal_pointer_array_get_item(migrating_procs, i_proc);
        if( NULL == proc ) {
            continue;
        }

        /* proc->state != ORTE_PROC_STATE_LAUNCHED */
        if( !(ORTE_PROC_STATE_RUNNING & proc->state) ) {
            is_done = false;
            break;
        }
    }

    if( is_done ) {
        migrating_restarted = true;
    }
    else {
        OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                             "\tStill waiting for restart: \"%s\" [0x%x] != [0x%x]\n",
                             ORTE_NAME_PRINT(&proc->name), proc->state, ORTE_PROC_STATE_RUNNING));
    }

    return ORTE_SUCCESS;
}

static void errmgr_crmig_process_fault_app(orte_job_t *jdata,
                                           orte_process_name_t *proc,
                                           orte_proc_state_t state)
{
    OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                         "errmgr:hnp(crmig):process_fault_app() "
                         "------- Application fault reported! proc %s (0x%x) "
                         "- %s",
                         ORTE_NAME_PRINT(proc),
                         state,
                         (migrating_underway ? "Migrating" : "Not Migrating") ));

    return;
}

static void errmgr_crmig_process_fault_daemon(orte_job_t *jdata,
                                              orte_process_name_t *proc,
                                              orte_proc_state_t state)
{
    OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                         "errmgr:hnp(crmig):process_fault_daemon() "
                         "------- Daemon fault reported! proc %s (0x%x) "
                         "- %s",
                         ORTE_NAME_PRINT(proc),
                         state,
                         (migrating_underway ? "Migrating" : "Not Migrating") ));

    /*
     * Failed communication can be ignored for the most part.
     * Make sure to remove the route
     * JJH: Check to make sure this is not a new daemon loss. 
     */
    if( ORTE_PROC_STATE_COMM_FAILED == state ) {
        OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                             "errmgr:hnp(crmig):process_fault_daemon() "
                             "------- Daemon fault reported! proc %s (0x%x) "
                             "- Communication failure, keep going",
                             ORTE_NAME_PRINT(proc),
                             state ));
    }

    return;
}

static int check_and_pre_map(opal_list_t *off_procs,
                             opal_list_t *off_nodes,
                             orte_snapc_base_quiesce_t *cur_datum)
{
    /*
     * Check the 'off_procs' list for processes that should not be migrated
     */

    /*
     * Check the 'current_onto_mapping_exclusive' for processes that are moving
     * 'near/with' other processes that are also moving. Be sure to watch out
     * for circular deadlock.
     */

    /*
     * Use the 'pre_map_fixed_node' structure to fix this process' mapping.
     */

    return ORTE_SUCCESS;
}

static void display_request(opal_list_t *off_procs,
                            opal_list_t *off_nodes,
                            orte_snapc_base_quiesce_t *cur_datum)
{
    orte_std_cntr_t i_node;
    orte_std_cntr_t i_proc;
    orte_node_t *node = NULL;
    orte_proc_t *proc = NULL;
    bool found = false;
    char * status_str = NULL;
    char * tmp_str = NULL;
    orte_errmgr_predicted_proc_t *off_proc = NULL;
    orte_errmgr_predicted_node_t *off_node = NULL;
    orte_errmgr_predicted_map_t  *onto_map = NULL;
    opal_list_item_t *item = NULL;

    /*
     * Display all requested processes to migrate
     */
    OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                         "errmgr:hnp(crmig):migrate() Requested Processes to migrate: (%d procs)\n",
                         (int) opal_list_get_size(off_procs) ));
    for(item  = opal_list_get_first(off_procs);
        item != opal_list_get_end(off_procs);
        item  = opal_list_get_next(item) ) {
        off_proc = (orte_errmgr_predicted_proc_t*)item;

        /*
         * Find the process in the job structure
         */
        found = false;
        for(i_proc = 0; i_proc < opal_pointer_array_get_size(current_global_jobdata->procs); ++i_proc) {
            proc = (orte_proc_t*)opal_pointer_array_get_item(current_global_jobdata->procs, i_proc);
            if( NULL == proc ) {
                continue;
            }

            if( proc->name.vpid == off_proc->proc_name.vpid) {
                found = true;
                break;
            }
        }
        OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                             "\t%s (Rank %3d) on node %s\n",
                             ORTE_NAME_PRINT(&proc->name), (int)off_proc->proc_name.vpid, proc->node->name));
    }

    /*
     * Display Off Nodes
     */
    OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                         "errmgr:hnp(crmig):migrate() Requested Nodes to migration: (%d nodes)\n",
                         (int)opal_list_get_size(off_nodes) ));

    for(item  = opal_list_get_first(off_nodes);
        item != opal_list_get_end(off_nodes);
        item  = opal_list_get_next(item) ) {
        off_node = (orte_errmgr_predicted_node_t*)item;

        for(i_node = 0; i_node < opal_pointer_array_get_size(current_global_jobdata->map->nodes); ++i_node) {
            node = (orte_node_t*)opal_pointer_array_get_item(current_global_jobdata->map->nodes, i_node);
            if( NULL == node ) {
                continue;
            }

            found = false;
            if( 0 == strncmp(node->name, off_node->node_name, strlen(off_node->node_name)) ) {
                found = true;
                break;
            }
        }
        if( found ) {
            OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                                 "\t\"%s\" \t%d\n",
                                 node->name, node->num_procs));
            for(i_proc = 0; i_proc < opal_pointer_array_get_size(node->procs); ++i_proc) {
                proc = (orte_proc_t*)opal_pointer_array_get_item(node->procs, i_proc);
                if( NULL == proc ) {
                    continue;
                }

                OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                                     "\t\t\"%s\" [0x%x]\n",
                                     ORTE_NAME_PRINT(&proc->name), proc->state));
            }
        }
    }

    /*
     * Suggested onto nodes
     */
    OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                         "errmgr:hnp(crmig):migrate() Suggested nodes to migration onto: (%d nodes)\n",
                         (int)opal_list_get_size(current_onto_mapping_general) ));
    for(item  = opal_list_get_first(current_onto_mapping_general);
        item != opal_list_get_end(current_onto_mapping_general);
        item  = opal_list_get_next(item) ) {
        onto_map = (orte_errmgr_predicted_map_t*) item;
        OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                             "\t\"%s\"\n",
                             onto_map->map_node_name));
    }

    OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                         "errmgr:hnp(crmig):migrate() Suggested nodes to migration onto (exclusive): (%d nodes)\n",
                         (int)opal_list_get_size(current_onto_mapping_exclusive) ));
    for(item  = opal_list_get_first(current_onto_mapping_exclusive);
        item != opal_list_get_end(current_onto_mapping_exclusive);
        item  = opal_list_get_next(item) ) {
        onto_map = (orte_errmgr_predicted_map_t*) item;
        OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                             "\t%d\t(%c)\t\"%s\"\n",
                             onto_map->proc_name.vpid,
                             (onto_map->off_current_node ? 'T' : 'F'),
                             onto_map->map_node_name));
    }

    /*
     * Display all processes scheduled to migrate
     */
    OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                         "errmgr:hnp(crmig):migrate() All Migrating Processes: (%d procs)\n",
                         cur_datum->num_migrating));
    for(i_proc = 0; i_proc < opal_pointer_array_get_size(&(cur_datum->migrating_procs)); ++i_proc) {
        proc = (orte_proc_t*)opal_pointer_array_get_item(&(cur_datum->migrating_procs), i_proc);
        if( NULL == proc ) {
            continue;
        }

        OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                             "\t\"%s\" [0x%x] [%s]\n",
                             ORTE_NAME_PRINT(&proc->name), proc->state, proc->node->name));

        if( NULL == status_str ) {
            asprintf(&status_str, "\t%s Rank %d on Node %s\n",
                     ORTE_NAME_PRINT(&proc->name),
                     (int)proc->name.vpid,
                     proc->node->name);
        } else {
            tmp_str = strdup(status_str);
            free(status_str);
            status_str = NULL;
            asprintf(&status_str, "%s\t%s Rank %d on Node %s\n",
                     tmp_str,
                     ORTE_NAME_PRINT(&proc->name),
                     (int)proc->name.vpid,
                     proc->node->name);
        }
    }

    opal_show_help("help-orte-errmgr-hnp.txt", "crmig_migrating_job", true,
                   status_str);

    if( NULL != tmp_str ) {
        free(tmp_str);
        tmp_str = NULL;
    }

    if( NULL != status_str ) {
        free(status_str);
        status_str = NULL;
    }

    return;
}

/************************
 * Timing
 ************************/
static void errmgr_crmig_set_time(int idx)
{
    if(idx < ERRMGR_CRMIG_TIMER_MAX ) {
        if( timer_start[idx] <= 0.0 ) {
            timer_start[idx] = errmgr_crmig_get_time();
        }
    }
}

static void errmgr_crmig_display_all_timers(void)
{
    double diff = 0.0;
    char * label = NULL;

    opal_output(0, "Process Migration Timing: ******************** Summary Begin\n");

    /********** Structure Setup **********/
    label = strdup("Setup");
    diff = timer_start[ERRMGR_CRMIG_TIMER_SETUP]   - timer_start[ERRMGR_CRMIG_TIMER_START];
    errmgr_crmig_display_indv_timer_core(diff, label);
    free(label);

    /********** Checkpoint **********/
    label = strdup("Checkpoint");
    diff = timer_start[ERRMGR_CRMIG_TIMER_CKPT]   - timer_start[ERRMGR_CRMIG_TIMER_SETUP];
    errmgr_crmig_display_indv_timer_core(diff, label);
    free(label);

    /********** Termination **********/
    label = strdup("Terminate");
    diff = timer_start[ERRMGR_CRMIG_TIMER_TERM]   - timer_start[ERRMGR_CRMIG_TIMER_CKPT];
    errmgr_crmig_display_indv_timer_core(diff, label);
    free(label);

    /********** Setup new job **********/
    label = strdup("Setup Relaunch");
    diff = timer_start[ERRMGR_CRMIG_TIMER_RESETUP]   - timer_start[ERRMGR_CRMIG_TIMER_TERM];
    errmgr_crmig_display_indv_timer_core(diff, label);
    free(label);

    /********** Restart **********/
    label = strdup("Restart");
    diff = timer_start[ERRMGR_CRMIG_TIMER_RESTART]   - timer_start[ERRMGR_CRMIG_TIMER_RESETUP];
    errmgr_crmig_display_indv_timer_core(diff, label);
    free(label);

    /********** Finish **********/
    label = strdup("Finalize");
    diff = timer_start[ERRMGR_CRMIG_TIMER_FINISH]   - timer_start[ERRMGR_CRMIG_TIMER_RESTART];
    errmgr_crmig_display_indv_timer_core(diff, label);
    free(label);

    opal_output(0, "Process Migration Timing: ******************** Summary End\n");
}

static void errmgr_crmig_clear_timers(void)
{
    int i;
    for(i = 0; i < ERRMGR_CRMIG_TIMER_MAX; ++i) {
        timer_start[i] = 0.0;
    }
}

static double errmgr_crmig_get_time(void)
{
    double wtime;

#if OPAL_TIMER_USEC_NATIVE
    wtime = (double)opal_timer_base_get_usec() / 1000000.0;
#else
    struct timeval tv;
    gettimeofday(&tv, NULL);
    wtime = tv.tv_sec;
    wtime += (double)tv.tv_usec / 1000000.0;
#endif

    return wtime;
}

static void errmgr_crmig_display_indv_timer_core(double diff, char *str)
{
    double total = 0;
    double perc  = 0;

    total = timer_start[ERRMGR_CRMIG_TIMER_MAX-1] - timer_start[ERRMGR_CRMIG_TIMER_START];
    perc = (diff/total) * 100;

    opal_output(0,
                "errmgr_crmig: timing: %-20s = %10.2f s\t%10.2f s\t%6.2f\n",
                str,
                diff,
                total,
                perc);
    return;
}

#endif /* OPAL_ENABLE_FT_CR */
