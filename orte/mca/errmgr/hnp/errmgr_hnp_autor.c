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
#include "opal/mca/event/event.h"

#include "orte/util/error_strings.h"
#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/orte_globals.h"
#include "opal/dss/dss.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/iof/iof.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/base.h"
#include "orte/mca/plm/base/plm_private.h"
#include "orte/mca/filem/filem.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/runtime/orte_wait.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"
#include "orte/mca/sstore/sstore.h"
#include "orte/mca/sstore/base/base.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/errmgr/base/errmgr_private.h"

#include "errmgr_hnp.h"

#include MCA_timer_IMPLEMENTATION_HEADER

#if OPAL_ENABLE_FT_CR
/************************
 * Work Pool structures
 ************************/
struct errmgr_autor_wp_item_t {
    /** List super object */
    opal_list_item_t super;

    /** ORTE Process name */
    orte_process_name_t name;

    /** State that was passed with it */
    orte_proc_state_t state;
};
typedef struct errmgr_autor_wp_item_t errmgr_autor_wp_item_t;

OBJ_CLASS_DECLARATION(errmgr_autor_wp_item_t);

void errmgr_autor_wp_item_construct(errmgr_autor_wp_item_t *wp);
void errmgr_autor_wp_item_destruct(errmgr_autor_wp_item_t *wp);

OBJ_CLASS_INSTANCE(errmgr_autor_wp_item_t,
                   opal_list_item_t,
                   errmgr_autor_wp_item_construct,
                   errmgr_autor_wp_item_destruct);

/************************************
 * Locally Global vars & functions :)
 ************************************/
static orte_jobid_t current_global_jobid = ORTE_JOBID_INVALID;
static orte_job_t  *current_global_jobdata = NULL;

static bool autor_mask_faults = false;

static opal_list_t *procs_pending_recovery = NULL;
static bool autor_timer_active = false;
static opal_event_t *autor_timer_event = NULL;

static void errmgr_autor_recover_processes(int fd, short event, void *cbdata);
static int autor_set_current_job_info(orte_job_t *given_jdata, orte_process_name_t *proc_name);

static int display_procs(void );
static int autor_procs_sort_compare_fn(opal_list_item_t **a,
                                       opal_list_item_t **b);

static int orte_errmgr_hnp_autor_global_process_fault(orte_job_t *jdata,
                                                        orte_process_name_t *proc_name,
                                                        orte_proc_state_t state);
static void errmgr_autor_process_fault_app(orte_job_t *jdata,
                                           orte_process_name_t *proc,
                                           orte_proc_state_t state);
static void errmgr_autor_process_fault_daemon(orte_job_t *jdata,
                                              orte_process_name_t *proc,
                                              orte_proc_state_t state);

static int check_if_terminated(opal_pointer_array_t *procs);
static int check_if_restarted(opal_pointer_array_t *procs);

/*
 * Timer stuff
 */
static void errmgr_autor_set_time(int idx);
static void errmgr_autor_display_all_timers(void);
static void errmgr_autor_clear_timers(void);

static double errmgr_autor_get_time(void);
static void errmgr_autor_display_indv_timer_core(double diff, char *str);
static double timer_start[OPAL_CR_TIMER_MAX];

#define ERRMGR_AUTOR_TIMER_START    0
#define ERRMGR_AUTOR_TIMER_SETUP    1
#define ERRMGR_AUTOR_TIMER_TERM     2
#define ERRMGR_AUTOR_TIMER_RESETUP  3
#define ERRMGR_AUTOR_TIMER_RESTART  4
#define ERRMGR_AUTOR_TIMER_FINISH   5
#define ERRMGR_AUTOR_TIMER_MAX      6

#define ERRMGR_AUTOR_CLEAR_TIMERS()                                     \
    {                                                                   \
        if(OPAL_UNLIKELY(mca_errmgr_hnp_component.autor_timing_enabled > 0)) { \
            errmgr_autor_clear_timers();                                \
        }                                                               \
    }

#define ERRMGR_AUTOR_SET_TIMER(idx)                                     \
    {                                                                   \
        if(OPAL_UNLIKELY(mca_errmgr_hnp_component.autor_timing_enabled > 0)) { \
            errmgr_autor_set_time(idx);                                 \
        }                                                               \
    }

#define ERRMGR_AUTOR_DISPLAY_ALL_TIMERS()                               \
    {                                                                   \
        if(OPAL_UNLIKELY(mca_errmgr_hnp_component.autor_timing_enabled > 0)) { \
            errmgr_autor_display_all_timers();                          \
        }                                                               \
    }

/************************
 * Function Definitions: Global
 ************************/
int orte_errmgr_hnp_autor_global_module_init(void)
{
    opal_output_verbose(10, mca_errmgr_hnp_component.super.output_handle,
                        "errmgr:hnp(autor):init()");

    procs_pending_recovery = OBJ_NEW(opal_list_t);

    current_global_jobid   = ORTE_JOBID_INVALID;
    current_global_jobdata = NULL;

    ERRMGR_AUTOR_CLEAR_TIMERS();

    return ORTE_SUCCESS;
}

int orte_errmgr_hnp_autor_global_module_finalize(void)
{
    opal_output_verbose(10, mca_errmgr_hnp_component.super.output_handle,
                        "errmgr:hnp(autor):finalize()");

    if( NULL != procs_pending_recovery ) {
        OBJ_RELEASE(procs_pending_recovery);
        procs_pending_recovery = NULL;
    }
    if( NULL != autor_timer_event ) {
        free(autor_timer_event);
    }

    current_global_jobid   = ORTE_JOBID_INVALID;
    current_global_jobdata = NULL;

    ERRMGR_AUTOR_CLEAR_TIMERS();

    return ORTE_SUCCESS;
}

static int autor_set_current_job_info(orte_job_t *given_jdata, orte_process_name_t *proc_name)
{
    orte_job_t *jdata = NULL;
    int i;

    /*
     * If we already figured it out, then just move ahead
     */
    if( NULL != current_global_jobdata ) {
        if( given_jdata->jobid != ORTE_PROC_MY_NAME->jobid &&
            given_jdata->jobid != current_global_jobdata->jobid ) {
            current_global_jobdata = given_jdata;
            current_global_jobid   = given_jdata->jobid;
        }
        return ORTE_SUCCESS;
    }

    /*
     * If this references the application, and not the daemons
     */
    if( given_jdata->jobid != ORTE_PROC_MY_NAME->jobid ) {
        current_global_jobdata = given_jdata;
        current_global_jobid   = given_jdata->jobid;
        return ORTE_SUCCESS;
    }

    /*
     * Otherwise iterate through the job structure and find the first job.
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
        opal_output(0, "errmgr:hnp(autor):process_fault(): Global) Error: Cannot find the jdata for the current job.");
        return ORTE_ERROR;
    }

    return ORTE_SUCCESS;
}

int orte_errmgr_hnp_autor_global_update_state(orte_jobid_t job,
                                                orte_job_state_t jobstate,
                                                orte_process_name_t *proc_name,
                                                orte_proc_state_t state,
                                                pid_t pid,
                                                orte_exit_code_t exit_code)
{
    orte_proc_t *loc_proc = NULL;
    orte_job_t *jdata = NULL;
    int ret = ORTE_SUCCESS, exit_status = ORTE_SUCCESS;
    int32_t i;

    /*
     * if orte is trying to shutdown, just let it
     */
    if( mca_errmgr_hnp_component.term_in_progress ) {
        return ORTE_SUCCESS;
    }

    if( NULL != proc_name &&
        OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, ORTE_PROC_MY_NAME, proc_name) ) {
        OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base.output,
                             "%s errmgr:hnp(autor): Update reported on self (%s), state %s. Skip...",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(proc_name),
                             orte_proc_state_to_str(state) ));
        return ORTE_SUCCESS;
    }

    /* get the job data object for this process */
    if (NULL == (jdata = orte_get_job_data_object(job))) {
        ret = ORTE_ERROR;
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * If this is a tool, ignore
     */
    if( jdata->num_apps == 0 &&
        OPAL_EQUAL != orte_util_compare_name_fields(ORTE_NS_CMP_JOBID, ORTE_PROC_MY_NAME, proc_name) ) {
        OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base.output,
                             "%s errmgr:hnp(autor): An external tool disconnected. Ignore...",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        exit_status = ORTE_SUCCESS;
        goto cleanup;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base.output,
                         "%s errmgr:hnp(autor): job %s reported state %s"
                         " for proc %s state %s exit_code %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(job),
                         orte_job_state_to_str(jobstate),
                         (NULL == proc_name) ? "NULL" : ORTE_NAME_PRINT(proc_name),
                         orte_proc_state_to_str(state), exit_code));

    if( ORTE_JOB_STATE_RESTART == jobstate ) {
        for(i = 0; i < jdata->procs->size; ++i) {
            if (NULL == (loc_proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, i))) {
                continue;
            }
            break;
        }

        if( ORTE_SUCCESS != (ret = orte_errmgr_hnp_autor_global_process_fault(jdata, &(loc_proc->name), state)) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }
    else if( ORTE_PROC_STATE_ABORTED_BY_SIG == state ||
             ORTE_PROC_STATE_COMM_FAILED    == state ) {
        if( ORTE_SUCCESS != (ret = orte_errmgr_hnp_autor_global_process_fault(jdata, proc_name, state)) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }
    else if( ORTE_PROC_STATE_KILLED_BY_CMD == state ) {
        if( autor_mask_faults ) {
            mca_errmgr_hnp_component.ignore_current_update = true;
            orte_errmgr_hnp_update_proc(jdata, proc_name, state, 0, exit_code);
        }
    }

 cleanup:
    return ret;
}

static int orte_errmgr_hnp_autor_global_process_fault(orte_job_t *jdata,
                                                        orte_process_name_t *proc_name,
                                                        orte_proc_state_t state)
{
    int ret;

    /*
     * Recover from the process failure by relaunching.
     */
    if( ORTE_SUCCESS != (ret = autor_set_current_job_info(jdata, proc_name)) ) {
        ORTE_ERROR_LOG(ret);
        return ORTE_SUCCESS; /* JJH: Do this for now. Need to fix the flag for normal shutdown */
        /*return ret;*/
    }

    current_global_jobdata->controls |= ORTE_JOB_CONTROL_RECOVERABLE;

    if( proc_name->jobid == ORTE_PROC_MY_NAME->jobid ) {
        errmgr_autor_process_fault_daemon(jdata, proc_name, state);
    } else {
        orte_errmgr_hnp_update_proc(jdata, proc_name, state, 0, 0);
        errmgr_autor_process_fault_app(jdata, proc_name, state);
    }

    return ORTE_SUCCESS;
}

int orte_errmgr_hnp_autor_global_suggest_map_targets(orte_proc_t *proc,
                                                       orte_node_t *oldnode,
                                                       opal_list_t *node_list)
{
    opal_list_item_t *item = NULL;
    errmgr_autor_wp_item_t *wp_item = NULL;
    orte_node_t *node = NULL;
    bool found = false;
    int num_removed = 0, num_to_remove;

    if( NULL == current_global_jobdata ) {
        return ORTE_SUCCESS;
    }

    /* JJH Nasty Hack */
    num_to_remove = current_global_jobdata->num_procs / 2;
    num_to_remove += 1;

    /*
     * Find this process in the known failures list
     */
    found = false;
    if( mca_errmgr_hnp_component.autor_skip_oldnode ) {
        for(item  = opal_list_get_first(procs_pending_recovery);
            item != opal_list_get_end(procs_pending_recovery);
            item  = opal_list_get_next(item) ) {
            wp_item = (errmgr_autor_wp_item_t*)item;

            if( wp_item->name.vpid  == proc->name.vpid &&
                wp_item->name.jobid == proc->name.jobid ) {
                found = true;
                break;
            }
        }
    }

    OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                         "%s errmgr:hnp(autor): suggest_map() "
                         "Process remapping: %s oldnode %s, %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&proc->name),
                         oldnode->name,
                         (found ? "Failed Proc." : "Good Proc.") ));

    /*
     * If not a failed process, then return it to the oldnode
     * If failed process, do not place it back on the same node
     */
    num_removed = 0;
    for( item  = opal_list_get_first(node_list);
         item != opal_list_get_end(node_list);
         item  = opal_list_get_next(item) ) {
        node = (orte_node_t*)item;
        if( found ) {
            if( num_removed >= num_to_remove ) {
                break;
            }
            /* JJH Nasty Hack */
#if 0
            /* Remove oldnode (if more than one node) */
            if( node == oldnode && 1 < opal_list_get_size(node_list) ) {
                opal_output(0, "JJH Remove Node (%s)", node->name);
                opal_list_remove_item(node_list, item);
                OBJ_RELEASE(item);
            }
#else
            if( 1 < opal_list_get_size(node_list) ) {
                opal_list_remove_item(node_list, item);
                OBJ_RELEASE(item);
            }
#endif
            num_removed++;
        } else {
            /* Stay on same node */
            if( node != oldnode ) {
                opal_list_remove_item(node_list, item);
                OBJ_RELEASE(item);
            }
        }
    }

    return ORTE_SUCCESS;
}

int orte_errmgr_hnp_autor_global_ft_event(int state)
{
    return ORTE_SUCCESS;
}


/*****************
 * Local Functions
 *****************/
static void errmgr_autor_process_fault_app(orte_job_t *jdata,
                                           orte_process_name_t *proc,
                                           orte_proc_state_t state)
{
    errmgr_autor_wp_item_t *wp_item = NULL;
    struct timeval soon;

    OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                         "%s errmgr:hnp(autor): process_fault() "
                         "Process fault! proc %s (0x%x)",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         state));

    if( !orte_sstore_base_is_checkpoint_available ) {
        OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                             "%s errmgr:hnp(autor): process_fault() "
                             "No checkpoints are available for this job! Cannot Automaticly Recover!",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME) ));
        opal_show_help("help-orte-errmgr-hnp.txt", "autor_failed_to_recover_proc", true,
                       ORTE_NAME_PRINT(proc), proc->vpid);
        return;
    }

    mca_errmgr_hnp_component.ignore_current_update = true;

    /*
     * If we are already in the shutdown stage of the recovery, then just skip it
     */
    if( autor_mask_faults ) {
        OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                             "%s errmgr:hnp(autor):process_fault() "
                             "Currently recovering the job. Failure masked!",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        return;
    }

    /*
     * Append this process to the list to process
     */
    wp_item = OBJ_NEW(errmgr_autor_wp_item_t);
    wp_item->name.jobid = proc->jobid;
    wp_item->name.vpid = proc->vpid;
    wp_item->state = state;

    opal_list_append(procs_pending_recovery, &(wp_item->super));

    /*
     * Activate the timer, if it is not already setup
     */
    if( !autor_timer_active ) {
        autor_timer_active = true;

        opal_event_evtimer_set(opal_event_base, autor_timer_event, errmgr_autor_recover_processes, NULL);
        soon.tv_sec  = mca_errmgr_hnp_component.autor_recovery_delay;
        soon.tv_usec = 0;
        opal_event_evtimer_add(autor_timer_event, &soon);
    }

    return;
}

static void errmgr_autor_process_fault_daemon(orte_job_t *jdata,
                                              orte_process_name_t *proc,
                                              orte_proc_state_t state)
{
    orte_proc_t *loc_proc = NULL, *child_proc = NULL;
    orte_std_cntr_t i_proc;
    int32_t i;

    OPAL_OUTPUT_VERBOSE((15, mca_errmgr_hnp_component.super.output_handle,
                         "%s errmgr:hnp(autor): process_fault_daemon() "
                         "------- Daemon fault reported! proc %s (0x%x)",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         state));

    /*
     * Set the process state in the job data structure
     */
    for(i = 0; i < jdata->procs->size; ++i) {
        if (NULL == (loc_proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, i))) {
            continue;
        }

        if( loc_proc->name.vpid != proc->vpid) {
            continue;
        }

        loc_proc->state = state;

        break;
    }

    /*
     * Remove the route to this process
     */
    orte_routed.delete_route(proc);

    /*
     * If the aborted daemon had active processes on its node, then we should
     * make sure to signal that all the children are gone.
     */
    if( loc_proc->node->num_procs > 0 ) {
        OPAL_OUTPUT_VERBOSE((10, orte_errmgr_base.output,
                             "%s errmgr:base: stabalize_runtime() "
                             "------- Daemon lost with the following processes",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

        for(i_proc = 0; i_proc < opal_pointer_array_get_size(loc_proc->node->procs); ++i_proc) {
            child_proc = (orte_proc_t*)opal_pointer_array_get_item(loc_proc->node->procs, i_proc);
            if( NULL == child_proc ) {
                continue;
            }

            OPAL_OUTPUT_VERBOSE((10, orte_errmgr_base.output,
                                 "%s errmgr:base: stabalize_runtime() "
                                 "\t %s [0x%x]",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&child_proc->name),
                                 child_proc->state));

            if( child_proc->last_errmgr_state < child_proc->state ) {
                child_proc->last_errmgr_state = child_proc->state;
                orte_errmgr.update_state(child_proc->name.jobid, ORTE_JOB_STATE_COMM_FAILED, 
                                         &(child_proc->name), ORTE_PROC_STATE_COMM_FAILED,
                                         0, 1); 
            }
        }
    } else {
        /* This daemon had no children, so just mask the failure */
        mca_errmgr_hnp_component.ignore_current_update = true;
    }

    /*
     * Record the dead daemon
     */
    orte_errmgr_hnp_record_dead_daemon(jdata, proc->vpid, state, 0);

    return;
}

void errmgr_autor_wp_item_construct(errmgr_autor_wp_item_t *wp)
{
    wp->name.jobid = ORTE_JOBID_INVALID;
    wp->name.vpid  = ORTE_VPID_INVALID;

    wp->state = 0;
}

void errmgr_autor_wp_item_destruct(errmgr_autor_wp_item_t *wp)
{
    wp->name.jobid = ORTE_JOBID_INVALID;
    wp->name.vpid  = ORTE_VPID_INVALID;

    wp->state = 0;
}

static int display_procs(void )
{
    opal_list_item_t *item = NULL;
    errmgr_autor_wp_item_t *wp_item = NULL;
    char *proc_str = NULL;
    char *tmp_str = NULL;

    for(item  = opal_list_get_first(procs_pending_recovery);
        item != opal_list_get_end(procs_pending_recovery);
        item  = opal_list_get_next(item) ) {
        wp_item = (errmgr_autor_wp_item_t*)item;

        if( NULL == proc_str ) {
            asprintf(&proc_str, "\t%s Rank %d\n",
                     ORTE_NAME_PRINT(&(wp_item->name)),
                     (int)wp_item->name.vpid);
        } else {
            tmp_str = strdup(proc_str);
            free(proc_str);
            proc_str = NULL;
            asprintf(&proc_str, "%s\t%s Rank %d\n",
                     tmp_str,
                     ORTE_NAME_PRINT(&(wp_item->name)),
                     (int)wp_item->name.vpid);
        }
    }

    opal_show_help("help-orte-errmgr-hnp.txt", "autor_recovering_job", true,
                   proc_str);

    if( NULL != tmp_str ) {
        free(tmp_str);
        tmp_str = NULL;
    }

    if( NULL != proc_str ) {
        free(proc_str);
        proc_str = NULL;
    }

    return ORTE_SUCCESS;
}

static int autor_procs_sort_compare_fn(opal_list_item_t **a,
                                       opal_list_item_t **b)
{
    errmgr_autor_wp_item_t *wp_a, *wp_b;

    wp_a = (errmgr_autor_wp_item_t*)(*a);
    wp_b = (errmgr_autor_wp_item_t*)(*b);

    if( wp_a->name.vpid > wp_b->name.vpid ) {
        return 1;
    }
    else if( wp_a->name.vpid == wp_b->name.vpid ) {
        return 0;
    }
    else {
        return -1;
    }
}

static void errmgr_autor_recover_processes(int fd, short event, void *cbdata)
{
    int ret, exit_status = ORTE_SUCCESS;
    opal_list_item_t *item = NULL;
    errmgr_autor_wp_item_t *wp_item = NULL;
    orte_std_cntr_t i_proc;
    orte_proc_t *proc = NULL;
    orte_sstore_base_global_snapshot_info_t *snapshot = NULL;
    char * tmp_str = NULL;

    autor_mask_faults = true;
    ERRMGR_AUTOR_CLEAR_TIMERS();
    ERRMGR_AUTOR_SET_TIMER(ERRMGR_AUTOR_TIMER_START);

    /*
     * Display the processes that are to be recovered
     */
    OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                         "%s errmgr:hnp(autor):recover() "
                         "------- Display known failed processes in the job %s -------",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(current_global_jobdata->jobid)));

    opal_list_sort(procs_pending_recovery, autor_procs_sort_compare_fn);
    display_procs();

    /*
     * Find the latest checkpoint
     */
    OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                         "%s errmgr:hnp(autor):recover() "
                         "------- Find the latest checkpoint for the job %s -------",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(current_global_jobdata->jobid)));

    snapshot = OBJ_NEW(orte_sstore_base_global_snapshot_info_t);
    if( ORTE_SUCCESS != (ret = orte_sstore.request_global_snapshot_data(&orte_sstore_handle_last_stable, snapshot)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    ERRMGR_AUTOR_SET_TIMER(ERRMGR_AUTOR_TIMER_SETUP);

    /*
     * Safely terminate the entire job
     */
    opal_output_verbose(10, mca_errmgr_hnp_component.super.output_handle,
                        "errmgr:hnp(autor):recover() "
                        "------- Safely terminate the job %s -------",
                        ORTE_JOBID_PRINT(current_global_jobdata->jobid));

    for(i_proc = 0; i_proc < opal_pointer_array_get_size(current_global_jobdata->procs); ++i_proc) {
        proc = (orte_proc_t*)opal_pointer_array_get_item(current_global_jobdata->procs, i_proc);
        if( NULL == proc ) {
            continue;
        }
        if( proc->state < ORTE_PROC_STATE_UNTERMINATED ) {
            proc->state = ORTE_PROC_STATE_MIGRATING;
        }
        if( current_global_jobdata->stdin_target == proc->name.vpid ) {
            orte_iof.close(&(proc->name), ORTE_IOF_STDIN);
        }
    }

    orte_plm.terminate_procs(current_global_jobdata->procs);

    /*
     * Wait for the job to terminate all processes
     */
    while(!check_if_terminated(current_global_jobdata->procs) ) {
        opal_progress();
    }

    ERRMGR_AUTOR_SET_TIMER(ERRMGR_AUTOR_TIMER_TERM);

    opal_output_verbose(10, mca_errmgr_hnp_component.super.output_handle,
                        "errmgr:hnp(autor):recover() "
                        "------- Done waiting for termination of job %s -------",
                        ORTE_JOBID_PRINT(current_global_jobdata->jobid));
    current_global_jobdata->num_terminated = current_global_jobdata->num_procs;
    orte_plm_base_reset_job(current_global_jobdata);

    /*
     * Construct the app contexts to restart
     */
    OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                         "%s errmgr:hnp(autor):recover() "
                         "------- Rebuild job %s app context -------",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(current_global_jobdata->jobid)));
    for(i_proc = 0; i_proc < opal_pointer_array_get_size(current_global_jobdata->procs); ++i_proc) {
        proc = (orte_proc_t*)opal_pointer_array_get_item(current_global_jobdata->procs, i_proc);
        if( NULL == proc ) {
            continue;
        }

        if( ORTE_SUCCESS != (ret = orte_errmgr_base_update_app_context_for_cr_recovery(current_global_jobdata,
                                                                                       proc,
                                                                                       &(snapshot->local_snapshots))) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                             "\tAdjusted: \"%s\" [0x%d] [%s]\n",
                             ORTE_NAME_PRINT(&proc->name), proc->state, proc->node->name));
    }

    ERRMGR_AUTOR_SET_TIMER(ERRMGR_AUTOR_TIMER_RESETUP);

    /*
     * Spawn the restarted job
     */
    opal_output_verbose(10, mca_errmgr_hnp_component.super.output_handle,
                        "errmgr:hnp(autor):recover() "
                        "------- Respawning the job %s -------",
                        ORTE_JOBID_PRINT(current_global_jobdata->jobid));
    orte_snapc_base_has_recovered = false;
    autor_mask_faults = false; /* Failures pass this point are worth noting */
    orte_plm.spawn(current_global_jobdata);

    /*
     * Wait for all the processes to restart
     */
    opal_output_verbose(10, mca_errmgr_hnp_component.super.output_handle,
                        "errmgr:hnp(autor):recover() "
                        "------- Waiting for restart -------");
    while(!check_if_restarted(current_global_jobdata->procs) ) {
        opal_progress();
    }

    ERRMGR_AUTOR_SET_TIMER(ERRMGR_AUTOR_TIMER_RESTART);

    /*
     * All done
     */
    while( !orte_snapc_base_has_recovered ) {
        opal_progress();
    }

    opal_output_verbose(10, mca_errmgr_hnp_component.super.output_handle,
                        "errmgr:hnp(autor):recover() "
                        "------- Finished recovering job %s -------",
                        ORTE_JOBID_PRINT(current_global_jobdata->jobid));

    opal_show_help("help-orte-errmgr-hnp.txt", "autor_recovery_complete", true);

    ERRMGR_AUTOR_SET_TIMER(ERRMGR_AUTOR_TIMER_FINISH);

 cleanup:
    while(NULL != (item = opal_list_remove_first(procs_pending_recovery))) {
        wp_item = (errmgr_autor_wp_item_t*)item;
        OBJ_RELEASE(wp_item);
    }

    if( NULL != tmp_str ) {
        free(tmp_str);
        tmp_str = NULL;
    }

    ERRMGR_AUTOR_DISPLAY_ALL_TIMERS();

    autor_timer_active = false;
    autor_mask_faults  = false;

    return;
}

static int check_if_terminated(opal_pointer_array_t *procs)
{
    orte_std_cntr_t i_proc;
    orte_proc_t *proc = NULL;
    bool is_done;

    if( NULL == procs ){
        return true;
    }

    is_done = true;
    for(i_proc = 0; i_proc < opal_pointer_array_get_size(procs); ++i_proc) {
        proc = (orte_proc_t*)opal_pointer_array_get_item(procs, i_proc);
        if( NULL == proc ) {
            continue;
        }

        if( proc->state < ORTE_PROC_STATE_UNTERMINATED ||
            proc->state == ORTE_PROC_STATE_MIGRATING ) {
            is_done = false;
            break;
        }
    }

    if( !is_done ) {
        OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                             "\t Still waiting for termination: \"%s\" [0x%x] < [0x%x]\n",
                             ORTE_NAME_PRINT(&proc->name), proc->state, ORTE_PROC_STATE_UNTERMINATED));
    }

    return is_done;
}

static int check_if_restarted(opal_pointer_array_t *procs)
{
    orte_std_cntr_t i_proc;
    orte_proc_t *proc = NULL;
    bool is_done;

    if( NULL == procs ){
        return true;
    }

    is_done = true;
    for(i_proc = 0; i_proc < opal_pointer_array_get_size(procs); ++i_proc) {
        proc = (orte_proc_t*)opal_pointer_array_get_item(procs, i_proc);
        if( NULL == proc ) {
            continue;
        }

        if( !(ORTE_PROC_STATE_RUNNING & proc->state) ) {
            is_done = false;
            break;
        }
    }

    if( !is_done ) {
        OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnp_component.super.output_handle,
                             "\t Still waiting for restart: \"%s\" [0x%x] != [0x%x]\n",
                             ORTE_NAME_PRINT(&proc->name), proc->state, ORTE_PROC_STATE_RUNNING));
    }

    return is_done;
}

/************************
 * Timing
 ************************/
static void errmgr_autor_set_time(int idx)
{
    if(idx < ERRMGR_AUTOR_TIMER_MAX ) {
        if( timer_start[idx] <= 0.0 ) {
            timer_start[idx] = errmgr_autor_get_time();
        }
    }
}

static void errmgr_autor_display_all_timers(void)
{
    double diff = 0.0;
    char * label = NULL;

    opal_output(0, "Auto. Recovery Timing: ******************** Summary Begin\n");

    /********** Structure Setup **********/
    label = strdup("Setup");
    diff = timer_start[ERRMGR_AUTOR_TIMER_SETUP]   - timer_start[ERRMGR_AUTOR_TIMER_START];
    errmgr_autor_display_indv_timer_core(diff, label);
    free(label);

    /********** Termination **********/
    label = strdup("Terminate");
    diff = timer_start[ERRMGR_AUTOR_TIMER_TERM]   - timer_start[ERRMGR_AUTOR_TIMER_SETUP];
    errmgr_autor_display_indv_timer_core(diff, label);
    free(label);

    /********** Setup new job **********/
    label = strdup("Setup Relaunch");
    diff = timer_start[ERRMGR_AUTOR_TIMER_RESETUP]   - timer_start[ERRMGR_AUTOR_TIMER_TERM];
    errmgr_autor_display_indv_timer_core(diff, label);
    free(label);

    /********** Restart **********/
    label = strdup("Restart");
    diff = timer_start[ERRMGR_AUTOR_TIMER_RESTART]   - timer_start[ERRMGR_AUTOR_TIMER_RESETUP];
    errmgr_autor_display_indv_timer_core(diff, label);
    free(label);

    /********** Finish **********/
    label = strdup("Finalize");
    diff = timer_start[ERRMGR_AUTOR_TIMER_FINISH]   - timer_start[ERRMGR_AUTOR_TIMER_RESTART];
    errmgr_autor_display_indv_timer_core(diff, label);
    free(label);

    opal_output(0, "Auto. Recovery Timing: ******************** Summary End\n");
}

static void errmgr_autor_clear_timers(void)
{
    int i;
    for(i = 0; i < ERRMGR_AUTOR_TIMER_MAX; ++i) {
        timer_start[i] = 0.0;
    }
}

static double errmgr_autor_get_time(void)
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

static void errmgr_autor_display_indv_timer_core(double diff, char *str)
{
    double total = 0;
    double perc  = 0;

    total = timer_start[ERRMGR_AUTOR_TIMER_MAX-1] - timer_start[ERRMGR_AUTOR_TIMER_START];
    perc = (diff/total) * 100;

    opal_output(0,
                "errmgr_autor: timing: %-20s = %10.2f s\t%10.2f s\t%6.2f\n",
                str,
                diff,
                total,
                perc);
    return;
}

#endif /* OPAL_ENABLE_FT_CR */
