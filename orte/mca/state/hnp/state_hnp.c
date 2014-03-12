/*
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.
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
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "opal/util/output.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/iof/iof.h"
#include "orte/mca/plm/base/base.h"
#include "orte/mca/ras/base/base.h"
#include "orte/mca/rmaps/base/base.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/sensor/sensor.h"
#include "orte/util/session_dir.h"
#include "orte/runtime/orte_quit.h"

#include "orte/mca/state/state.h"
#include "orte/mca/state/base/base.h"
#include "orte/mca/state/base/state_private.h"
#include "state_hnp.h"

/*
 * Module functions: Global
 */
static int init(void);
static int finalize(void);

/******************
 * HNP module - just uses base functions after
 * initializing the proc state machine. Job state
 * machine is unused by hnplication procs at this
 * time.
 ******************/
orte_state_base_module_t orte_state_hnp_module = {
    init,
    finalize,
    orte_state_base_activate_job_state,
    orte_state_base_add_job_state,
    orte_state_base_set_job_state_callback,
    orte_state_base_set_job_state_priority,
    orte_state_base_remove_job_state,
    orte_state_base_activate_proc_state,
    orte_state_base_add_proc_state,
    orte_state_base_set_proc_state_callback,
    orte_state_base_set_proc_state_priority,
    orte_state_base_remove_proc_state
};

/* defined default state machine sequence - individual
 * plm's must add a state for launching daemons
 */
static orte_job_state_t launch_states[] = {
    ORTE_JOB_STATE_INIT,
    ORTE_JOB_STATE_INIT_COMPLETE,
    ORTE_JOB_STATE_ALLOCATE,
    ORTE_JOB_STATE_ALLOCATION_COMPLETE,
    ORTE_JOB_STATE_DAEMONS_LAUNCHED,
    ORTE_JOB_STATE_DAEMONS_REPORTED,
    ORTE_JOB_STATE_VM_READY,
    ORTE_JOB_STATE_MAP,
    ORTE_JOB_STATE_MAP_COMPLETE,
    ORTE_JOB_STATE_SYSTEM_PREP,
    ORTE_JOB_STATE_LAUNCH_APPS,
    ORTE_JOB_STATE_LOCAL_LAUNCH_COMPLETE,
    ORTE_JOB_STATE_RUNNING,
    ORTE_JOB_STATE_REGISTERED,
    /* termination states */
    ORTE_JOB_STATE_TERMINATED,
    ORTE_JOB_STATE_NOTIFY_COMPLETED,
    ORTE_JOB_STATE_ALL_JOBS_COMPLETE
};
static orte_state_cbfunc_t launch_callbacks[] = {
    orte_plm_base_setup_job,
    orte_plm_base_setup_job_complete,
    orte_ras_base_allocate,
    orte_plm_base_allocation_complete,
    orte_plm_base_daemons_launched,
    orte_plm_base_daemons_reported,
    orte_plm_base_vm_ready,
    orte_rmaps_base_map_job,
    orte_plm_base_mapping_complete,
    orte_plm_base_complete_setup,
    orte_plm_base_launch_apps,
    orte_state_base_local_launch_complete,
    orte_plm_base_post_launch,
    orte_plm_base_registered,
    orte_state_base_check_all_complete,
    orte_state_base_cleanup_job,
    orte_quit
};

static orte_proc_state_t proc_states[] = {
    ORTE_PROC_STATE_RUNNING,
    ORTE_PROC_STATE_REGISTERED,
    ORTE_PROC_STATE_IOF_COMPLETE,
    ORTE_PROC_STATE_WAITPID_FIRED,
    ORTE_PROC_STATE_TERMINATED
};
static orte_state_cbfunc_t proc_callbacks[] = {
    orte_state_base_track_procs,
    orte_state_base_track_procs,
    orte_state_base_track_procs,
    orte_state_base_track_procs,
    orte_state_base_track_procs
};

static void force_quit(int fd, short args, void *cbdata)
{
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;

    /* give us a chance to stop the orteds */
    orte_plm.terminate_orteds();
    OBJ_RELEASE(caddy);
}

/************************
 * API Definitions
 ************************/
static int init(void)
{
    int i, rc;
    int num_states;

    /* setup the state machines */
    OBJ_CONSTRUCT(&orte_job_states, opal_list_t);
    OBJ_CONSTRUCT(&orte_proc_states, opal_list_t);

    /* setup the job state machine */
    num_states = sizeof(launch_states) / sizeof(orte_job_state_t);
    for (i=0; i < num_states; i++) {
        if (ORTE_SUCCESS != (rc = orte_state.add_job_state(launch_states[i],
                                                           launch_callbacks[i],
                                                           ORTE_SYS_PRI))) {
            ORTE_ERROR_LOG(rc);
        }
    }
    /* add the termination response */
    if (ORTE_SUCCESS != (rc = orte_state.add_job_state(ORTE_JOB_STATE_DAEMONS_TERMINATED,
                                                       orte_quit, ORTE_SYS_PRI))) {
        ORTE_ERROR_LOG(rc);
    }
    /* add a default error response */
    if (ORTE_SUCCESS != (rc = orte_state.add_job_state(ORTE_JOB_STATE_FORCED_EXIT,
                                                       force_quit, ORTE_ERROR_PRI))) {
        ORTE_ERROR_LOG(rc);
    }
    /* add callback to report progress, if requested */
    if (ORTE_SUCCESS != (rc = orte_state.add_job_state(ORTE_JOB_STATE_REPORT_PROGRESS,
                                                       orte_state_base_report_progress, ORTE_ERROR_PRI))) {
        ORTE_ERROR_LOG(rc);
    }
    if (5 < opal_output_get_verbosity(orte_state_base_framework.framework_output)) {
        orte_state_base_print_job_state_machine();
    }

    /* populate the proc state machine to allow us to
     * track proc lifecycle changes
     */
    num_states = sizeof(proc_states) / sizeof(orte_proc_state_t);
    for (i=0; i < num_states; i++) {
        if (ORTE_SUCCESS != (rc = orte_state.add_proc_state(proc_states[i],
                                                            proc_callbacks[i],
                                                            ORTE_SYS_PRI))) {
            ORTE_ERROR_LOG(rc);
        }
    }
    if (5 < opal_output_get_verbosity(orte_state_base_framework.framework_output)) {
        orte_state_base_print_proc_state_machine();
    }

    return ORTE_SUCCESS;
}

static int finalize(void)
{
    opal_list_item_t *item;

    /* cleanup the proc state machine */
    while (NULL != (item = opal_list_remove_first(&orte_proc_states))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&orte_proc_states);

    return ORTE_SUCCESS;
}
