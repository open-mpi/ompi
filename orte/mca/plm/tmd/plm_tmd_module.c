/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <signal.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#ifdef HAVE_SCHED_H
#include <sched.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <errno.h>
#include <string.h>

#include <tm.h>

#include "opal/mca/installdirs/installdirs.h"
#include "opal/mca/event/event.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "orte/util/show_help.h"
#include "opal/util/opal_environ.h"
#include "opal/util/basename.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/runtime/opal_progress.h"

#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rmaps/rmaps.h"
#include "orte/runtime/orte_quit.h"

#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/plm_private.h"
#include "plm_tmd.h"



/*
 * Local functions
 */
static int plm_tmd_init(void);
static int plm_tmd_launch_job(orte_job_t *jdata);
static int plm_tmd_terminate_job(orte_jobid_t jobid);
static int plm_tmd_terminate_orteds(void);
static int plm_tmd_signal_job(orte_jobid_t jobid, int32_t signal);
static int plm_tmd_finalize(void);

static int plm_tmd_connect(void);
static void failed_start(int fd, short event, void *arg);
static int obit_submit(int tid);

/*
 * Local "global" variables
 */
static opal_event_t *ev=NULL;
static bool connected;
static tm_event_t *events_spawn = NULL;
static tm_event_t *events_obit = NULL;
static tm_task_id *tm_task_ids = NULL;
static int *evs = NULL;
static bool time_is_up;

/*
 * Global variable
 */
orte_plm_base_module_t orte_plm_tmd_module = {
    plm_tmd_init,
    orte_plm_base_set_hnp_name,
    plm_tmd_launch_job,
    NULL,
    plm_tmd_terminate_job,
    plm_tmd_terminate_orteds,
    NULL,
    plm_tmd_signal_job,
    plm_tmd_finalize
};

/* catch timeout to allow cmds to progress */
static void timer_cb(int fd, short event, void *cbdata)
{
    opal_event_t *ev = (opal_event_t*)cbdata;
    
    /* free event */
    if (NULL != ev) {
        free(ev);
    }
    /* declare time is up */
    time_is_up = true;
}


/**
* Init the module
 */
static int plm_tmd_init(void)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_plm_base_comm_start())) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}


/* When working in this function, ALWAYS jump to "cleanup" if
 * you encounter an error so that orterun will be woken up and
 * the job can cleanly terminate
 */
static int plm_tmd_launch_job(orte_job_t *jdata)
{
    orte_job_t *jdatorted;
    orte_job_map_t *map = NULL;
    orte_app_context_t **apps;
    orte_node_t **nodes;
    int proc_vpid_index;
    char *param;
    char **env = NULL;
    char *var;
    char **argv = NULL;
    int argc = 0;
    int rc;
    orte_std_cntr_t launched = 0, i; 
    char *bin_base = NULL, *lib_base = NULL;
    int local_err;
    bool failed_launch = true;
    mode_t current_umask;
    orte_jobid_t failed_job;
    orte_job_state_t job_state = ORTE_JOB_STATE_NEVER_LAUNCHED;
    int offset;
    tm_event_t eventpolled;
    orte_std_cntr_t num_daemons;
    opal_event_t *timerev;
    int j;
    
    /* default to declaring the daemons as failed */
    failed_job = ORTE_PROC_MY_NAME->jobid;
    connected = false;

    /* setup the job */
    if (ORTE_SUCCESS != (rc = orte_plm_base_setup_job(jdata))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:tm: launching job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jdata->jobid)));
    
    /* Get the map for this job */
    if (NULL == (map = orte_rmaps.get_job_map(jdata->jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        rc = ORTE_ERR_NOT_FOUND;
        goto cleanup;
    }
    apps = (orte_app_context_t**)jdata->apps->addr;
    nodes = (orte_node_t**)map->nodes->addr;

    if (0 == map->num_new_daemons) {
        /* have all the daemons we need - launch app */
        goto launch_apps;
    }
    
    /* lookup the daemon job object - must do this -after- the job is
     * setup so the number of required daemons has been updated
     */
    if (NULL == (jdatorted = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        rc = ORTE_ERR_NOT_FOUND;
        goto cleanup;
    }
    num_daemons = jdatorted->num_procs - 1; /* do not include myself as I am already here! */
    if (0 >= num_daemons) {
        /* this won't work */
        rc = ORTE_ERR_BAD_PARAM;
        goto cleanup;
    }
    
    /* Allocate a bunch of TM events to use */
    if (NULL == events_spawn) {
        /* spawn events for first launch */
        events_spawn = (tm_event_t*)malloc(num_daemons * sizeof(tm_event_t));
        if (NULL == events_spawn) {
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
    } else {
        /* comm_spawn launch */
        events_spawn = (tm_event_t*)realloc(events_spawn, sizeof(tm_event_t) * num_daemons);
        if (NULL == events_spawn) {
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        
    }
    if (NULL == events_obit) {
        /* obit events for first launch */
        events_obit = (tm_event_t*)malloc(num_daemons * sizeof(tm_event_t));
        if (NULL == events_obit) {
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
    } else {
        /* comm_spawn launch */
        events_obit = (tm_event_t*)realloc(events_obit, sizeof(tm_event_t) * num_daemons);
        if (NULL == events_obit) {
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        
    }
    if (NULL == evs) {
        /* evs for first launch */
        evs = (int*)malloc(num_daemons * sizeof(tm_event_t));
        if (NULL == evs) {
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
    } else {
        /* comm_spawn launch */
        evs = (int*)realloc(evs, sizeof(int) * num_daemons);
        if (NULL == evs) {
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        
    }

    /* allocate task ids for the orteds */
    if (NULL == tm_task_ids) {
        /* first launch */
        tm_task_ids = (tm_task_id*)malloc(num_daemons * sizeof(tm_task_id));
        if (NULL == tm_task_ids) {
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
    } else {
        /* comm_spawn launch */
        tm_task_ids = (tm_task_id*)realloc(tm_task_ids, sizeof(tm_task_id) * num_daemons);
        if (NULL == tm_task_ids) {
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }        
    }

    /* compute the offset into the event/task arrays */
    offset = num_daemons - map->num_new_daemons;
    
    /* initialize them */
    for (i=0; i < map->num_new_daemons; i++) {
        *(tm_task_ids + offset + i)  = TM_NULL_TASK;
        *(events_spawn + offset + i) = TM_NULL_EVENT;
        *(events_obit + offset + i)  = TM_NULL_EVENT;
        *(evs + offset + i)          = 0;
    }
    
    /* add the daemon command (as specified by user) */
    orte_plm_base_setup_orted_cmd(&argc, &argv);

    /* Add basic orted command line options */
    orte_plm_base_orted_append_basic_args(&argc, &argv, "env",
                                          &proc_vpid_index,
                                          true, NULL);

    if (0 < opal_output_get_verbosity(orte_plm_globals.output)) {
        param = opal_argv_join(argv, ' ');
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:tm: final top-level argv:\n\t%s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (NULL == param) ? "NULL" : param));
        if (NULL != param) free(param);
    }

    rc = plm_tmd_connect();
    if (ORTE_SUCCESS != rc) {
        goto cleanup;
    }
    connected = true;

    /* Figure out the basenames for the libdir and bindir.  There is a
       lengthy comment about this in plm_rsh_module.c explaining all
       the rationale for how / why we're doing this. */
    lib_base = opal_basename(opal_install_dirs.libdir);
    bin_base = opal_basename(opal_install_dirs.bindir);

    /* setup environment */
    env = opal_argv_copy(orte_launch_environ);

    /* add our umask -- see big note in orted.c */
    current_umask = umask(0);
    umask(current_umask);
    asprintf(&var, "0%o", current_umask);
    opal_setenv("ORTE_DAEMON_UMASK_VALUE", var, true, &env);
    free(var);
    
    /* If we have a prefix, then modify the PATH and
        LD_LIBRARY_PATH environment variables. We only allow
        a single prefix to be specified. Since there will
        always be at least one app_context, we take it from
        there
    */
    if (NULL != apps[0]->prefix_dir) {
        char *newenv;
        
        for (i = 0; NULL != env && NULL != env[i]; ++i) {
            /* Reset PATH */
            if (0 == strncmp("PATH=", env[i], 5)) {
                asprintf(&newenv, "%s/%s:%s", 
                            apps[0]->prefix_dir, bin_base, env[i] + 5);
                OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                     "%s plm:tm: resetting PATH: %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     newenv));
                opal_setenv("PATH", newenv, true, &env);
                free(newenv);
            } 
            
            /* Reset LD_LIBRARY_PATH */
            else if (0 == strncmp("LD_LIBRARY_PATH=", env[i], 16)) {
                asprintf(&newenv, "%s/%s:%s", 
                            apps[0]->prefix_dir, lib_base, env[i] + 16);
                OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                     "%s plm:tm: resetting LD_LIBRARY_PATH: %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     newenv));
                opal_setenv("LD_LIBRARY_PATH", newenv, true, &env);
                free(newenv);
            } 
        }
    }
    
    /* set the job state to indicate we attempted to launch */
    job_state = ORTE_JOB_STATE_FAILED_TO_START;
    
    /* Iterate through each of the nodes and spin
     * up a daemon.
     */
    for (i = 0; i < map->num_nodes; i++) {
        orte_node_t* node = nodes[i];
        char* vpid_string;
        
        /* if this daemon already exists, don't launch it! */
        if (node->daemon_launched) {
            continue;
        }
 
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:tm: launching on node %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             node->name));
        
        /* setup process name */
        rc = orte_util_convert_vpid_to_string(&vpid_string, nodes[i]->daemon->name.vpid);
        if (ORTE_SUCCESS != rc) {
            opal_output(0, "plm:tm: unable to get daemon vpid as string");
            goto cleanup;
        }
        free(argv[proc_vpid_index]);
        argv[proc_vpid_index] = strdup(vpid_string);
        free(vpid_string);
        
        /* exec the daemon */
        if (0 < opal_output_get_verbosity(orte_plm_globals.output)) {
            param = opal_argv_join(argv, ' ');
            OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                 "%s plm:tm: executing:\n\t%s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 (NULL == param) ? "NULL" : param));
            if (NULL != param) free(param);
        }
        
        rc = tm_spawn(argc, argv, env, node->launch_id, tm_task_ids + offset + launched, events_spawn + offset + launched);
        if (TM_SUCCESS != rc) {
            orte_show_help("help-plm-tm.txt", "tm-spawn-failed",
                           true, argv[0], node->name, node->launch_id);
            rc = ORTE_ERROR;
            goto cleanup;
        }
        
        launched++;

        /* Allow some progress to occur */
        opal_event.loop(opal_event_base, OPAL_EVLOOP_NONBLOCK);
    }

    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:tm:launch: finished spawning orteds",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* setup a timer to give the cmd a chance to be sent */
    time_is_up = false;
    ORTE_DETECT_TIMEOUT(&timerev, launched,
                        100, -1, timer_cb);
    
    ORTE_PROGRESSED_WAIT(time_is_up, 0, 1);

    /* TM poll for all the spawns */
    while (0 < launched) {
        rc = tm_poll(TM_NULL_EVENT, &eventpolled, (int)false, &local_err);
        if (TM_SUCCESS != rc) {
            opal_output(0, "plm:tm: event poll for spawned daemon failed, return status = %d", rc);
            rc = ORTE_ERROR;
            goto cleanup;
        }
        /* if we get back the NULL event, then just continue */
        if (eventpolled == TM_NULL_EVENT) {
            continue;
        }
        /* look for the spawned event */
        for (j=0; j < map->num_new_daemons; j++) {
            if (eventpolled == *(events_spawn + offset  + j)) {
                /* got the event - check returned code */
                if (local_err) {
                    /* this orted failed to launch! */
                    orte_show_help("help-plm-tm.txt", "tm-spawn-failed",
                                   true, argv[0], nodes[j]->name, nodes[j]->launch_id);
                    rc = ORTE_ERROR;
                    goto cleanup;
                }
                /* register the corresponding obit so we can detect when this
                 * orted terminates
                 */
                if (ORTE_SUCCESS != (rc = obit_submit(offset+j))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
                /* all done with this event */
                goto MOVEON;
            }
        }
        /* if we get here, then we failed to find the event */
        opal_output(0, "TM FAILED TO FIND SPAWN EVENT WHEN LAUNCHING");
        rc = ORTE_ERROR;
        goto cleanup;

    MOVEON:
        launched--;
    }
    
    /* set a timer to tell us if one or more daemon's fails to start - use the
     * millisec/daemon timeout provided by the user to compute time
     */
    if (0 < orte_startup_timeout) {
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:tm: setting startup timer for %d milliseconds",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             orte_startup_timeout));
        ORTE_DETECT_TIMEOUT(&ev, map->num_new_daemons,
                            orte_startup_timeout*1000,
                            -1, failed_start);
    }
    
    /* wait for daemons to callback */
    if (ORTE_SUCCESS != (rc = orte_plm_base_daemon_callback(map->num_new_daemons))) {
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:tm: daemon launch failed for job %s on error %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(jdata->jobid), ORTE_ERROR_NAME(rc)));
        goto cleanup;
    }
    
    /* if issued, cancel the failed-to-start timer */
    if (NULL != ev) {
        opal_event.del(ev);
    }
    
launch_apps:
    /* since the daemons have launched, any failures now will be for the
     * application job
     */
    failed_job = jdata->jobid;
    if (ORTE_SUCCESS != (rc = orte_plm_base_launch_apps(jdata->jobid))) {
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:tm: launch of apps failed for job %s on error %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(jdata->jobid), ORTE_ERROR_NAME(rc)));
        goto cleanup;
    }
    
    /* if we get here, then everything launched okay - record that fact */
    failed_launch = false;
    
    
 cleanup:
    if (NULL != argv) {
        opal_argv_free(argv);
    }
    if (NULL != env) {
        opal_argv_free(env);
    }
    
    if (NULL != lib_base) {
        free(lib_base);
    }
    if (NULL != bin_base) {
        free(bin_base);
    }

    /* check for failed launch - if so, force terminate */
    if (failed_launch) {
        orte_errmgr.update_state(failed_job, job_state,
                                 NULL, ORTE_PROC_STATE_UNDEF,
                                 0, ORTE_ERROR_DEFAULT_EXIT_CODE);
    }
        
    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:tm:launch: finished",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    return rc;
}


static int plm_tmd_terminate_job(orte_jobid_t jobid)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

/* quick timeout loop */
static bool timer_fired;

static void quicktime_cb(int fd, short event, void *cbdata)
{
    /* declare it fired */
    timer_fired = true;
}

/**
 * Terminate the orteds for a given job
 */
int plm_tmd_terminate_orteds(void)
{
    int rc;
    orte_job_t *jdata;
    orte_proc_t **daemons;
    tm_event_t eventpolled; 
    orte_vpid_t j, alive;
    int local_err;
    opal_event_t *timerev=NULL;
    opal_event_t *quicktime=NULL;
    struct timeval quicktimeval;
    bool aborted;

    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:tm: terminating orteds",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* lookup the daemon job object */
    if (NULL == (jdata = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
    }
    alive = jdata->num_procs - 1; /* do not include myself! */
    daemons = (orte_proc_t**)jdata->procs->addr;
    aborted = false;
    
    /* tell them to die! */
    if (ORTE_SUCCESS != (rc = orte_plm_base_orted_exit(ORTE_DAEMON_EXIT_CMD))) {
        ORTE_ERROR_LOG(rc);
    }
    
    /* if there are more than just me... */
    if (0 < alive) {
        /* setup a max time for the daemons to die */
        time_is_up = false;
        ORTE_DETECT_TIMEOUT(&timerev, alive,
                            1000000, 60000000, timer_cb);
        
        /* give the cmds a chance to get out */
        quicktimeval.tv_sec = 0;
        quicktimeval.tv_usec = 100;
        timer_fired = false;
        ORTE_DETECT_TIMEOUT(&quicktime, alive, 1000, 10000, quicktime_cb);
        ORTE_PROGRESSED_WAIT(timer_fired, 0, 1);
        
        /* now begin polling to see if daemons have terminated */
        while (!time_is_up && 0 < alive) {
            OPAL_OUTPUT_VERBOSE((10, orte_plm_globals.output,
                                 "%s plm:tm: polling for daemon termination",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            rc = tm_poll(TM_NULL_EVENT, &eventpolled, (int)false, &local_err);
            if (TM_SUCCESS != rc) {
                errno = local_err;
                opal_output(0, "plm:tm: event poll for daemon termination failed, return status = %d", rc);
                continue;  /* we will wait for timeout to tell us to quit */
            }
            /* if we get back the NULL event, then just continue */
            if (eventpolled == TM_NULL_EVENT) {
                OPAL_OUTPUT_VERBOSE((10, orte_plm_globals.output,
                                     "%s plm:tm: got null event",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                /* give system a little time to progress */
                timer_fired = false;
                opal_event.evtimer_add(quicktime, &quicktimeval);
                ORTE_PROGRESSED_WAIT(timer_fired, 0, 1);
                continue;
            }
            /* look for the obit event */
            for (j=0; j < jdata->num_procs-1; j++) {
                if (eventpolled == *(events_obit + j)) {
                    /* got the event - check returned code */
                    if (local_err == TM_ESYSTEM) {
                        OPAL_OUTPUT_VERBOSE((10, orte_plm_globals.output,
                                             "%s plm:tm: got TM_ESYSTEM on obit - resubmitting",
                                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                        if (ORTE_SUCCESS != (rc = obit_submit(j))) {
                            ORTE_ERROR_LOG(rc);
                            goto MOVEON;
                        }
                        /* give system a little time to progress */
                        timer_fired = false;
                        opal_event.evtimer_add(quicktime, &quicktimeval);
                        ORTE_PROGRESSED_WAIT(timer_fired, 0, 1);
                    }
                    if (0 != local_err) {
                        OPAL_OUTPUT_VERBOSE((10, orte_plm_globals.output,
                                             "%s plm:tm: got error %d on obit for task %d",
                                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), local_err, j));
                        rc = ORTE_ERROR;
                        goto MOVEON;
                    }
                    /* this daemon has terminated */
                    *(tm_task_ids+j) = TM_NULL_TASK;
                    *(events_obit+j) = TM_NULL_EVENT;
                    OPAL_OUTPUT_VERBOSE((10, orte_plm_globals.output,
                                         "%s plm:tm: task %d exited with status %d",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), j, *(evs+j)));
                    /* update the termination status for this daemon */
                    daemons[j+1]->exit_code = *(evs+j);
                    if (0 != daemons[j+1]->exit_code) {
                        daemons[j+1]->state = ORTE_PROC_STATE_ABORTED;
                        aborted = true;
                    } else {
                        daemons[j+1]->state = ORTE_PROC_STATE_TERMINATED;
                    }
                    jdata->num_terminated++;
                    /* all done with this event */
                    goto MOVEON;
                }
            }
            /* if we get here, then we failed to find the event */
            opal_output(0, "TM FAILED TO FIND OBIT EVENT");
            
        MOVEON:
            alive--;
        }
        
        /* release event if not already done */
        if (NULL != quicktime) {
            free(quicktime);
        }
        if (NULL != timerev) {
            opal_event.del(timerev);
            free(timerev);
        }
    } else {
        /* still need to give the cmds a chance to get out so I can process
         * them myself!
         */
        timer_fired = false;
        ORTE_DETECT_TIMEOUT(&quicktime, 1, 1000, 10000, quicktime_cb);
        ORTE_PROGRESSED_WAIT(timer_fired, 0, 1);
    }
    
    /* declare the daemons done */
    if (aborted || 0 < alive) {
        jdata->state = ORTE_JOB_STATE_ABORTED;
    } else {
        jdata->state = ORTE_JOB_STATE_TERMINATED;
    }
    orte_quit();
    return rc;
}

static int plm_tmd_signal_job(orte_jobid_t jobid, int32_t signal)
{
    int rc;
    
    /* order them to pass this signal to their local procs */
    if (ORTE_SUCCESS != (rc = orte_plm_base_orted_signal_local_procs(jobid, signal))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}


/*
 * Free stuff
 */
static int plm_tmd_finalize(void)
{
    int rc;
    
    /* cleanup any pending recvs */
    if (ORTE_SUCCESS != (rc = orte_plm_base_comm_stop())) {
        ORTE_ERROR_LOG(rc);
    }

    if (connected) {
        tm_finalize();
    }
    
    /* cleanup data arrays */
    if (NULL != events_spawn) {
        free(events_spawn);
    }
    if (NULL != events_obit) {
        free(events_obit);
    }
    if (NULL != tm_task_ids) {
        free(tm_task_ids);
    }
    if (NULL != evs) {
        free(evs);
    }
    
    return ORTE_SUCCESS;
}


static int plm_tmd_connect(void)
{
    int ret;
    struct tm_roots tm_root;
    int count, progress;

    /* try a couple times to connect - might get busy signals every
       now and then */
    for (count = 0 ; count < 10; ++count) {
        ret = tm_init(NULL, &tm_root);
        if (TM_SUCCESS == ret) {
            return ORTE_SUCCESS;
        }

        for (progress = 0 ; progress < 10 ; ++progress) {
            opal_progress();
#if HAVE_SCHED_YIELD
            sched_yield();
#endif
        }
    }

    return ORTE_ERR_RESOURCE_BUSY;
}


/* call this function if the timer fires indicating that one
 * or more daemons failed to start
 */
static void failed_start(int fd, short dummy, void *arg)
{
    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:tm:failed_start",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* if we are aborting, ignore this */
    if (orte_abnormal_term_ordered) {
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:tm:failed_start - abnormal term in progress",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        return;
    }
    
    orte_errmgr.update_state(ORTE_PROC_MY_NAME->jobid, ORTE_JOB_STATE_FAILED_TO_START,
                             NULL, ORTE_PROC_STATE_UNDEF, 0, ORTE_ERROR_DEFAULT_EXIT_CODE);
}

static int obit_submit(int tid)
{
    int rc;
    
    if (TM_SUCCESS != (rc = tm_obit(*(tm_task_ids+tid), evs+tid, events_obit+tid))) {
        opal_output(0, "failed to register termination notice for task %d", tid);
        rc = ORTE_ERROR;
        return rc;
    }
    if (*(events_obit+tid) == TM_NULL_EVENT) {
        opal_output(0, "task %d is already dead", tid);
    } else if (*(events_obit+tid) == TM_ERROR_EVENT) {
        opal_output(0, "Error on obit return - got error event for task %d", tid);
    }
    
    return ORTE_SUCCESS;
}
