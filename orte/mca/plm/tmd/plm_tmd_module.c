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

#if HAVE_UNISTD_H
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
#include <tm.h>

#include "opal/mca/installdirs/installdirs.h"
#include "opal/threads/condition.h"
#include "opal/event/event.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/opal_environ.h"
#include "opal/util/show_help.h"
#include "opal/util/path.h"
#include "opal/util/basename.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/runtime/opal_progress.h"

#include "orte/util/name_fns.h"
#include "orte/util/context_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_wakeup.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rmaps/rmaps.h"

#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/plm_private.h"
#include "plm_tmd.h"

/* define frequency of checking job complete */
#define PLM_CHECK_COMPLETE_TIME 1

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
static int plm_tmd_disconnect(void);

/*
 * Local "global" variables
 */
static tm_task_id *tm_task_ids = NULL;
static int num_tids;
static orte_job_t *active_jdata;

/*
 * Global variable
 */
orte_plm_base_module_t orte_plm_tmd_module = {
    plm_tmd_init,
    orte_plm_base_set_hnp_name,
    plm_tmd_launch_job,
    plm_tmd_terminate_job,
    plm_tmd_terminate_orteds,
    plm_tmd_signal_job,
    plm_tmd_finalize
};

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


/* check for job completion */
static void check_job_complete(int fd, short evnt, void *arg)
{
    tm_event_t event, *tm_events;
    int *exit_codes, i, rc, local_err;
    orte_vpid_t v;
    orte_proc_t **procs;
    
opal_output(0, "checking job completion");

    tm_events = malloc(sizeof(tm_event_t) * num_tids);
    exit_codes = malloc(sizeof(int) * num_tids);
    
    /* connect to the mom */
    rc = plm_tmd_connect();
    if (ORTE_SUCCESS != rc) {
        opal_output(0, "CANNOT CONNECT TO MOM");
        goto cleanup;
    }
    
    /* cycle through the task_id's and check for complete */
    for (i=0; i < num_tids; i++) {
        if (ORTE_SUCCESS != (rc = tm_obit(tm_task_ids[i], exit_codes + i, tm_events + i))) {
            opal_output(0, "CANNOT CHECK PROC %d", i);
        }
    }
    opal_output(0, "obits requested");
    
    
    /* cycle through and poll the events */
    for (i=0; i < num_tids; i++) {
        rc = tm_poll(TM_NULL_EVENT, &event, 1, &local_err);
        if (TM_SUCCESS != rc) {
            errno = local_err;
            opal_output(0, "plm:tmd: failed to poll obit, return status = %d", rc);
        }
    }
    opal_output(0, "obits polled");
    
    
    /* store the exit codes */
    procs = (orte_proc_t**)active_jdata->procs->addr;
    for (v=0; v < active_jdata->num_procs; v++) {
        procs[v]->exit_code = exit_codes[v];
        ORTE_UPDATE_EXIT_STATUS(exit_codes[v]);
        opal_output(0, "rank %d ecode %d", (int)v, exit_codes[v]);
        if (WIFEXITED(exit_codes[v])) {
            if (procs[v]->state < ORTE_PROC_STATE_TERMINATED) {
                procs[v]->state = ORTE_PROC_STATE_TERMINATED;
                active_jdata->num_terminated++;
            }
        } else {
            procs[v]->state = ORTE_PROC_STATE_ABORTED_BY_SIG;
            active_jdata->abort = true;
            active_jdata->aborted_proc = procs[v];
            active_jdata->state = ORTE_JOB_STATE_ABORTED;
        }
    }
    
    /* disconnect from mom */
    plm_tmd_disconnect();
    
cleanup:
    /* free tm_events */
    free(tm_events);
    free(exit_codes);

    /* check for completion */
    if (active_jdata->num_terminated >= active_jdata->num_procs) {
        active_jdata->state = ORTE_JOB_STATE_TERMINATED;
        orte_wakeup();
    } else if (active_jdata->state == ORTE_JOB_STATE_ABORTED &&
               !orte_abnormal_term_ordered && !orte_abort_in_progress) {
        orte_errmgr.proc_aborted(&(active_jdata->aborted_proc->name),
                                 active_jdata->aborted_proc->exit_code);
    }

    /* reset the timer */
    ORTE_TIMER_EVENT(1, check_job_complete);
    opal_output(0, "timer reset");
    
}


/* When working in this function, ALWAYS jump to "cleanup" if
 * you encounter an error so that orterun will be woken up and
 * the job can cleanly terminate
 */
static int plm_tmd_launch_job(orte_job_t *jdata)
{
    orte_job_map_t *map = NULL;
    orte_app_context_t **apps;
    orte_node_t **nodes;
    orte_proc_t **procs;
    char *param, *param2;
    char **env = NULL;
    int rc;
    bool connected = false;
    orte_std_cntr_t launched = 0, i, n; 
    char *bin_base = NULL, *lib_base = NULL;
    int local_err;
    tm_event_t event;
    bool failed_launch = true;
    orte_vpid_t v;
    tm_event_t *tm_events = NULL;

    /* record who we are working on */
    active_jdata = jdata;
    
    /* create a jobid for this job */
    if (ORTE_SUCCESS != (rc = orte_plm_base_create_jobid(&jdata->jobid))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:tm: launching job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jdata->jobid)));
    
    /* setup the job */
    if (ORTE_SUCCESS != (rc = orte_plm_base_setup_job(jdata))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* Get the map for this job */
    if (NULL == (map = orte_rmaps.get_job_map(jdata->jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        rc = ORTE_ERR_NOT_FOUND;
        goto cleanup;
    }
    apps = (orte_app_context_t**)jdata->apps->addr;
    nodes = (orte_node_t**)map->nodes->addr;
    procs = (orte_proc_t**)jdata->procs->addr;

    /* Allocate a bunch of TM events to use for tm_spawn()ing - we will
     * need one/process since we are not launching daemons
     */
    tm_events = malloc(sizeof(tm_event_t) * jdata->num_procs);
    if (NULL == tm_events) {
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    tm_task_ids = malloc(sizeof(tm_task_id) * jdata->num_procs);
    if (NULL == tm_task_ids) {
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    num_tids = jdata->num_procs;

    /* Figure out the basenames for the libdir and bindir.  There is a
        lengthy comment about this in plm_rsh_module.c explaining all
        the rationale for how / why we're doing this. */
    lib_base = opal_basename(opal_install_dirs.libdir);
    bin_base = opal_basename(opal_install_dirs.bindir);
    
    
    /* setup the environment for each app_context - we need to insert enviro
     * values that tell the procs how to bootstrap themselves
     */
    for (n=0; n < jdata->num_apps; n++) {
        orte_app_context_t *context = apps[n];
        char ***env = &context->env;
        
        /* setup base environment: copy the current environ and merge
         * in the app context environ
         */
        if (NULL != context->env) {
            *env = opal_environ_merge(orte_launch_environ, context->env);
        } else {
            *env = opal_argv_copy(orte_launch_environ);
        }
        
        /* Try to change to the context cwd and check that the app
         * exists and is executable The function will
         * take care of outputting a pretty error message, if required
         */
        if (ORTE_SUCCESS != (rc = orte_util_check_context_cwd(context, true))) {
            /* do not ERROR_LOG - it will be reported elsewhere */
            goto cleanup;
        }
        if (ORTE_SUCCESS != (rc = orte_util_check_context_app(context))) {
            /* do not ERROR_LOG - it will be reported elsewhere */
            goto cleanup;
        }
        
        /* special case handling for --prefix: this is somewhat icky,
         * but at least some users do this.  :-\ It is possible that
         * when using --prefix, the user will also "-x PATH" and/or
         * "-x LD_LIBRARY_PATH", which would therefore clobber the
         * work that was done in the prior pls to ensure that we have
         * the prefix at the beginning of the PATH and
         * LD_LIBRARY_PATH.  So examine the context->env and see if we
         * find PATH or LD_LIBRARY_PATH.  If found, that means the
         * prior work was clobbered, and we need to re-prefix those
         * variables.
         */
        for (i = 0; NULL != context->env && NULL != context->env[i]; ++i) {
            char *newenv;
            
            /* Reset PATH */
            if (0 == strncmp("PATH=", context->env[i], 5)) {
                asprintf(&newenv, "%s/%s:%s", 
                         apps[n]->prefix_dir, bin_base, context->env[i] + 5);
                OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                     "%s plm:tmd: resetting PATH: %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     newenv));
                opal_setenv("PATH", newenv, true, env);
                free(newenv);
            }
            
            /* Reset LD_LIBRARY_PATH */
            else if (0 == strncmp("LD_LIBRARY_PATH=", context->env[i], 16)) {
                asprintf(&newenv, "%s/%s:%s", 
                         apps[n]->prefix_dir, lib_base, context->env[i] + 16);
                OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                     "%s plm:tm: resetting LD_LIBRARY_PATH: %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     newenv));
                opal_setenv("LD_LIBRARY_PATH", newenv, true, env);
                free(newenv);
            }
        }
        
        /* there will be no local daemon */
        param = mca_base_param_environ_variable("orte","local_daemon","uri");
        opal_unsetenv(param, env);
        free(param);
        
        /* pass the hnp's contact info to the local proc */
        param = mca_base_param_environ_variable("orte","hnp","uri");
        opal_setenv(param, orte_process_info.my_hnp_uri, true, env);
        free(param);
        
        /* set the app_context number into the environment */
        param = mca_base_param_environ_variable("orte","app","num");
        asprintf(&param2, "%ld", (long)context->idx);
        opal_setenv(param, param2, true, env);
        free(param);
        free(param2);
        
        /* set the universe size in the environment */
        param = mca_base_param_environ_variable("orte","universe","size");
        asprintf(&param2, "%ld", (long)jdata->total_slots_alloc);
        opal_setenv(param, param2, true, env);
        free(param);
        
        /* although the total_slots_alloc is the universe size, users
         * would appreciate being given a public environmental variable
         * that also represents this value - something MPI specific - so
         * do that here.
         *
         * AND YES - THIS BREAKS THE ABSTRACTION BARRIER TO SOME EXTENT.
         * We know - just live with it
         */
        opal_setenv("OMPI_UNIVERSE_SIZE", param2, true, env);
        free(param2);
        
        /* tell the proc to use the "envd" - i.e., the environment direct
         * ESS module to set itself up
         */
        param = mca_base_param_environ_variable("ess",NULL,"NULL");
        opal_setenv(param, "envd", true, env);
        free(param);
       
        /* since we want to pass the name as separate components, make sure
         * that the "name" environmental variable is cleared!
         */
        if(NULL == (param = mca_base_param_environ_variable("orte","ess","name"))) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        opal_unsetenv(param, env);
        free(param);
        
        /* tell the proc the jobid - same for everyone */
        if (ORTE_SUCCESS != (rc = orte_util_convert_jobid_to_string(&param2, jdata->jobid))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        if(NULL == (param = mca_base_param_environ_variable("orte","ess","jobid"))) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            goto cleanup;
        }
        opal_setenv(param, param2, true, env);
        free(param);
        free(param2);
        
        /* tell the proc the #procs in this job */
        asprintf(&param2, "%ld", (long) jdata->num_procs);
        if(NULL == (param = mca_base_param_environ_variable("orte","ess","num_procs"))) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        opal_setenv(param, param2, true, env);
        free(param);
        
        /* although the num_procs is the comm_world size, users
         * would appreciate being given a public environmental variable
         * that also represents this value - something MPI specific - so
         * do that here.
         *
         * AND YES - THIS BREAKS THE ABSTRACTION BARRIER TO SOME EXTENT.
         * We know - just live with it
         */
        opal_setenv("OMPI_COMM_WORLD_SIZE", param2, true, env);
        free(param2);
        
    }
    
    rc = plm_tmd_connect();
    if (ORTE_SUCCESS != rc) {
        goto cleanup;
    }
    connected = true;

    /* Iterate through each of the procs and launch it */
    for (v = 0; v < jdata->num_procs; v++) {
        orte_proc_t *proc = procs[v];
        orte_node_t* node = proc->node;
        orte_app_context_t *context = apps[proc->app_idx];
        char **environ_copy;
        int argc;
        
        /* copy the environment */
        environ_copy = opal_argv_copy(context->env);
        
        /* pass the proc's vpid */
        if (ORTE_SUCCESS != (rc = orte_util_convert_vpid_to_string(&param2, proc->name.vpid))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        if(NULL == (param = mca_base_param_environ_variable("orte","ess","vpid"))) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            goto cleanup;
        }
        opal_setenv(param, param2, true, &environ_copy);
        free(param);
        /* although the vpid IS the process' rank within the job, users
         * would appreciate being given a public environmental variable
         * that also represents this value - something MPI specific - so
         * do that here.
         *
         * AND YES - THIS BREAKS THE ABSTRACTION BARRIER TO SOME EXTENT.
         * We know - just live with it
         */
        opal_setenv("OMPI_COMM_WORLD_RANK", param2, true, &environ_copy);
        free(param2);  /* done with this now */

        /* pass the node's launch_id to the process as a "nodeid" so it can
         * identify which procs are local to it
         */
        asprintf(&param2, "%ld", (long)node->launch_id);
        param = mca_base_param_environ_variable("orte","nodeid",NULL);
        opal_setenv(param, param2, true, &environ_copy);
        free(param);
        free(param2);
        /* ensure we use the same nodename */
        param = mca_base_param_environ_variable("orte", "base", "nodename");
        opal_setenv(param, node->name, true, &environ_copy);
        free(param);
        
        /* set the local rank */
        asprintf(&param2, "%lu", (unsigned long) proc->local_rank);
        if(NULL == (param = mca_base_param_environ_variable("orte","ess","local_rank"))) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            goto cleanup;
        }
        opal_setenv(param, param2, true, &environ_copy);
        free(param);
        free(param2);
        
        /* setup yield schedule and processor affinity
         * We default here to always setting the affinity processor if we want
         * it. The processor affinity system then determines
         * if processor affinity is enabled/requested - if so, it then uses
         * this value to select the process to which the proc is "assigned".
         * Otherwise, the paffinity subsystem just ignores this value anyway
         */
        param = mca_base_param_environ_variable("mpi", NULL,
                                                "paffinity_processor");
        asprintf(&param2, "%lu", (unsigned long) proc->local_rank);
        opal_setenv(param, param2, true, &environ_copy);
        free(param);
        free(param2);
        
        if (node->oversubscribed) {
            param = mca_base_param_environ_variable("mpi", NULL, "yield_when_idle");
            opal_setenv(param, "1", false, &environ_copy);
        } else {
            param = mca_base_param_environ_variable("mpi", NULL, "yield_when_idle");
            opal_setenv(param, "0", false, &environ_copy);
        }
        free(param);
        
        asprintf(&param2, "%ld", (long) node->num_procs);
        if(NULL == (param = mca_base_param_environ_variable("orte","ess","num_local_procs"))) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        opal_setenv(param, param2, true, &environ_copy);
        free(param);
        free(param2);
        
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:tm: launching on node %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             node->name));
        
        /* set the argc count */
        argc = opal_argv_count(context->argv);
        
        rc = tm_spawn(argc, context->argv, environ_copy, node->launch_id, tm_task_ids + launched, tm_events + launched);
        if (TM_SUCCESS != rc) {
            opal_show_help("help-plm-tm.txt", "tm-spawn-failed",
                           true, context->argv[0], node->name, node->launch_id);
            rc = ORTE_ERROR;
            goto cleanup;
        }
        opal_argv_free(environ_copy);
        launched++;

        /* Allow some progress to occur */
        opal_event_loop(OPAL_EVLOOP_NONBLOCK);
    }

    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:tm:launch: finished spawning procs",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* TM poll for all the spawns */
    for (i = 0; i < launched; ++i) {
        rc = tm_poll(TM_NULL_EVENT, &event, 1, &local_err);
        if (TM_SUCCESS != rc) {
            errno = local_err;
            opal_output(0, "plm:tm: failed to poll for a spawned proc, return status = %d", rc);
            goto cleanup;
        }
    }
#if 0    
    if (ORTE_SUCCESS != (rc = orte_plm_base_launch_apps(jdata->jobid))) {
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:tm: launch of apps failed for job %s on error %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(jdata->jobid), ORTE_ERROR_NAME(rc)));
        goto cleanup;
    }
#endif
    /* if we get here, then everything launched okay - record that fact */
    failed_launch = false;
    
    /* setup periodic timer so we check for job completion */
    ORTE_TIMER_EVENT(1, check_job_complete);
    
 cleanup:
    /* can't reuse the events, so may as well get rid of them */
    if (NULL != tm_events) {
        free(tm_events);
    }
    
    if (NULL != env) {
        opal_argv_free(env);
    }
    
    if (connected) {
        plm_tmd_disconnect();
    }
    if (NULL != lib_base) {
        free(lib_base);
    }
    if (NULL != bin_base) {
        free(bin_base);
    }

    /* check for failed launch - if so, force terminate */
    if (failed_launch) {
        orte_plm_base_launch_failed(jdata->jobid, false, -1, 0, ORTE_JOB_STATE_FAILED_TO_START);
    }
        
    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:tm:launch: finished",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    return rc;
}


static int plm_tmd_terminate_job(orte_jobid_t jobid)
{
    int rc;
    int i, local_err;
    tm_event_t event, *tm_events;
    
    tm_events = malloc(sizeof(tm_event_t) * num_tids);
    if (NULL == tm_events) {
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* connect to the mom */
    rc = plm_tmd_connect();
    if (ORTE_SUCCESS != rc) {
        opal_output(0, "CANNOT CONNECT TO MOM");
        goto cleanup;
    }

    /* cycle through the task_id's and kill them */
    for (i=0; i < num_tids; i++) {
        if (ORTE_SUCCESS != (rc = tm_kill(tm_task_ids[i], SIGTERM, &tm_events[i]))) {
            opal_output(0, "CANNOT KILL PROC %d", i);
        }
    }
    
    /* cycle through and poll the events */
    for (i=0; i < num_tids; i++) {
        rc = tm_poll(TM_NULL_EVENT, &event, 1, &local_err);
        if (TM_SUCCESS != rc) {
            errno = local_err;
            opal_output(0, "plm:tmd: failed to kill proc, return status = %d", rc);
        }
    }

    /* disconnect from mom */
    plm_tmd_disconnect();

cleanup:
    /* free tm_events */
    free(tm_events);
    
    return ORTE_SUCCESS;
}


/**
 * Terminate the orteds for a given job
 */
static int plm_tmd_terminate_orteds(void)
{
    orte_job_t *daemons;
    int data=1;

    /* fake the system into thinking the orteds
     * reported their clean termination
     */
    if (NULL == (daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    daemons->num_terminated = daemons->num_procs;
    
    /* fire the trigger indicating that the orteds are gone */
    write(orteds_exit, &data, sizeof(int));
    opal_progress();
    
    return ORTE_SUCCESS;
}

static int plm_tmd_signal_job(orte_jobid_t jobid, int32_t signal)
{
    int rc;
    int i, local_err;
    tm_event_t event, *tm_events;
    
    tm_events = malloc(sizeof(tm_event_t) * num_tids);
    if (NULL == tm_events) {
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* connect to the mom */
    rc = plm_tmd_connect();
    if (ORTE_SUCCESS != rc) {
        opal_output(0, "CANNOT CONNECT TO MOM");
        goto cleanup;
    }
    
    /* cycle through the task_id's and kill them */
    for (i=0; i < num_tids; i++) {
        if (ORTE_SUCCESS != (rc = tm_kill(tm_task_ids[i], signal, &tm_events[i]))) {
            opal_output(0, "CANNOT SIGNAL PROC %d", i);
        }
    }
    
    /* cycle through and poll the events */
    for (i=0; i < num_tids; i++) {
        rc = tm_poll(TM_NULL_EVENT, &event, 1, &local_err);
        if (TM_SUCCESS != rc) {
            errno = local_err;
            opal_output(0, "plm:tmd: failed to signal proc, return status = %d", rc);
            goto cleanup;
        }
    }
    
    /* disconnect from mom */
    plm_tmd_disconnect();
    
cleanup:
    /* free tm_events */
    free(tm_events);
    
    return ORTE_SUCCESS;
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

    if (NULL != tm_task_ids) {
        free(tm_task_ids);
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


static int plm_tmd_disconnect(void)
{
    tm_finalize();

    return ORTE_SUCCESS;
}
