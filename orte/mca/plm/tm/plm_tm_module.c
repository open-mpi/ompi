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
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rmaps/rmaps.h"

#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/plm_private.h"
#include "plm_tm.h"



/*
 * Local functions
 */
static int plm_tm_init(void);
static int plm_tm_launch_job(orte_job_t *jdata);
static int plm_tm_terminate_job(orte_jobid_t jobid);
static int plm_tm_terminate_orteds(void);
static int plm_tm_signal_job(orte_jobid_t jobid, int32_t signal);
static int plm_tm_finalize(void);

static int plm_tm_connect(void);
static int plm_tm_disconnect(void);

/*
 * Global variable
 */
orte_plm_base_module_t orte_plm_tm_module = {
    plm_tm_init,
    orte_plm_base_set_hnp_name,
    plm_tm_launch_job,
    plm_tm_terminate_job,
    plm_tm_terminate_orteds,
    plm_tm_signal_job,
    plm_tm_finalize
};

/**
* Init the module
 */
static int plm_tm_init(void)
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
static int plm_tm_launch_job(orte_job_t *jdata)
{
    orte_job_map_t *map = NULL;
    orte_app_context_t **apps;
    orte_node_t **nodes;
    int node_name_index;
    int proc_vpid_index;
    char *param;
    char **env = NULL;
    char *var;
    char **argv = NULL;
    int argc;
    int rc;
    bool connected = false;
    orte_std_cntr_t launched = 0, i; 
    char *bin_base = NULL, *lib_base = NULL;
    tm_event_t *tm_events = NULL;
    tm_task_id *tm_task_ids = NULL;
    int local_err;
    tm_event_t event;
    struct timeval launchstart, launchstop, completionstart, completionstop;
    struct timeval jobstart, jobstop;
    int maxtime=0, mintime=99999999, maxiter = 0, miniter = 0, deltat;
    float avgtime=0.0;
    bool failed_launch = true;
    mode_t current_umask;
    
    
    /* check for timing request - get start time if so */
    if (orte_timing) {
        if (0 != gettimeofday(&jobstart, NULL)) {
            opal_output(0, "plm_tm: could not obtain job start time");
        }
    }
    
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

    if (0 == map->num_new_daemons) {
        /* have all the daemons we need - launch app */
        goto launch_apps;
    }
    
    /* Allocate a bunch of TM events to use for tm_spawn()ing */
    tm_events = malloc(sizeof(tm_event_t) * map->num_new_daemons);
    if (NULL == tm_events) {
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    tm_task_ids = malloc(sizeof(tm_task_id) * map->num_new_daemons);
    if (NULL == tm_task_ids) {
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* add the daemon command (as specified by user) */
    argv = opal_argv_split(mca_plm_tm_component.orted, ' ');
    argc = opal_argv_count(argv);

    /* Add basic orted command line options */
    orte_plm_base_orted_append_basic_args(&argc, &argv, "env",
                                          &proc_vpid_index,
                                          &node_name_index);

    if (0 < opal_output_get_verbosity(orte_plm_globals.output)) {
        param = opal_argv_join(argv, ' ');
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:tm: final top-level argv:\n\t%s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (NULL == param) ? "NULL" : param));
        if (NULL != param) free(param);
    }

    rc = plm_tm_connect();
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
    env = opal_argv_copy(environ);

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
 
        /* setup node name */
        free(argv[node_name_index]);
        argv[node_name_index] = strdup(node->name);
        
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:tm: launching on node %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             node->name));
        
        /* setup process name */
        rc = orte_util_convert_vpid_to_string(&vpid_string, nodes[i]->daemon->name.vpid);
        if (ORTE_SUCCESS != rc) {
            opal_output(0, "plm:tm: unable to get daemon vpid as string");
            exit(-1);
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
        
        /* check for timing request - get start time if so */
        if (orte_timing) {
            if (0 != gettimeofday(&launchstart, NULL)) {
                opal_output(0, "plm_tm: could not obtain start time");
                launchstart.tv_sec = 0;
                launchstart.tv_usec = 0;
            }
        }
        
        rc = tm_spawn(argc, argv, env, node->launch_id, tm_task_ids + launched, tm_events + launched);
        if (TM_SUCCESS != rc) {
            opal_show_help("help-plm-tm.txt", "tm-spawn-failed",
                           true, argv[0], node->name, node->launch_id);
            rc = ORTE_ERROR;
            goto cleanup;
        }
        
        /* check for timing request - get stop time and process if so */
        if (orte_timing) {
            if (0 != gettimeofday(&launchstop, NULL)) {
                opal_output(0, "plm_tm: could not obtain stop time");
            } else {
                deltat = (launchstop.tv_sec - launchstart.tv_sec)*1000000 +
                         (launchstop.tv_usec - launchstart.tv_usec);
                avgtime = avgtime + deltat / map->num_new_daemons;
                if (deltat < mintime) {
                    mintime = deltat;
                    miniter = launched;
                }
                if (deltat > maxtime) {
                    maxtime = deltat;
                    maxiter = launched;
                }
            }
        }
        
        launched++;

        /* Allow some progress to occur */
        opal_event_loop(OPAL_EVLOOP_NONBLOCK);
    }

    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:tm:launch: finished spawning orteds",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* check for timing request - get start time for launch completion */
    if (orte_timing) {
        if (0 != gettimeofday(&completionstart, NULL)) {
            opal_output(0, "plm_tm: could not obtain completion start time");
            completionstart.tv_sec = 0;
            completionstart.tv_usec = 0;
        }
    }
    
    /* TM poll for all the spawns */
    for (i = 0; i < launched; ++i) {
        rc = tm_poll(TM_NULL_EVENT, &event, 1, &local_err);
        if (TM_SUCCESS != rc) {
            errno = local_err;
            opal_output(0, "plm:tm: failed to poll for a spawned daemon, return status = %d", rc);
            goto cleanup;
        }
    }
    
    /* wait for daemons to callback */
    if (ORTE_SUCCESS != (rc = orte_plm_base_daemon_callback(map->num_new_daemons))) {
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:tm: daemon launch failed for job %s on error %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(jdata->jobid), ORTE_ERROR_NAME(rc)));
        goto cleanup;
    }
    
    /* check for timing request - get stop time for launch completion and report */
    if (orte_timing) {
        if (0 != gettimeofday(&completionstop, NULL)) {
            opal_output(0, "plm_tm: could not obtain completion stop time");
        } else {
            deltat = (completionstop.tv_sec - jobstart.tv_sec)*1000000 +
            (completionstop.tv_usec - completionstop.tv_usec);
            opal_output(0, "plm_tm: time to launch/wireup all daemons: %d usec", deltat);
        }
        opal_output(0, "plm_tm: Launch statistics:");
        opal_output(0, "plm_tm: Average time to launch an orted: %f usec", avgtime);
        opal_output(0, "plm_tm: Max time to launch an orted: %d usec at iter %d", maxtime, maxiter);
        opal_output(0, "plm_tm: Min time to launch an orted: %d usec at iter %d", mintime, miniter);
    }
    
launch_apps:
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
    
    if (connected) {
        plm_tm_disconnect();
    }
    if (NULL != tm_events) {
        free(tm_events);
    }
    if (NULL != tm_task_ids) {
        free(tm_task_ids);
    }
    
    if (NULL != lib_base) {
        free(lib_base);
    }
    if (NULL != bin_base) {
        free(bin_base);
    }

    /* check for failed launch - if so, force terminate */
    if (failed_launch) {
        orte_plm_base_launch_failed(jdata->jobid, true, -1, 0, ORTE_JOB_STATE_FAILED_TO_START);
    }
        
    /* check for timing request - get stop time and process if so */
    if (orte_timing) {
        if (0 != gettimeofday(&jobstop, NULL)) {
            opal_output(0, "plm_tm: could not obtain stop time");
        } else {
            deltat = (jobstop.tv_sec - jobstart.tv_sec)*1000000 +
                     (jobstop.tv_usec - jobstart.tv_usec);
            opal_output(0, "plm_tm: launch of entire job required %d usec", deltat);
        }
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:tm:launch: finished",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    return rc;
}


static int plm_tm_terminate_job(orte_jobid_t jobid)
{
    int rc;
    
   /* order all of the daemons to kill their local procs for this job */
    if (ORTE_SUCCESS != (rc = orte_plm_base_orted_kill_local_procs(jobid))) {
        ORTE_ERROR_LOG(rc);
    }

    return rc;
}


/**
 * Terminate the orteds for a given job
 */
int plm_tm_terminate_orteds(void)
{
    int rc;
    
    /* now tell them to die! */
    if (ORTE_SUCCESS != (rc = orte_plm_base_orted_exit())) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

static int plm_tm_signal_job(orte_jobid_t jobid, int32_t signal)
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
static int plm_tm_finalize(void)
{
    int rc;
    
    /* cleanup any pending recvs */
    if (ORTE_SUCCESS != (rc = orte_plm_base_comm_stop())) {
        ORTE_ERROR_LOG(rc);
    }

    return ORTE_SUCCESS;
}


static int plm_tm_connect(void)
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


static int plm_tm_disconnect(void)
{
    tm_finalize();

    return ORTE_SUCCESS;
}
