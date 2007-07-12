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
#include "orte/orte_constants.h"

#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <signal.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
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

#include "orte/orte_types.h"
#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_wakeup.h"
#include "orte/mca/pls/pls.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/smr/smr.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/sds/base/base.h"
#include "orte/mca/rmaps/rmaps.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/ns/ns.h"

#include "orte/mca/pls/base/base.h"
#include "orte/mca/pls/base/pls_private.h"
#include "pls_tm.h"



/*
 * Local functions
 */
static int pls_tm_launch_job(orte_jobid_t jobid);
static int pls_tm_terminate_job(orte_jobid_t jobid, struct timeval *timeout, opal_list_t *attrs);
static int pls_tm_terminate_orteds(struct timeval *timeout, opal_list_t *attrs);
static int pls_tm_terminate_proc(const orte_process_name_t *name);
static int pls_tm_signal_job(orte_jobid_t jobid, int32_t signal, opal_list_t *attrs);
static int pls_tm_signal_proc(const orte_process_name_t *name, int32_t signal);
static int pls_tm_finalize(void);

static int pls_tm_connect(void);
static int pls_tm_disconnect(void);

/*
 * Local variables
 */


/*
 * Global variable
 */
orte_pls_base_module_t orte_pls_tm_module = {
    pls_tm_launch_job,
    pls_tm_terminate_job,
    pls_tm_terminate_orteds,
    pls_tm_terminate_proc,
    pls_tm_signal_job,
    pls_tm_signal_proc,
    pls_tm_finalize
};

/* When working in this function, ALWAYS jump to "cleanup" if
 * you encounter an error so that orterun will be woken up and
 * the job can cleanly terminate
 */
static int pls_tm_launch_job(orte_jobid_t jobid)
{
    orte_job_map_t *map = NULL;
    opal_list_item_t *item;
    size_t num_nodes;
    int node_name_index;
    int proc_name_index;
    char *param;
    char **env = NULL;
    char *var;
    char **argv = NULL;
    int argc;
    int rc;
    bool connected = false;
    uint launched = 0, i; 
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
    
    /* check for timing request - get start time if so */
    if (mca_pls_tm_component.timing) {
        if (0 != gettimeofday(&jobstart, NULL)) {
            opal_output(0, "pls_tm: could not obtain job start time");
        }
    }
    
    /* Query the map for this job.
     * We need the entire mapping for a couple of reasons:
     *  - need the prefix to start with.
     *  - need to know if we are launching on a subset of the allocated nodes
     */
    rc = orte_rmaps.get_job_map(&map, jobid);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* Iterate through each of the nodes and check to see if we have
     * a valid launch_id (must be > 0). If not, then error out as
     * we cannot do anything
     */
    for (item =  opal_list_get_first(&map->nodes);
         item != opal_list_get_end(&map->nodes);
         item =  opal_list_get_next(item)) {
        orte_mapped_node_t* node = (orte_mapped_node_t*)item;
        
        if (node->launch_id < 0) {
            opal_show_help("help-pls-tm.txt", "tm-bad-launchid",
                           true, node->nodename, node->launch_id);
            goto cleanup;
        }
    }    
        
    num_nodes = map->num_new_daemons;
    if (0 == num_nodes) {
        /* have all the daemons we need - launch app */
        goto launch_apps;
    }
    
    /* Allocate a bunch of TM events to use for tm_spawn()ing */
    tm_events = malloc(sizeof(tm_event_t) * num_nodes);
    if (NULL == tm_events) {
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    tm_task_ids = malloc(sizeof(tm_task_id) * num_nodes);
    if (NULL == tm_task_ids) {
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* add the daemon command (as specified by user) */
    argv = opal_argv_split(mca_pls_tm_component.orted, ' ');
    argc = opal_argv_count(argv);

    opal_argv_append(&argc, &argv, "--no-daemonize");
    
    /* Add basic orted command line options */
    orte_pls_base_orted_append_basic_args(&argc, &argv,
                                          &proc_name_index,
                                          &node_name_index,
                                          map->num_nodes);

    if (mca_pls_tm_component.debug) {
        param = opal_argv_join(argv, ' ');
        if (NULL != param) {
            opal_output(0, "pls:tm: final top-level argv:");
            opal_output(0, "pls:tm:     %s", param);
            free(param);
        }
    }

    rc = pls_tm_connect();
    if (ORTE_SUCCESS != rc) {
        goto cleanup;
    }
    connected = true;

    /* Figure out the basenames for the libdir and bindir.  There is a
       lengthy comment about this in pls_rsh_module.c explaining all
       the rationale for how / why we're doing this. */
    lib_base = opal_basename(opal_install_dirs.libdir);
    bin_base = opal_basename(opal_install_dirs.bindir);

    /* setup environment */
    env = opal_argv_copy(environ);
    var = mca_base_param_environ_variable("seed",NULL,NULL);
    opal_setenv(var, "0", true, &env);

    /* clean out any MCA component selection directives that
     * won't work on remote nodes
     */
    orte_pls_base_purge_mca_params(&env);
    
    /* If we have a prefix, then modify the PATH and
        LD_LIBRARY_PATH environment variables. We only allow
        a single prefix to be specified. Since there will
        always be at least one app_context, we take it from
        there
    */
    if (NULL != map->apps[0]->prefix_dir) {
        char *newenv;
        
        for (i = 0; NULL != env && NULL != env[i]; ++i) {
            /* Reset PATH */
            if (0 == strncmp("PATH=", env[i], 5)) {
                asprintf(&newenv, "%s/%s:%s", 
                            map->apps[0]->prefix_dir, bin_base, env[i] + 5);
                if (mca_pls_tm_component.debug) {
                    opal_output(0, "pls:tm: resetting PATH: %s", 
                                newenv);
                }
                opal_setenv("PATH", newenv, true, &env);
                free(newenv);
            } 
            
            /* Reset LD_LIBRARY_PATH */
            else if (0 == strncmp("LD_LIBRARY_PATH=", env[i], 16)) {
                asprintf(&newenv, "%s/%s:%s", 
                            map->apps[0]->prefix_dir, lib_base, env[i] + 16);
                if (mca_pls_tm_component.debug) {
                    opal_output(0, "pls:tm: resetting LD_LIBRARY_PATH: %s", 
                                newenv);
                }
                opal_setenv("LD_LIBRARY_PATH", newenv, true, &env);
                free(newenv);
            } 
        }
    }
    
    /* Iterate through each of the nodes and spin
     * up a daemon.
     */
    for (item =  opal_list_get_first(&map->nodes);
         item != opal_list_get_end(&map->nodes);
         item =  opal_list_get_next(item)) {
        orte_mapped_node_t* node = (orte_mapped_node_t*)item;
        char* name_string;
        
        /* if this daemon already exists, don't launch it! */
        if (node->daemon_preexists) {
            continue;
        }
 
        /* setup node name */
        free(argv[node_name_index]);
        argv[node_name_index] = strdup(node->nodename);
        
        /* setup per-node options */
        if (mca_pls_tm_component.debug ||
            mca_pls_tm_component.verbose) {
            opal_output(0, "pls:tm: launching on node %s", 
                        node->nodename);
        }
        
        /* setup process name */
        rc = orte_ns.get_proc_name_string(&name_string, node->daemon);
        if (ORTE_SUCCESS != rc) {
            opal_output(0, "pls:tm: unable to create process name");
            goto cleanup;
        }
        free(argv[proc_name_index]);
        argv[proc_name_index] = strdup(name_string);
    
        /* exec the daemon */
        if (mca_pls_tm_component.debug) {
            param = opal_argv_join(argv, ' ');
            if (NULL != param) {
                opal_output(0, "pls:tm: executing: %s", param);
                free(param);
            }
        }
        
        /* check for timing request - get start time if so */
        if (mca_pls_tm_component.timing) {
            if (0 != gettimeofday(&launchstart, NULL)) {
                opal_output(0, "pls_tm: could not obtain start time");
                launchstart.tv_sec = 0;
                launchstart.tv_usec = 0;
            }
        }
        
        rc = tm_spawn(argc, argv, env, node->launch_id, tm_task_ids + launched, tm_events + launched);
        if (TM_SUCCESS != rc) {
            opal_show_help("help-pls-tm.txt", "tm-spawn-failed",
                           true, argv[0], node->nodename, node->launch_id);
            rc = ORTE_ERROR;
            goto cleanup;
        }
        
        /* check for timing request - get stop time and process if so */
        if (mca_pls_tm_component.timing) {
            if (0 != gettimeofday(&launchstop, NULL)) {
                opal_output(0, "pls_tm: could not obtain stop time");
            } else {
                deltat = (launchstop.tv_sec - launchstart.tv_sec)*1000000 +
                         (launchstop.tv_usec - launchstart.tv_usec);
                avgtime = avgtime + deltat / num_nodes;
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
    if (mca_pls_tm_component.debug) {
        opal_output(0, "pls:tm:launch: finished spawning orteds\n");
    }

    /* check for timing request - get start time for launch completion */
    if (mca_pls_tm_component.timing) {
        if (0 != gettimeofday(&completionstart, NULL)) {
            opal_output(0, "pls_tm: could not obtain completion start time");
            completionstart.tv_sec = 0;
            completionstart.tv_usec = 0;
        }
    }
    
    /* TM poll for all the spawns */
    for (i = 0; i < launched; ++i) {
        rc = tm_poll(TM_NULL_EVENT, &event, 1, &local_err);
        if (TM_SUCCESS != rc) {
            errno = local_err;
            opal_output(0, "pls:tm: failed to poll for a spawned proc, return status = %d", rc);
            goto cleanup;
        }
    }
    
    /* wait for daemons to callback */
    if (ORTE_SUCCESS != (rc = orte_pls_base_daemon_callback(map->num_new_daemons))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
launch_apps:
    if (ORTE_SUCCESS != (rc = orte_pls_base_launch_apps(map))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* if we get here, then everything launched okay - record that fact */
    failed_launch = false;
    
    /* check for timing request - get stop time for launch completion and report */
    if (mca_pls_tm_component.timing) {
        if (0 != gettimeofday(&completionstop, NULL)) {
            opal_output(0, "pls_tm: could not obtain completion stop time");
        } else {
            deltat = (launchstop.tv_sec - launchstart.tv_sec)*1000000 +
                     (launchstop.tv_usec - launchstart.tv_usec);
            opal_output(0, "pls_tm: launch completion required %d usec", deltat);
        }
        opal_output(0, "pls_tm: Launch statistics:");
        opal_output(0, "pls_tm: Average time to launch an orted: %f usec", avgtime);
        opal_output(0, "pls_tm: Max time to launch an orted: %d usec at iter %d", maxtime, maxiter);
        opal_output(0, "pls_tm: Min time to launch an orted: %d usec at iter %d", mintime, miniter);
    }
    
    
 cleanup:
    if (NULL != map) {
        OBJ_RELEASE(map);
    }
    if (NULL != argv) {
        opal_argv_free(argv);
    }
    if (NULL != env) {
        opal_argv_free(env);
    }
    
    if (connected) {
        pls_tm_disconnect();
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
        if (ORTE_SUCCESS != (rc = orte_smr.set_job_state(jobid, ORTE_JOB_STATE_FAILED_TO_START))) {
            ORTE_ERROR_LOG(rc);
        }
        
        if (ORTE_SUCCESS != (rc = orte_wakeup(jobid))) {
            ORTE_ERROR_LOG(rc);
        }        
    }
        
    /* check for timing request - get stop time and process if so */
    if (mca_pls_tm_component.timing) {
        if (0 != gettimeofday(&jobstop, NULL)) {
            opal_output(0, "pls_tm: could not obtain stop time");
        } else {
            deltat = (jobstop.tv_sec - jobstart.tv_sec)*1000000 +
                     (jobstop.tv_usec - jobstart.tv_usec);
            opal_output(0, "pls_tm: launch of entire job required %d usec", deltat);
        }
    }
    
    if (mca_pls_tm_component.debug) {
        opal_output(0, "pls:tm:launch: finished\n");
    }
    return rc;
}


static int pls_tm_terminate_job(orte_jobid_t jobid, struct timeval *timeout, opal_list_t *attrs)
{
    int rc;
    
   /* order all of the daemons to kill their local procs for this job */
    if (ORTE_SUCCESS != (rc = orte_pls_base_orted_kill_local_procs(jobid, timeout, attrs))) {
        ORTE_ERROR_LOG(rc);
    }

    return rc;
}


/**
 * Terminate the orteds for a given job
 */
int pls_tm_terminate_orteds(struct timeval *timeout, opal_list_t *attrs)
{
    int rc;
    
    /* now tell them to die! */
    if (ORTE_SUCCESS != (rc = orte_pls_base_orted_exit(timeout, attrs))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

/*
 * TM can't kill individual processes -- PBS will kill the entire job
 */
static int pls_tm_terminate_proc(const orte_process_name_t *name)
{
    if (mca_pls_tm_component.debug) {
        opal_output(0, "pls:tm:terminate_proc: not supported");
    }
    return ORTE_ERR_NOT_SUPPORTED;
}


static int pls_tm_signal_job(orte_jobid_t jobid, int32_t signal, opal_list_t *attrs)
{
    int rc;
    
    /* order them to pass this signal to their local procs */
    if (ORTE_SUCCESS != (rc = orte_pls_base_orted_signal_local_procs(jobid, signal, attrs))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}


static int pls_tm_signal_proc(const orte_process_name_t *name, int32_t signal)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}


/*
 * Free stuff
 */
static int pls_tm_finalize(void)
{
    int rc;
    
    /* cleanup any pending recvs */
    if (ORTE_SUCCESS != (rc = orte_pls_base_comm_stop())) {
        ORTE_ERROR_LOG(rc);
    }

    return ORTE_SUCCESS;
}


static int pls_tm_connect(void)
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


static int pls_tm_disconnect(void)
{
    tm_finalize();

    return ORTE_SUCCESS;
}
