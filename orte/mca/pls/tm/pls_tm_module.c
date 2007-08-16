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

#include "orte/orte_types.h"
#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_wait.h"
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
static int pls_tm_terminate_orteds(orte_jobid_t jobid, struct timeval *timeout, opal_list_t *attrs);
static int pls_tm_terminate_proc(const orte_process_name_t *name);
static int pls_tm_signal_job(orte_jobid_t jobid, int32_t signal, opal_list_t *attrs);
static int pls_tm_signal_proc(const orte_process_name_t *name, int32_t signal);
static int pls_tm_cancel_operation(void);
static int pls_tm_finalize(void);

static int pls_tm_connect(void);
static int pls_tm_disconnect(void);
static int pls_tm_check_path(char *exe, char **env);

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
    pls_tm_cancel_operation,
    pls_tm_finalize
};

static int pls_tm_launch_job(orte_jobid_t jobid)
{
    orte_job_map_t *map;
    opal_list_item_t *item;
    size_t num_nodes;
    orte_vpid_t vpid;
    int node_name_index;
    int proc_name_index;
    char *jobid_string;
    char *uri, *param;
    char **env;
    char *var;
    char **argv;
    int argc;
    int rc;
    bool connected = false;
    uint launched = 0, i; 
    char *bin_base = NULL, *lib_base = NULL;
    tm_event_t *tm_events = NULL;
    tm_task_id *tm_task_ids = NULL;
    int local_err;
    tm_event_t event;
    opal_list_t daemons;
    orte_pls_daemon_info_t *dmn;
    struct timeval launchstart, launchstop, completionstart, completionstop;
    struct timeval jobstart, jobstop;
    int maxtime=0, mintime=99999999, maxiter = 0, miniter = 0, deltat;
    float avgtime=0.0;
    mode_t current_umask;
    
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
        return rc;
    }

    /* if the user requested that we re-use daemons,
     * launch the procs on any existing, re-usable daemons
     */
    if (orte_pls_base.reuse_daemons) {
        if (ORTE_SUCCESS != (rc = orte_pls_base_launch_on_existing_daemons(map))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(map);
            return rc;
        }
    }
    
    num_nodes = opal_list_get_size(&map->nodes);
    if (0 == num_nodes) {
        /* must have been launched on existing daemons - just return */
        OBJ_RELEASE(map);
        return ORTE_SUCCESS;
    }
    
    /*
     * Allocate a range of vpids for the daemons.
     */
    rc = orte_ns.reserve_range(0, num_nodes, &vpid);
    if (ORTE_SUCCESS != rc) {
        goto cleanup;
    }

    /* setup the orted triggers for passing their launch info */
    if (ORTE_SUCCESS != (rc = orte_smr.init_orted_stage_gates(jobid, num_nodes, NULL, NULL))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* setup a list that will contain the info for all the daemons
     * so we can store it on the registry when done
     */
    OBJ_CONSTRUCT(&daemons, opal_list_t);
    
    /* Allocate a bunch of TM events to use for tm_spawn()ing */
    tm_events = malloc(sizeof(tm_event_t) * num_nodes);
    if (NULL == tm_events) {
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }
    tm_task_ids = malloc(sizeof(tm_task_id) * num_nodes);
    if (NULL == tm_task_ids) {
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }

    /* need integer value for command line parameter */
    asprintf(&jobid_string, "%lu", (unsigned long) jobid);

    /* add the daemon command (as specified by user) */
    argv = opal_argv_split(mca_pls_tm_component.orted, ' ');
    argc = opal_argv_count(argv);

    opal_argv_append(&argc, &argv, "--no-daemonize");
    
    /* check for debug flags */
    orte_pls_base_mca_argv(&argc, &argv);

    /* proxy information */
    opal_argv_append(&argc, &argv, "--bootproxy");
    opal_argv_append(&argc, &argv, jobid_string);
    opal_argv_append(&argc, &argv, "--name");
    proc_name_index = argc;
    opal_argv_append(&argc, &argv, "");

    /* tell the daemon how many procs are in the daemon's job */
    opal_argv_append(&argc, &argv, "--num_procs");
    asprintf(&param, "%lu", (unsigned long)(vpid + num_nodes));
    opal_argv_append(&argc, &argv, param);
    free(param);

    /* tell the daemon the starting vpid of the daemon's job */
    opal_argv_append(&argc, &argv, "--vpid_start");
    opal_argv_append(&argc, &argv, "0");
    
    opal_argv_append(&argc, &argv, "--nodename");
    node_name_index = argc;
    opal_argv_append(&argc, &argv, "");

    /* pass along the universe name and location info */
    opal_argv_append(&argc, &argv, "--universe");
    asprintf(&param, "%s@%s:%s", orte_universe_info.uid,
                orte_universe_info.host, orte_universe_info.name);
    opal_argv_append(&argc, &argv, param);
    free(param);
    
    /* setup ns contact info */
    opal_argv_append(&argc, &argv, "--nsreplica");
    if (NULL != orte_process_info.ns_replica_uri) {
        uri = strdup(orte_process_info.ns_replica_uri);
    } else {
        uri = orte_rml.get_uri();
    }
    asprintf(&param, "\"%s\"", uri);
    opal_argv_append(&argc, &argv, param);
    free(uri);
    free(param);

    /* setup gpr contact info */
    opal_argv_append(&argc, &argv, "--gprreplica");
    if (NULL != orte_process_info.gpr_replica_uri) {
        uri = strdup(orte_process_info.gpr_replica_uri);
    } else {
        uri = orte_rml.get_uri();
    }
    asprintf(&param, "\"%s\"", uri);
    opal_argv_append(&argc, &argv, param);
    free(uri);
    free(param);

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
    
    /* Do a quick sanity check to ensure that we can find the
        orted in the PATH */
    
    if (ORTE_SUCCESS != 
        (rc = pls_tm_check_path(argv[0], env))) {
        ORTE_ERROR_LOG(rc);
        opal_show_help("help-pls-tm.txt", "daemon-not-found",
                        true, argv[0]);
        goto cleanup;
    }
        
    /* Iterate through each of the nodes and spin
     * up a daemon.
     */
    for (item =  opal_list_get_first(&map->nodes);
         item != opal_list_get_end(&map->nodes);
         item =  opal_list_get_next(item)) {
        orte_mapped_node_t* node = (orte_mapped_node_t*)item;
        orte_process_name_t* name;
        char* name_string;
        
        /* new daemon - setup to record its info */
        dmn = OBJ_NEW(orte_pls_daemon_info_t);
        dmn->active_job = jobid;
        opal_list_append(&daemons, &dmn->super);
        
        /* setup node name */
        free(argv[node_name_index]);
        argv[node_name_index] = strdup(node->nodename);
        
        /* record the node name in the daemon struct */
        dmn->cell = node->cell;
        dmn->nodename = strdup(node->nodename);
        
        /* initialize daemons process name */
        rc = orte_ns.create_process_name(&name, node->cell, 0, vpid);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        
        /* save it in the daemon struct */
        if (ORTE_SUCCESS != (rc = orte_dss.copy((void**)&(dmn->name), name, ORTE_NAME))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        
        /* setup per-node options */
        if (mca_pls_tm_component.debug ||
            mca_pls_tm_component.verbose) {
            opal_output(0, "pls:tm: launching on node %s", 
                        node->nodename);
        }
        
        /* setup process name */
        rc = orte_ns.get_proc_name_string(&name_string, name);
        if (ORTE_SUCCESS != rc) {
            opal_output(0, "pls:tm: unable to create process name");
            return rc;
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
            return ORTE_ERROR;
        }
        
        if (ORTE_SUCCESS != rc) {
            opal_output(0, "pls:tm: start_procs returned error %d", rc);
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
        ++vpid;
        free(name);

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
    
    /* all done, so store the daemon info on the registry */
    if (ORTE_SUCCESS != (rc = orte_pls_base_store_active_daemons(&daemons))) {
        ORTE_ERROR_LOG(rc);
    }
    
    /* TM poll for all the spawns */
    for (i = 0; i < launched; ++i) {
        rc = tm_poll(TM_NULL_EVENT, &event, 1, &local_err);
        if (TM_SUCCESS != rc) {
            errno = local_err;
            opal_output(0, "pls:tm: failed to poll for a spawned proc, return status = %d", rc);
            return ORTE_ERR_IN_ERRNO;
        }
    }
    
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
    OBJ_RELEASE(map);
    
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

    /* deconstruct the daemon list */
    while (NULL != (item = opal_list_remove_first(&daemons))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&daemons);

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
    opal_list_t daemons;
    opal_list_item_t *item;
    
    /* construct the list of active daemons on this job */
    OBJ_CONSTRUCT(&daemons, opal_list_t);
    if (ORTE_SUCCESS != (rc = orte_pls_base_get_active_daemons(&daemons, jobid, attrs))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* order them to kill their local procs for this job */
    if (ORTE_SUCCESS != (rc = orte_pls_base_orted_kill_local_procs(&daemons, jobid, timeout))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
CLEANUP:
    while (NULL != (item = opal_list_remove_first(&daemons))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&daemons);
    return rc;
}


/**
 * Terminate the orteds for a given job
 */
int pls_tm_terminate_orteds(orte_jobid_t jobid, struct timeval *timeout, opal_list_t *attrs)
{
    int rc;
    opal_list_t daemons;
    opal_list_item_t *item;
    
    /* construct the list of active daemons on this job */
    OBJ_CONSTRUCT(&daemons, opal_list_t);
    if (ORTE_SUCCESS != (rc = orte_pls_base_get_active_daemons(&daemons, jobid, attrs))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* now tell them to die! */
    if (ORTE_SUCCESS != (rc = orte_pls_base_orted_exit(&daemons, timeout))) {
        ORTE_ERROR_LOG(rc);
    }
    
CLEANUP:
    while (NULL != (item = opal_list_remove_first(&daemons))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&daemons);
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
    opal_list_t daemons;
    opal_list_item_t *item;
    
    /* construct the list of active daemons on this job */
    OBJ_CONSTRUCT(&daemons, opal_list_t);
    if (ORTE_SUCCESS != (rc = orte_pls_base_get_active_daemons(&daemons, jobid, attrs))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&daemons);
        return rc;
    }
    
    /* order them to pass this signal to their local procs */
    if (ORTE_SUCCESS != (rc = orte_pls_base_orted_signal_local_procs(&daemons, signal))) {
        ORTE_ERROR_LOG(rc);
    }
    
    while (NULL != (item = opal_list_remove_first(&daemons))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&daemons);
    return rc;
}


static int pls_tm_signal_proc(const orte_process_name_t *name, int32_t signal)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}


/**
 * Cancel an operation involving comm to an orted
 */
static int pls_tm_cancel_operation(void)
{
    int rc;

    if (ORTE_SUCCESS != (rc = orte_pls_base_orted_cancel_operation())) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
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


static int pls_tm_check_path(char *exe, char **env)
{
    static int size = 256;
    int i;
    char *file;
    char *cwd;
    char *path = NULL;

    /* Do we want this check at all? */

    if (!mca_pls_tm_component.want_path_check) {
        return ORTE_SUCCESS;
    }

    /* Find the path in the supplied environment */

    for (i = 0; NULL != env[i]; ++i) {
        if (0 == strncmp("PATH=", env[i], 5)) {
            path = strdup(env[i]);
            break;
        }
    }
    if (NULL == env[i]) {
        path = strdup("NULL");
    }

    /* Check the already-successful paths (i.e., be a little
       friendlier to the filesystem -- if we find the executable
       successfully, save it) */

    for (i = 0; NULL != mca_pls_tm_component.checked_paths &&
             NULL != mca_pls_tm_component.checked_paths[i]; ++i) {
        if (0 == strcmp(path, mca_pls_tm_component.checked_paths[i])) {
            return ORTE_SUCCESS;
        }
    }

    /* We didn't already find it, so check now.  First, get the cwd. */

    do {
        cwd = malloc(size);
        if (NULL == cwd) {
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        if (NULL == getcwd(cwd, size)) {
            free(cwd);
            if (ERANGE == errno) {
                size *= 2;
            } else {
                return ORTE_ERR_IN_ERRNO;
            }
        } else {
            break;
        }
    } while (1);

    /* Now do the search */

    file = opal_path_findv(exe, X_OK, env, cwd);
    free(cwd);
    if (NULL == file) {
        free(path);
        return ORTE_ERR_NOT_FOUND;
    }
    if (mca_pls_tm_component.debug) {
        opal_output(0, "pls:tm: found %s", file);
    }
    free(file);

    /* Success -- so cache it */

    opal_argv_append_nosize(&mca_pls_tm_component.checked_paths, path);

    /* All done */

    free(path);
    return ORTE_SUCCESS;
}
