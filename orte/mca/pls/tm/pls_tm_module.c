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
#include <sys/types.h>
#include <sys/wait.h>
#ifdef HAVE_SCHED_H
#include <sched.h>
#endif
#include <errno.h>
#include <tm.h>

#include "opal/install_dirs.h"
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
#include "orte/mca/rml/rml.h"
#include "orte/mca/ns/ns.h"

/* needs to be cleaned up for ORTE 2.0 */
#include "orte/mca/rmaps/base/rmaps_private.h"


#include "orte/mca/pls/base/pls_private.h"
#include "pls_tm.h"



/*
 * Local functions
 */
static int pls_tm_launch_job(orte_jobid_t jobid);
static int pls_tm_terminate_job(orte_jobid_t jobid);
static int pls_tm_terminate_orteds(orte_jobid_t jobid);
static int pls_tm_terminate_proc(const orte_process_name_t *name);
static int pls_tm_signal_job(orte_jobid_t jobid, int32_t signal);
static int pls_tm_signal_proc(const orte_process_name_t *name, int32_t signal);
static int pls_tm_finalize(void);

static int pls_tm_connect(void);
static int pls_tm_disconnect(void);
static int pls_tm_query_hostnames(void);
static int pls_tm_start_proc(char *nodename, int argc, char **argv, 
                             char **env, tm_task_id *task_id, 
                             tm_event_t *event);
static int pls_tm_check_path(char *exe, char **env);

/*
 * Local variables
 */
/* Resolving TM hostname */
static char **tm_hostnames = NULL;
static tm_node_id *tm_node_ids = NULL;
static int num_tm_hostnames = 0, num_node_ids = 0;



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

#if !defined(__WINDOWS__)
extern char **environ;
#endif  /* !defined(__WINDOWS__) */

static int pls_tm_launch_job(orte_jobid_t jobid)
{
    opal_list_t mapping;
    opal_list_item_t *m_item, *n_item;
    size_t num_nodes;
    orte_vpid_t vpid;
    int node_name_index;
    int proc_name_index;
    char *jobid_string;
    char *uri, *param;
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

   /* Query the list of nodes allocated and mapped to this job.
     * We need the entire mapping for a couple of reasons:
     *  - need the prefix to start with.
     *  - need to know if we are launching on a subset of the allocated nodes
     */
    OBJ_CONSTRUCT(&mapping, opal_list_t);
    rc = orte_rmaps_base_get_map(jobid, &mapping);
    if (ORTE_SUCCESS != rc) {
        goto cleanup;
    }

    num_nodes = 0;
    for(m_item = opal_list_get_first(&mapping);
        m_item != opal_list_get_end(&mapping);
        m_item = opal_list_get_next(m_item)) {
        orte_rmaps_base_map_t* map = (orte_rmaps_base_map_t*)m_item;
        num_nodes += opal_list_get_size(&map->nodes);
    }

    /*
     * Allocate a range of vpids for the daemons.
     */
    if (0 == num_nodes) {
        return ORTE_ERR_BAD_PARAM;
    }
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

    /* Resolve the TM hostnames and TD node ID's (guarantee that we
       don't mix any of these TM events in with the TM spawn events,
       so that we can poll for each set of events without interference
       from the other */
    rc = pls_tm_query_hostnames();
    if (ORTE_SUCCESS != rc) {
        goto cleanup;
    }

    /* Figure out the basenames for the libdir and bindir.  There is a
       lengthy comment about this in pls_rsh_module.c explaining all
       the rationale for how / why we're doing this. */
    lib_base = opal_basename(OPAL_LIBDIR);
    bin_base = opal_basename(OPAL_BINDIR);

    /*
     * iterate through each of the contexts
     */
    for (m_item = opal_list_get_first(&mapping);
         m_item != opal_list_get_end(&mapping);
         m_item = opal_list_get_next(m_item)) {
        orte_rmaps_base_map_t* map = (orte_rmaps_base_map_t*)m_item;
        char** env;
        char* var;
		
        /* setup environment */
        env = opal_argv_copy(environ);
        var = mca_base_param_environ_variable("seed",NULL,NULL);
        opal_setenv(var, "0", true, &env);

        /* If we have a prefix, then modify the PATH and
           LD_LIBRARY_PATH environment variables.  */
        if (NULL != map->app->prefix_dir) {
            char *newenv;
            
            for (i = 0; NULL != env && NULL != env[i]; ++i) {
                /* Reset PATH */
                if (0 == strncmp("PATH=", env[i], 5)) {
                    asprintf(&newenv, "%s/%s:%s", 
                             map->app->prefix_dir, bin_base, env[i] + 5);
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
                             map->app->prefix_dir, lib_base, env[i] + 16);
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
        for (n_item =  opal_list_get_first(&map->nodes);
             n_item != opal_list_get_end(&map->nodes);
             n_item =  opal_list_get_next(n_item)) {
            orte_rmaps_base_node_t* rmaps_node = (orte_rmaps_base_node_t*)n_item;
            orte_ras_node_t* node = rmaps_node->node;
            orte_process_name_t* name;
            char* name_string;
            
            /* already launched on this node */
            if (0 != node->node_launched++) {
                continue;
            }
            
            /* new daemon - setup to record its info */
            dmn = OBJ_NEW(orte_pls_daemon_info_t);
            dmn->active_job = jobid;
            opal_list_append(&daemons, &dmn->super);
            
            /* setup node name */
            free(argv[node_name_index]);
            argv[node_name_index] = strdup(node->node_name);
            
            /* record the node name in the daemon struct */
            dmn->cell = node->node_cellid;
            dmn->nodename = strdup(node->node_name);
            
            /* initialize daemons process name */
            rc = orte_ns.create_process_name(&name, node->node_cellid, 0, vpid);
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
                            node->node_name);
            }
            
            /* setup process name */
            rc = orte_ns.get_proc_name_string(&name_string, name);
            if (ORTE_SUCCESS != rc) {
                opal_output(0, "pls:tm: unable to create process name");
                return rc;
            }
            free(argv[proc_name_index]);
            argv[proc_name_index] = strdup(name_string);

            /* set the progress engine schedule for this node.
             * if node_slots is set to zero, then we default to
             * NOT being oversubscribed
             */
            if (node->node_slots > 0 &&
                (orte_std_cntr_t)opal_list_get_size(&rmaps_node->node_procs) > node->node_slots) {
                if (mca_pls_tm_component.debug) {
                    opal_output(0, "pls:tm: oversubscribed -- setting mpi_yield_when_idle to 1 (%d %d)",
                                node->node_slots, 
                                opal_list_get_size(&rmaps_node->node_procs));
                }
                var = mca_base_param_environ_variable("mpi", NULL, "yield_when_idle");
                opal_setenv(var, "1", true, &env);
            } else {
                if (mca_pls_tm_component.debug) {
                    opal_output(0, "pls:tm: not oversubscribed -- setting mpi_yield_when_idle to 0");
                }
                var = mca_base_param_environ_variable("mpi", NULL, "yield_when_idle");
                opal_setenv(var, "0", true, &env);
            }
            free(var);
	    
            /* exec the daemon */
            if (mca_pls_tm_component.debug) {
                param = opal_argv_join(argv, ' ');
                if (NULL != param) {
                    opal_output(0, "pls:tm: executing: %s", param);
                    free(param);
                }
            }
            
            rc = pls_tm_start_proc(node->node_name, argc, argv, env,
                                   tm_task_ids + launched, 
                                   tm_events + launched);
            if (ORTE_SUCCESS != rc) {
                opal_output(0, "pls:tm: start_procs returned error %d", rc);
                goto cleanup;
            }
            launched++;
            ++vpid;
            free(name);

            /* Allow some progress to occur */
            opal_event_loop(OPAL_EVLOOP_NONBLOCK);
        }
    }
    if (mca_pls_tm_component.debug) {
        opal_output(0, "pls:tm:launch: finished spawning orteds\n");
    }

    /* all done, so store the daemon info on the registry */
    if (ORTE_SUCCESS != (rc = orte_pls_base_store_active_daemons(&daemons, jobid))) {
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

 cleanup:
    if (connected) {
        pls_tm_disconnect();
    }
    if (NULL != tm_events) {
        free(tm_events);
    }
    if (NULL != tm_task_ids) {
        free(tm_task_ids);
    }
    
    while (NULL != (m_item = opal_list_remove_first(&mapping))) {
        OBJ_RELEASE(m_item);
    }
    OBJ_DESTRUCT(&mapping);
    if (NULL != lib_base) {
        free(lib_base);
    }
    if (NULL != bin_base) {
        free(bin_base);
    }

    /* deconstruct the daemon list */
    while (NULL != (m_item = opal_list_remove_first(&daemons))) {
        OBJ_RELEASE(m_item);
    }
    OBJ_DESTRUCT(&daemons);

    if (mca_pls_tm_component.debug) {
        opal_output(0, "pls:tm:launch: finished\n");
    }
    return rc;
}


static int pls_tm_terminate_job(orte_jobid_t jobid)
{
    int rc;
    opal_list_t daemons;
    opal_list_item_t *item;
    
    /* construct the list of active daemons on this job */
    OBJ_CONSTRUCT(&daemons, opal_list_t);
    if (ORTE_SUCCESS != (rc = orte_pls_base_get_active_daemons(&daemons, jobid))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* order them to kill their local procs for this job */
    if (ORTE_SUCCESS != (rc = orte_pls_base_orted_kill_local_procs(&daemons, jobid))) {
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
int pls_tm_terminate_orteds(orte_jobid_t jobid)
{
    int rc;
    opal_list_t daemons;
    opal_list_item_t *item;
    
    /* construct the list of active daemons on this job */
    OBJ_CONSTRUCT(&daemons, opal_list_t);
    if (ORTE_SUCCESS != (rc = orte_pls_base_get_active_daemons(&daemons, jobid))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* now tell them to die! */
    if (ORTE_SUCCESS != (rc = orte_pls_base_orted_exit(&daemons))) {
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


static int pls_tm_signal_job(orte_jobid_t jobid, int32_t signal)
{
    int rc;
    opal_list_t daemons;
    opal_list_item_t *item;
    
    /* construct the list of active daemons on this job */
    OBJ_CONSTRUCT(&daemons, opal_list_t);
    if (ORTE_SUCCESS != (rc = orte_pls_base_get_active_daemons(&daemons, jobid))) {
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
    
    if (NULL != tm_hostnames) {
        opal_argv_free(tm_hostnames);
        tm_hostnames = NULL;
        num_tm_hostnames = 0;
    }
    if (NULL != tm_node_ids) {
        free(tm_node_ids);
        tm_node_ids = NULL;
        num_node_ids = 0;
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


/*
 * For a given TM node ID, get the string hostname corresponding to
 * it.
 */
static char *get_tm_hostname(tm_node_id node)
{
    char *hostname;
    char buffer[256];
    int ret, local_errno;
    tm_event_t event;
    char **argv;

    /* Get the info string corresponding to this TM node ID */

    ret = tm_rescinfo(node, buffer, sizeof(buffer) - 1, &event);
    if (TM_SUCCESS != ret) {
        opal_output(0, "tm_rescinfo returned %d\n", ret);
        return NULL;
    }

    /* Now wait for that event to happen */

    ret = tm_poll(TM_NULL_EVENT, &event, 1, &local_errno);
    if (TM_SUCCESS != ret) {
        opal_output(0, "tm_poll returned %d\n", ret);
        return NULL;
    }

    /* According to the TM man page, we get back a space-separated
       string array.  The hostname is the second item.  Use a cheap
       trick to get it. */

    buffer[sizeof(buffer) - 1] = '\0';
    argv = opal_argv_split(buffer, ' ');
    if (NULL == argv) {
        opal_output(0, "opal_argv_split failed\n");
        return NULL;
    }
    hostname = strdup(argv[1]);
    opal_argv_free(argv);

    /* All done */

    return hostname;
}


static int pls_tm_query_hostnames(void)
{
    char *h;
    int i, ret;

    /* Get the list of nodes allocated in this PBS job */

    ret = tm_nodeinfo(&tm_node_ids, &num_node_ids);
    if (TM_SUCCESS != ret) {
        return ORTE_ERR_NOT_FOUND;
    }

    /* TM "nodes" may actually correspond to PBS "VCPUs", which means
       there may be multiple "TM nodes" that correspond to the same
       physical node.  This doesn't really affect what we're doing
       here (we actually ignore the fact that they're duplicates --
       slightly inefficient, but no big deal); just mentioned for
       completeness... */

    tm_hostnames = NULL;
    num_tm_hostnames = 0;
    for (i = 0; i < num_node_ids; ++i) {
        h = get_tm_hostname(tm_node_ids[i]);
        if (NULL == h) {
            opal_output(0, "get_tm_hostname returned NULL");
            return ORTE_ERROR;
        }
        opal_argv_append(&num_tm_hostnames, &tm_hostnames, h);
        free(h);
    }

    /* All done */

    return ORTE_SUCCESS;
}

static int do_tm_resolve(char *hostname, tm_node_id *tnodeid)
{
    int i, ret;

    /* Have we already queried TM for all the node info? */
    if (NULL == tm_hostnames) {
        return ORTE_ERR_NOT_FOUND;
    }

    /* Find the TM ID of the hostname that we're looking for */
    for (i = 0; i < num_tm_hostnames; ++i) {
        if (0 == strcmp(hostname, tm_hostnames[i])) {
            *tnodeid = tm_node_ids[i];
            if (mca_pls_tm_component.debug) {
                opal_output(0, "pls:tm:launch: resolved host %s to node ID %d",
                            hostname, tm_node_ids[i]);
            }
            break;
        }
    }

    /* All done */
    if (i < num_tm_hostnames) {
        ret = ORTE_SUCCESS;
    } else { 
        ret = ORTE_ERR_NOT_FOUND;
    }

    return ret;
}


static int pls_tm_start_proc(char *nodename, int argc, char **argv, char **env,
                             tm_task_id *task_id, tm_event_t *event)
{
    int ret;
    tm_node_id node_id;

    /* get the tm node id for this node */
    ret = do_tm_resolve(nodename, &node_id);
    if (ORTE_SUCCESS != ret) {
        return ret;
    }

    ret = tm_spawn(argc, argv, env, node_id, task_id, event);
    if (TM_SUCCESS != ret) {
        return ORTE_ERROR;
    }

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
