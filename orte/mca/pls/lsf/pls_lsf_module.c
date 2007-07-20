/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Cisco Systems, Inc.  All rights reserved.
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
#include "orte/orte_types.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <signal.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#define SR1_PJOBS
#include <lsf/lsbatch.h>

#include "opal/mca/installdirs/installdirs.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/opal_environ.h"
#include "opal/util/path.h"
#include "opal/util/show_help.h"
#include "opal/util/basename.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_wakeup.h"
#include "orte/runtime/orte_wait.h"
#include "orte/mca/ns/base/base.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/smr/smr.h"
#include "orte/mca/rmaps/rmaps.h"

#include "orte/mca/pls/pls.h"
#include "orte/mca/pls/base/base.h"
#include "orte/mca/pls/base/pls_private.h"
#include "pls_lsf.h"


/*
 * Local functions
 */
static int pls_lsf_launch_job(orte_jobid_t jobid);
static int pls_lsf_terminate_job(orte_jobid_t jobid, struct timeval *timeout, opal_list_t *attrs);
static int pls_lsf_terminate_orteds(struct timeval *timeout, opal_list_t *attrs);
static int pls_lsf_terminate_proc(const orte_process_name_t *name);
static int pls_lsf_signal_job(orte_jobid_t jobid, int32_t signal, opal_list_t *attrs);
static int pls_lsf_signal_proc(const orte_process_name_t *name, int32_t signal);
static int pls_lsf_finalize(void);


/*
 * Global variable
 */
orte_pls_base_module_1_3_0_t orte_pls_lsf_module = {
    pls_lsf_launch_job,
    pls_lsf_terminate_job,
    pls_lsf_terminate_orteds,
    pls_lsf_terminate_proc,
    pls_lsf_signal_job,
    pls_lsf_signal_proc,
    pls_lsf_finalize
};

/*
 * Local variables
 */
static orte_jobid_t active_job = ORTE_JOBID_INVALID;


/* When working in this function, ALWAYS jump to "cleanup" if
 * you encounter an error so that orterun will be woken up and
 * the job can cleanly terminate
 */
static int pls_lsf_launch_job(orte_jobid_t jobid)
{
    orte_job_map_t *map = NULL;
    opal_list_item_t *item;
    size_t num_nodes;
    char *param;
    char **argv = NULL;
    int argc;
    int rc;
    char** env = NULL;
    char* var;
    char **nodelist_argv;
    int nodelist_argc;
    orte_process_name_t name;
    char *name_string;
    int i;
    char *cur_prefix;
    struct timeval joblaunchstart, launchstart, launchstop;
    int proc_name_index = 0;
    bool failed_launch = true;

    printf("pls lsf being used to launch!\n");
    if (mca_pls_lsf_component.timing) {
        if (0 != gettimeofday(&joblaunchstart, NULL)) {
            opal_output(0, "pls_lsf: could not obtain job start time");
        }        
    }
    
    /* save the active jobid */
    active_job = jobid;
    
    /* Query the map for this job.
     * We need the entire mapping for a couple of reasons:
     *  - need the prefix to start with.
     *  - need to know if we are launching on a subset of the allocated nodes
     * All other mapping responsibilities fall to orted in the fork PLS
     */
    rc = orte_rmaps.get_job_map(&map, jobid);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* account for any reuse of daemons */
    if (ORTE_SUCCESS != (rc = orte_pls_base_launch_on_existing_daemons(map))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    num_nodes = map->num_new_daemons;
    if (num_nodes == 0) {
        /* nothing to do - just return */
        failed_launch = false;
        rc = ORTE_SUCCESS;
        goto cleanup;
    }

    /* create nodelist */
    nodelist_argv = NULL;
    nodelist_argc = 0;

    for (item =  opal_list_get_first(&map->nodes);
         item != opal_list_get_end(&map->nodes);
         item =  opal_list_get_next(item)) {
        orte_mapped_node_t* node = (orte_mapped_node_t*)item;

        /* if the daemon already exists on this node, then
         * don't include it
         */
        if (node->daemon_preexists) {
            continue;
        }
        
        /* otherwise, add it to the list of nodes upon which
         * we need to launch a daemon
         */
        opal_argv_append(&nodelist_argc, &nodelist_argv, node->nodename);
    }


    /*
     * start building argv array
     */
    argv = NULL;
    argc = 0;
    
    /*
     * ORTED OPTIONS
     */

    /* add the daemon command (as specified by user) */
    opal_argv_append(&argc, &argv, mca_pls_lsf_component.orted);
    opal_argv_append(&argc, &argv, "--no-daemonize");

    /* Add basic orted command line options */
    orte_pls_base_orted_append_basic_args(&argc, &argv,
                                          &proc_name_index,
                                          NULL,
                                          num_nodes
                                          );

    /* force orted to use the lsf sds */
    opal_argv_append(&argc, &argv, "--ns-nds");
    opal_argv_append(&argc, &argv, "lsf");

    /* tell the new daemons the base of the name list so they can compute
     * their own name on the other end
     */
    name.jobid = 0;
    name.vpid = map->daemon_vpid_start;
    rc = orte_ns.get_proc_name_string(&name_string, &name);
    if (ORTE_SUCCESS != rc) {
        opal_output(0, "pls_lsf: unable to create process name");
        goto cleanup;
    }

    free(argv[proc_name_index]);
    argv[proc_name_index] = strdup(name_string);
    free(name_string);

    if (mca_pls_lsf_component.debug) {
        param = opal_argv_join(argv, ' ');
        if (NULL != param) {
            opal_output(0, "pls:lsf: final top-level argv:");
            opal_output(0, "pls:lsf:     %s", param);
            free(param);
        }
    }

    /* Copy the prefix-directory specified in the
       corresponding app_context.  If there are multiple,
       different prefix's in the app context, complain (i.e., only
       allow one --prefix option for the entire slurm run -- we
       don't support different --prefix'es for different nodes in
       the SLURM pls) */
    cur_prefix = NULL;
    for (i=0; i < map->num_apps; i++) {
        char * app_prefix_dir = map->apps[i]->prefix_dir;
         /* Check for already set cur_prefix_dir -- if different,
           complain */
        if (NULL != app_prefix_dir) {
            if (NULL != cur_prefix &&
                0 != strcmp (cur_prefix, app_prefix_dir)) {
                opal_show_help("help-pls-lsf.txt", "multiple-prefixes",
                               true, cur_prefix, app_prefix_dir);
                return ORTE_ERR_FATAL;
            }

            /* If not yet set, copy it; iff set, then it's the
               same anyway */
            if (NULL == cur_prefix) {
                cur_prefix = strdup(app_prefix_dir);
                if (mca_pls_lsf_component.debug) {
                    opal_output (0, "pls:lsf: Set prefix:%s",
                                 cur_prefix);
                }
            }
        }
    }

    /* setup environment */
    env = opal_argv_copy(environ);
    var = mca_base_param_environ_variable("seed", NULL, NULL);
    opal_setenv(var, "0", true, &env);
    free(var);

    if (mca_pls_lsf_component.timing) {
        if (0 != gettimeofday(&launchstart, NULL)) {
            opal_output(0, "pls_lsf: could not obtain start time");
        }        
    }
    
    /* exec the daemon(s). Do NOT wait for lsb_launch to complete as
     * it only completes when the processes it starts - in this case,
     * the orteds - complete. We need to go ahead and return so
     * orterun can do the rest of its stuff. Instead, we'll catch any
     * failures and deal with them elsewhere
     */
    argv = NULL;
    argc = 0;
    opal_argv_append(&argc, &argv, "env");
    opal_output(0, "launching on: %s", opal_argv_join(nodelist_argv, ' '));
    opal_output(0, "launching: %s", opal_argv_join(argv, ' '));
    if (lsb_launch(nodelist_argv, argv, LSF_DJOB_NOWAIT, env) < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_FAILED_TO_START);
        opal_output(0, "got nonzero: %d", rc);
        rc = ORTE_ERR_FAILED_TO_START;
        goto cleanup;
    }
    opal_output(0, "launched ok");
    sleep(5);
    exit(0);

    if (lsb_launch(nodelist_argv, argv, LSF_DJOB_NOWAIT, env) < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_FAILED_TO_START);
        rc = ORTE_ERR_FAILED_TO_START;
        goto cleanup;
    }
    
    /* declare the launch a success */
    failed_launch = false;
    
    if (mca_pls_lsf_component.timing) {
        if (0 != gettimeofday(&launchstop, NULL)) {
             opal_output(0, "pls_lsf: could not obtain stop time");
         } else {
             opal_output(0, "pls_lsf: daemon block launch time is %ld usec",
                         (launchstop.tv_sec - launchstart.tv_sec)*1000000 + 
                         (launchstop.tv_usec - launchstart.tv_usec));
             opal_output(0, "pls_lsf: total job launch time is %ld usec",
                         (launchstop.tv_sec - joblaunchstart.tv_sec)*1000000 + 
                         (launchstop.tv_usec - joblaunchstart.tv_usec));
         }
    }

    if (ORTE_SUCCESS != rc) {
        opal_output(0, "pls:lsf: start_procs returned error %d", rc);
        goto cleanup;
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
    
    /* check for failed launch - if so, force terminate */
    if (failed_launch) {
        if (ORTE_SUCCESS != 
            (rc = orte_smr.set_job_state(jobid, 
                                         ORTE_JOB_STATE_FAILED_TO_START))) {
            ORTE_ERROR_LOG(rc);
        }
        
        if (ORTE_SUCCESS != (rc = orte_wakeup(jobid))) {
            ORTE_ERROR_LOG(rc);
        }        
    }
    
    return rc;
}


static int pls_lsf_terminate_job(orte_jobid_t jobid, struct timeval *timeout, opal_list_t *attrs)
{
    int rc;
    
    /* order them to kill their local procs for this job */
    if (ORTE_SUCCESS !=
        (rc = orte_pls_base_orted_kill_local_procs(jobid, timeout, attrs))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}


/**
* Terminate the orteds for a given job
 */
static int pls_lsf_terminate_orteds(struct timeval *timeout, opal_list_t *attrs)
{
    int rc;
    
    /* tell them to die! */
    if (ORTE_SUCCESS != (rc = orte_pls_base_orted_exit(timeout, attrs))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}


/*
 * The way we've used SLURM, we can't kill individual processes --
 * we'll kill the entire job
 */
static int pls_lsf_terminate_proc(const orte_process_name_t *name)
{
    opal_output(0, "pls:lsf:terminate_proc: not supported");
    return ORTE_ERR_NOT_SUPPORTED;
}


/**
 * Signal all the processes in the child srun by sending the signal directly to it
 */
static int pls_lsf_signal_job(orte_jobid_t jobid, int32_t signal, opal_list_t *attrs)
{
    return ORTE_SUCCESS;
}


/*
 * Signal a specific process
 */
static int pls_lsf_signal_proc(const orte_process_name_t *name, int32_t signal)
{
    opal_output(0, "pls:lsf:signal_proc: not supported");
    return ORTE_ERR_NOT_SUPPORTED;
}


static int pls_lsf_finalize(void)
{
    int rc;
    
    /* cleanup any pending recvs */
    if (ORTE_SUCCESS != (rc = orte_pls_base_comm_stop())) {
        ORTE_ERROR_LOG(rc);
    }
    
    return ORTE_SUCCESS;
}
