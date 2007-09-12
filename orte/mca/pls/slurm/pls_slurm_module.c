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

#include "opal/mca/installdirs/installdirs.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/opal_environ.h"
#include "opal/util/path.h"
#include "opal/util/show_help.h"
#include "opal/util/basename.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/runtime/params.h"
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
#include "pls_slurm.h"


/*
 * Local functions
 */
static int pls_slurm_launch_job(orte_jobid_t jobid);
static int pls_slurm_terminate_job(orte_jobid_t jobid, struct timeval *timeout, opal_list_t *attrs);
static int pls_slurm_terminate_orteds(struct timeval *timeout, opal_list_t *attrs);
static int pls_slurm_terminate_proc(const orte_process_name_t *name);
static int pls_slurm_signal_job(orte_jobid_t jobid, int32_t signal, opal_list_t *attrs);
static int pls_slurm_signal_proc(const orte_process_name_t *name, int32_t signal);
static int pls_slurm_finalize(void);

static int pls_slurm_start_proc(int argc, char **argv, char **env,
                                char *prefix);


/*
 * Global variable
 */
orte_pls_base_module_1_3_0_t orte_pls_slurm_module = {
    pls_slurm_launch_job,
    pls_slurm_terminate_job,
    pls_slurm_terminate_orteds,
    pls_slurm_terminate_proc,
    pls_slurm_signal_job,
    pls_slurm_signal_proc,
    pls_slurm_finalize
};

/*
 * Local variables
 */
static pid_t srun_pid = 0;
static orte_jobid_t active_job = ORTE_JOBID_INVALID;
static bool failed_launch;


/* When working in this function, ALWAYS jump to "cleanup" if
 * you encounter an error so that orterun will be woken up and
 * the job can cleanly terminate
 */
static int pls_slurm_launch_job(orte_jobid_t jobid)
{
    orte_job_map_t *map = NULL;
    opal_list_item_t *item;
    size_t num_nodes;
    char *jobid_string = NULL;
    char *param;
    char **argv = NULL;
    int argc;
    int rc;
    char *tmp;
    char** env = NULL;
    char* var;
    char *nodelist_flat;
    char **nodelist_argv;
    int nodelist_argc;
    orte_process_name_t name;
    char *name_string;
    char **custom_strings;
    int num_args, i;
    char *cur_prefix;
    struct timeval joblaunchstart, launchstart, launchstop;
    int proc_name_index = 0;

    if (mca_pls_slurm_component.timing) {
        if (0 != gettimeofday(&joblaunchstart, NULL)) {
            opal_output(0, "pls_slurm: could not obtain job start time");
        }        
    }
    
    /* save the active jobid */
    active_job = jobid;
    
    /* indicate the state of the launch */
    failed_launch = true;
    
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
    
    num_nodes = map->num_new_daemons;
    if (num_nodes == 0) {
        /* no new daemons required - just launch apps */
        goto launch_apps;
    }

    /* need integer value for command line parameter */
    asprintf(&jobid_string, "%lu", (unsigned long) jobid);

    /*
     * start building argv array
     */
    argv = NULL;
    argc = 0;

    /*
     * SLURM srun OPTIONS
     */

    /* add the srun command */
    opal_argv_append(&argc, &argv, "srun");

    /* Append user defined arguments to srun */
    if ( NULL != mca_pls_slurm_component.custom_args ) {
        custom_strings = opal_argv_split(mca_pls_slurm_component.custom_args, ' ');
        num_args       = opal_argv_count(custom_strings);
        for (i = 0; i < num_args; ++i) {
            opal_argv_append(&argc, &argv, custom_strings[i]);
        }
        opal_argv_free(custom_strings);
    }

    asprintf(&tmp, "--nodes=%lu", (unsigned long) num_nodes);
    opal_argv_append(&argc, &argv, tmp);
    free(tmp);

    asprintf(&tmp, "--ntasks=%lu", (unsigned long) num_nodes);
    opal_argv_append(&argc, &argv, tmp);
    free(tmp);

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
    if (0 == opal_argv_count(nodelist_argv)) {
        opal_show_help("help-pls-slurm.txt", "no-hosts-in-list", true);
        rc = ORTE_ERR_FAILED_TO_START;
        goto cleanup;
    }
    nodelist_flat = opal_argv_join(nodelist_argv, ',');
    opal_argv_free(nodelist_argv);
    asprintf(&tmp, "--nodelist=%s", nodelist_flat);
    opal_argv_append(&argc, &argv, tmp);
    free(tmp);


    /*
     * ORTED OPTIONS
     */

    /* add the daemon command (as specified by user) */
    opal_argv_append(&argc, &argv, mca_pls_slurm_component.orted);
    
    /* ensure we don't lose contact */
    orte_no_daemonize_flag = true;

    /* Add basic orted command line options, including debug flags */
    orte_pls_base_orted_append_basic_args(&argc, &argv,
                                          &proc_name_index,
                                          NULL,
                                          num_nodes);

    /* force orted to use the slurm sds */
    opal_argv_append(&argc, &argv, "--ns-nds");
    opal_argv_append(&argc, &argv, "slurm");

    /* tell the new daemons the base of the name list so they can compute
     * their own name on the other end
     */
    name.jobid = 0;
    name.vpid = map->daemon_vpid_start;
    rc = orte_ns.get_proc_name_string(&name_string, &name);
    if (ORTE_SUCCESS != rc) {
        opal_output(0, "pls_slurm: unable to create process name");
        goto cleanup;
    }

    free(argv[proc_name_index]);
    argv[proc_name_index] = strdup(name_string);
    free(name_string);

    if (mca_pls_slurm_component.debug) {
        param = opal_argv_join(argv, ' ');
        if (NULL != param) {
            opal_output(0, "pls:slurm: final top-level argv:");
            opal_output(0, "pls:slurm:     %s", param);
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
                opal_show_help("help-pls-slurm.txt", "multiple-prefixes",
                               true, cur_prefix, app_prefix_dir);
                return ORTE_ERR_FATAL;
            }

            /* If not yet set, copy it; iff set, then it's the
               same anyway */
            if (NULL == cur_prefix) {
                cur_prefix = strdup(app_prefix_dir);
                if (mca_pls_slurm_component.debug) {
                    opal_output (0, "pls:slurm: Set prefix:%s",
                                 cur_prefix);
                }
            }
        }
    }

    /* setup environment */
    env = opal_argv_copy(environ);
    
    /* purge it of any params not for orteds */
    orte_pls_base_purge_mca_params(&env);
    
    /* add the nodelist */
    var = mca_base_param_environ_variable("orte", "slurm", "nodelist");
    opal_setenv(var, nodelist_flat, true, &env);
    free(nodelist_flat);
    free(var);

    if (mca_pls_slurm_component.timing) {
        if (0 != gettimeofday(&launchstart, NULL)) {
            opal_output(0, "pls_slurm: could not obtain start time");
        }        
    }
    
    /* exec the daemon(s) */
    if (ORTE_SUCCESS != (rc = pls_slurm_start_proc(argc, argv, env, cur_prefix))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* do NOT wait for srun to complete. Srun only completes when the processes
     * it starts - in this case, the orteds - complete. Instead, we'll catch
     * any srun failures and deal with them elsewhere
     */
    
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

    /* declare the launch a success */
    failed_launch = false;
    
    if (mca_pls_slurm_component.timing) {
        if (0 != gettimeofday(&launchstop, NULL)) {
             opal_output(0, "pls_slurm: could not obtain stop time");
         } else {
             opal_output(0, "pls_slurm: daemon block launch time is %ld usec",
                         (launchstop.tv_sec - launchstart.tv_sec)*1000000 + 
                         (launchstop.tv_usec - launchstart.tv_usec));
             opal_output(0, "pls_slurm: total job launch time is %ld usec",
                         (launchstop.tv_sec - joblaunchstart.tv_sec)*1000000 + 
                         (launchstop.tv_usec - joblaunchstart.tv_usec));
         }
    }

    if (ORTE_SUCCESS != rc) {
        opal_output(0, "pls:slurm: start_procs returned error %d", rc);
        goto cleanup;
    }

    /* JMS: short we stash the srun pid in the gpr somewhere for cleanup? */

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
    
    if(NULL != jobid_string) {
        free(jobid_string);
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
    
    return rc;
}


static int pls_slurm_terminate_job(orte_jobid_t jobid, struct timeval *timeout, opal_list_t *attrs)
{
    int rc;
    
    /* order them to kill their local procs for this job */
    if (ORTE_SUCCESS != (rc = orte_pls_base_orted_kill_local_procs(jobid, timeout, attrs))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}


/**
* Terminate the orteds for a given job
 */
static int pls_slurm_terminate_orteds(struct timeval *timeout, opal_list_t *attrs)
{
    int rc;
    
    /* deregister the waitpid callback to ensure we don't make it look like
    * srun failed when it didn't. Since the srun may have already completed,
    * do NOT ERROR_LOG any return code to avoid confusing, duplicate error
    * messages
    */
    orte_wait_cb_cancel(srun_pid);
    
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
static int pls_slurm_terminate_proc(const orte_process_name_t *name)
{
    opal_output(0, "pls:slurm:terminate_proc: not supported");
    return ORTE_ERR_NOT_SUPPORTED;
}


/**
 * Signal all the processes in the child srun by sending the signal directly to it
 */
static int pls_slurm_signal_job(orte_jobid_t jobid, int32_t signal, opal_list_t *attrs)
{
    if (0 != srun_pid) {
        kill(srun_pid, (int)signal);
   }
    return ORTE_SUCCESS;
}


/*
 * Signal a specific process
 */
static int pls_slurm_signal_proc(const orte_process_name_t *name, int32_t signal)
{
    opal_output(0, "pls:slurm:signal_proc: not supported");
    return ORTE_ERR_NOT_SUPPORTED;
}


static int pls_slurm_finalize(void)
{
    int rc;
    
    /* cleanup any pending recvs */
    if (ORTE_SUCCESS != (rc = orte_pls_base_comm_stop())) {
        ORTE_ERROR_LOG(rc);
    }
    
    return ORTE_SUCCESS;
}


static void srun_wait_cb(pid_t pid, int status, void* cbdata){
    /* According to the SLURM folks, srun always returns the highest exit
       code of our remote processes. Thus, a non-zero exit status doesn't
       necessarily mean that srun failed - it could be that an orted returned
       a non-zero exit status. Of course, that means the orted failed(!), so
       the end result is the same - the job didn't start.
    
       As a result, we really can't do much with the exit status itself - it
       could be something in errno (if srun itself failed), or it could be
       something returned by an orted, or it could be something returned by
       the OS (e.g., couldn't find the orted binary). Somebody is welcome
       to sort out all the options and pretty-print a better error message. For
       now, though, the only thing that really matters is that
       srun failed. Report the error and make sure that orterun
       wakes up - otherwise, do nothing!
    */
    
    int rc;
    
    if (0 != status) {
        if (failed_launch) {
            /* we have a problem during launch */
            opal_output(0, "ERROR: srun failed to start the required daemons.");
            opal_output(0, "ERROR: This could be due to an inability to find the orted binary");
            opal_output(0, "ERROR: on one or more remote nodes, lack of authority to execute");
            opal_output(0, "ERROR: on one or more specified nodes, or other factors.");
            
            /* set the job state so we know it failed to start */
            if (ORTE_SUCCESS != (rc = orte_smr.set_job_state(active_job, ORTE_JOB_STATE_FAILED_TO_START))) {
                ORTE_ERROR_LOG(rc);
            }
            
        } else {
            /* an orted must have died unexpectedly after launch */
            opal_output(0, "ERROR: srun has detected that a required daemon terminated");
            opal_output(0, "ERROR: during execution of the application with a non-zero");
            opal_output(0, "ERROR: status of %ld. This is a fatal error.", (long)status);
            
            /* set the job state so we know it aborted */
            if (ORTE_SUCCESS != (rc = orte_smr.set_job_state(active_job, ORTE_JOB_STATE_ABORTED))) {
                ORTE_ERROR_LOG(rc);
            }
        }
        /* force termination of the job */
        if (ORTE_SUCCESS != (rc = orte_wakeup(active_job))) {
            ORTE_ERROR_LOG(rc);
        }        
    }
    
}


static int pls_slurm_start_proc(int argc, char **argv, char **env,
                                char *prefix)
{
    int fd;
    char *exec_argv = opal_path_findv(argv[0], 0, env, NULL);

    if (NULL == exec_argv) {
        return ORTE_ERR_NOT_FOUND;
    }

    srun_pid = fork();
    if (-1 == srun_pid) {
        ORTE_ERROR_LOG(ORTE_ERR_SYS_LIMITS_CHILDREN);
        return ORTE_ERR_SYS_LIMITS_CHILDREN;
    }
    
    if (0 == srun_pid) {  /* child */
        char *bin_base = NULL, *lib_base = NULL;

        /* Figure out the basenames for the libdir and bindir.  There
           is a lengthy comment about this in pls_rsh_module.c
           explaining all the rationale for how / why we're doing
           this. */

        lib_base = opal_basename(opal_install_dirs.libdir);
        bin_base = opal_basename(opal_install_dirs.bindir);

        /* If we have a prefix, then modify the PATH and
           LD_LIBRARY_PATH environment variables.  */
        if (NULL != prefix) {
            char *oldenv, *newenv;

            /* Reset PATH */
            oldenv = getenv("PATH");
            if (NULL != oldenv) {
                asprintf(&newenv, "%s/%s:%s", prefix, bin_base, oldenv);
            } else {
                asprintf(&newenv, "%s/%s", prefix, bin_base);
            }
            opal_setenv("PATH", newenv, true, &env);
            if (mca_pls_slurm_component.debug) {
                opal_output(0, "pls:slurm: reset PATH: %s", newenv);
            }
            free(newenv);

            /* Reset LD_LIBRARY_PATH */
            oldenv = getenv("LD_LIBRARY_PATH");
            if (NULL != oldenv) {
                asprintf(&newenv, "%s/%s:%s", prefix, lib_base, oldenv);
            } else {
                asprintf(&newenv, "%s/%s", prefix, lib_base);
            }
            opal_setenv("LD_LIBRARY_PATH", newenv, true, &env);
            if (mca_pls_slurm_component.debug) {
                opal_output(0, "pls:slurm: reset LD_LIBRARY_PATH: %s",
                            newenv);
            }
            free(newenv);
        }

        fd = open("/dev/null", O_CREAT|O_WRONLY|O_TRUNC, 0666);
        if(fd > 0) {
            dup2(fd, 0);
        }

        /* When not in debug mode and --debug-daemons was not passed,
         * tie stdout/stderr to dev null so we don't see messages from orted */
        if (0 == mca_pls_slurm_component.debug && !orte_debug_daemons_flag) {
            if (fd >= 0) {
                if (fd != 1) {
                    dup2(fd,1);
                }
                if (fd != 2) {
                    dup2(fd,2);
                }
            }
        }

        if (fd > 2) {
            close(fd);
        }

        /* get the srun process out of orterun's process group so that
           signals sent from the shell (like those resulting from
           cntl-c) don't get sent to srun */
        setpgid(0, 0);

        execve(exec_argv, argv, env);

        opal_output(0, "pls:slurm:start_proc: exec failed");
        /* don't return - need to exit - returning would be bad -
           we're not in the calling process anymore */
        exit(1);
    } else {  /* parent */
        /* just in case, make sure that the srun process is not in our
        process group any more.  Stevens says always do this on both
        sides of the fork... */
        setpgid(srun_pid, srun_pid);
        
        /* setup the waitpid so we can find out if srun succeeds! */
        orte_wait_cb(srun_pid, srun_wait_cb, NULL);
        free(exec_argv);
    }

    return ORTE_SUCCESS;
}
