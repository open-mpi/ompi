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
#include "orte/constants.h"
#include "orte/types.h"

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
#include "opal/runtime/opal_progress.h"

#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_wakeup.h"
#include "orte/runtime/orte_wait.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rmaps/rmaps.h"

#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/plm_private.h"
#include "plm_md.h"


/*
 * Local functions
 */
static int plm_md_init(void);
static int plm_md_launch_job(orte_job_t *jdata);
static int plm_md_terminate_job(orte_jobid_t jobid);
static int plm_md_terminate_orteds(void);
static int plm_md_signal_job(orte_jobid_t jobid, int32_t signal);
static int plm_md_finalize(void);

static int plm_md_start_proc(int argc, char **argv, char **env,
                                char *prefix);


/*
 * Global variable
 */
orte_plm_base_module_1_0_0_t orte_plm_md_module = {
    plm_md_init,
    orte_plm_base_set_hnp_name,
    plm_md_launch_job,
    plm_md_terminate_job,
    plm_md_terminate_orteds,
    plm_md_signal_job,
    plm_md_finalize
};

/*
 * Local variables
 */
static pid_t srun_pid = 0;
static orte_jobid_t active_job = ORTE_JOBID_INVALID;
static bool failed_launch;


/**
* Init the module
 */
static int plm_md_init(void)
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
static int plm_md_launch_job(orte_job_t *jdata)
{
    orte_app_context_t **apps;
    orte_node_t **nodes;
    orte_std_cntr_t n;
    orte_job_map_t *map;
    char *jobid_string = NULL;
    char *param;
    char **argv = NULL;
    int argc;
    int rc=ORTE_SUCCESS;
    char *tmp;
    char** env = NULL;
    char* var;
    char *nodelist_flat;
    char **nodelist_argv;
    char *rml_uri;
    int nodelist_argc;
    int i;
    char *cur_prefix;
    struct timeval launchstart, launchstop;
    
    if (orte_timing) {
        if (0 != gettimeofday(&launchstart, NULL)) {
            opal_output(0, "plm_md: could not obtain job start time");
            launchstart.tv_sec = 0;
            launchstart.tv_usec = 0;
        }        
    }
    
    /* indicate the state of the launch */
    failed_launch = true;
    
    /* if this jobid isn't invalid, then it already
     * has been setup, so skip the setup actions
     */
    if (ORTE_JOBID_INVALID != jdata->jobid) {
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:slurmd: launching job %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(jdata->jobid)));
        goto GETMAP;
    }
    
    /* create a jobid for this job */
    if (ORTE_SUCCESS != (rc = orte_plm_base_create_jobid(&jdata->jobid))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
        
    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:slurmd: launching job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jdata->jobid)));
    
    /* setup the job */
    if (ORTE_SUCCESS != (rc = orte_plm_base_setup_job(jdata))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

GETMAP:
     /* set the active jobid */
     active_job = jdata->jobid;
    
    /* Get the map for this job */
    if (NULL == (map = orte_rmaps.get_job_map(active_job))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        rc = ORTE_ERR_NOT_FOUND;
        goto cleanup;
    }
    apps = (orte_app_context_t**)jdata->apps->addr;
    nodes = (orte_node_t**)map->nodes->addr;
    
    /* we are going to ignore the daemons and just launch
     * the apps directly. This means we cannot use the
     * base function to launch_apps as that function assumes
     * we are doing it via our daemons
     */
    
    /* are there any procs to actually launch? */
    if (0 == jdata->num_procs) {
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:slurmd: no procs to launch!",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        goto cleanup;
    }
    
    /* need the jobid as a string for command line parameter */
    asprintf(&jobid_string, "%lu", (unsigned long) jdata->jobid);

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

    asprintf(&tmp, "--nodes=%lu", (unsigned long) map->num_new_daemons);
    opal_argv_append(&argc, &argv, tmp);
    free(tmp);

    asprintf(&tmp, "--ntasks=%lu", (unsigned long) jdata->num_procs);
    opal_argv_append(&argc, &argv, tmp);
    free(tmp);

    /* create nodelist */
    nodelist_argv = NULL;
    nodelist_argc = 0;

    for (n=0; n < map->num_nodes; n++ ) {
        /* add it to the list of nodes upon which
         * we need to launch a proc
         */
        opal_argv_append(&nodelist_argc, &nodelist_argv, nodes[n]->name);
    }
    if (0 == opal_argv_count(nodelist_argv)) {
        opal_show_help("help-plm-slurmd.txt", "no-hosts-in-list", true);
        rc = ORTE_ERR_FAILED_TO_START;
        goto cleanup;
    }
    nodelist_flat = opal_argv_join(nodelist_argv, ',');
    opal_argv_free(nodelist_argv);
    asprintf(&tmp, "--nodelist=%s", nodelist_flat);
    opal_argv_append(&argc, &argv, tmp);
    free(tmp);

    OPAL_OUTPUT_VERBOSE((2, orte_plm_globals.output,
                         "%s plm:slurmd: launching on nodes %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), nodelist_flat));
    
    /*
     * APP OPTIONS
     */

    /* add the executable (as specified by user) */
    opal_argv_append(&argc, &argv, apps[0]->app);
    
    /* add the user's cmd args */
    for (i=1; NULL != apps[0]->argv[i]; i++) {
        opal_argv_append(&argc, &argv, apps[0]->argv[i]);
    }
    
    /* setup the environment - since there will be no daemon, we have
     * tell the the apps everything they need to know via the environment,
     * not command line args. Hence, we cannot use the plm base function
     * to setup the info
     */
    if (NULL != apps[0]->env) {
        env = opal_environ_merge(environ, apps[0]->env);
    } else {
        env = opal_argv_copy(environ);
    }
    
    /* add the nodelist */
    var = mca_base_param_environ_variable("orte", "slurm", "nodelist");
    opal_setenv(var, nodelist_flat, true, &env);
    free(nodelist_flat);
    free(var);
    
    /* tell the proc what SDS component to use */
    var = mca_base_param_environ_variable("sds", NULL, NULL);
    opal_setenv(var, "slurm", true, &env);
    free(var);
    
    /* pass the jobid */
    var = mca_base_param_environ_variable("orte", "sds", "jobid");
    orte_util_convert_jobid_to_string(&param, jdata->jobid);
    opal_setenv(var, param, true, &env);
    free(var);
    free(param);
    
    /* pass the starting vpid */
    var = mca_base_param_environ_variable("orte", "sds", "vpid");
    opal_setenv(var, "0", true, &env);
    free(var);
    
    /* pass the total number of procs that will be in the system */
    var = mca_base_param_environ_variable("orte", "sds", "num_procs");
    asprintf(&param, "%lu", (unsigned long)(jdata->num_procs));
    opal_setenv(var, param, true, &env);
    free(var);
    free(param);
    
    /* pass the uri of the hnp */
    var = mca_base_param_environ_variable("orte", "hnp", "uri");
    rml_uri = orte_rml.get_contact_info();
    opal_setenv(var, rml_uri, true, &env);
    free(var);
    /* make the proc think the local daemon is the hnp */
    var = mca_base_param_environ_variable("orte", "local_daemon", "uri");
    opal_setenv(var, rml_uri, true, &env);
    free(var);
    free(rml_uri);
    
    /* order the proc to use the unity routed component */
    var = mca_base_param_environ_variable("routed", NULL, NULL);
    opal_setenv(var, "unity", true, &env);
    free(var);
    
    if (0 < opal_output_get_verbosity(orte_plm_globals.output)) {
        param = opal_argv_join(argv, ' ');
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:slurmd: final top-level argv:\n\t%s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (NULL == param) ? "NULL" : param));
        if (NULL != param) free(param);
    }

    /* Copy the prefix-directory specified in the
       corresponding app_context.  If there are multiple,
       different prefix's in the app context, complain (i.e., only
       allow one --prefix option for the entire slurmd run -- we
       don't support different --prefix'es for different nodes in
       the SLURM plm) */
    cur_prefix = NULL;
    for (n=0; n < jdata->num_apps; n++) {
        char * app_prefix_dir = apps[n]->prefix_dir;
         /* Check for already set cur_prefix_dir -- if different,
           complain */
        if (NULL != app_prefix_dir) {
            if (NULL != cur_prefix &&
                0 != strcmp (cur_prefix, app_prefix_dir)) {
                opal_show_help("help-plm-slurmd.txt", "multiple-prefixes",
                               true, cur_prefix, app_prefix_dir);
                return ORTE_ERR_FATAL;
            }

            /* If not yet set, copy it; iff set, then it's the
               same anyway */
            if (NULL == cur_prefix) {
                cur_prefix = strdup(app_prefix_dir);
                OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                     "%s plm:slurmd: Set prefix:%s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     cur_prefix));
            }
        }
    }

    /* exec the proc(s) */
    if (ORTE_SUCCESS != (rc = plm_md_start_proc(argc, argv, env, cur_prefix))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* do NOT wait for srun to complete. Srun only completes when the processes
     * it starts complete. Instead, we'll catch
     * any srun failures and deal with them elsewhere
     */
    
    /* declare the launch a success */
    failed_launch = false;
    
    if (orte_timing) {
        if (0 != gettimeofday(&launchstop, NULL)) {
             opal_output(0, "plm_md: could not obtain stop time");
         } else {
             opal_output(0, "plm_md: total job launch time is %ld usec",
                         (launchstop.tv_sec - launchstart.tv_sec)*1000000 + 
                         (launchstop.tv_usec - launchstart.tv_usec));
         }
    }
cleanup:
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
        jdata->state = ORTE_JOB_STATE_FAILED_TO_START;
		orte_wakeup(1);
    }
    
    return rc;
}


static int plm_md_terminate_job(orte_jobid_t jobid)
{
    if (0 != srun_pid) {
        kill(srun_pid, -9);
    }
    return ORTE_SUCCESS;
}


/**
* Terminate the orteds for a given job
 */
static int plm_md_terminate_orteds(void)
{
    orte_job_t *daemons;
    int data=1;
    
    /* get the daemon job object */
    if (NULL != (daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid))) {
        /* record all daemons as terminated */
        daemons->num_terminated = daemons->num_procs;
        daemons->state = ORTE_JOB_STATE_TERMINATED;
    }
    
    /* trip the orted exit trigger */
    write(orteds_exit, &data, sizeof(int));
    opal_progress();
    
    return ORTE_SUCCESS;
}


/**
 * Signal all the processes in the child srun by sending the signal directly to it
 */
static int plm_md_signal_job(orte_jobid_t jobid, int32_t signal)
{
    if (0 != srun_pid) {
        kill(srun_pid, (int)signal);
    }
    return ORTE_SUCCESS;
}


static int plm_md_finalize(void)
{
    int rc;
    
    /* cleanup any pending recvs */
    if (ORTE_SUCCESS != (rc = orte_plm_base_comm_stop())) {
        ORTE_ERROR_LOG(rc);
    }
    
    return ORTE_SUCCESS;
}


static void srun_wait_cb(pid_t pid, int status, void* cbdata){
    /* According to the SLURM folks, srun always returns the highest exit
       code of our remote processes. Thus, a non-zero exit status doesn't
       necessarily mean that srun failed - it could be that a proc returned
       a non-zero exit status. Of course, that means the job failed(!), so
       the end result is the same.
    
       As a result, we really can't do much with the exit status itself - it
       could be something in errno (if srun itself failed), or it could be
       something returned by a proc, or it could be something returned by
       the OS (e.g., couldn't find the executable). Somebody is welcome
       to sort out all the options and pretty-print a better error message. For
       now, though, the only thing that really matters is that
       srun terminated. Report the error (if there is one) and make sure that orterun
       wakes up!
    */
    orte_job_t *jdata;
    
    if (NULL == (jdata = orte_get_job_data_object(active_job))) {
        /* bad jobid */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        orte_wakeup(status);
        return;
    }

    if (0 != status) {
        if (failed_launch) {
            /* we have a problem during launch */
            opal_output(0, "ERROR: srun failed to start the specified procs.");
            opal_output(0, "ERROR: This could be due to an inability to find the executable");
            opal_output(0, "ERROR: on one or more remote nodes, lack of authority to execute");
            opal_output(0, "ERROR: on one or more specified nodes, or other factors.");
            
            /* report that the launch has failed so we break out of the daemon
             * callback receive and exit
             */
            jdata->state = ORTE_JOB_STATE_FAILED_TO_START;
            
        } else {
            /* a proc must have died unexpectedly after launch - report
             * that the job has failed so we exit
             */
            jdata->state = ORTE_JOB_STATE_ABORTED;
       }
    } else {
        jdata->state = ORTE_JOB_STATE_TERMINATED;
    }
	
    orte_wakeup(status);
}


static int plm_md_start_proc(int argc, char **argv, char **env,
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
           is a lengthy comment about this in plm_rsh_module.c
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
            OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                 "%s plm:slurmd: reset PATH: %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 newenv));
            free(newenv);

            /* Reset LD_LIBRARY_PATH */
            oldenv = getenv("LD_LIBRARY_PATH");
            if (NULL != oldenv) {
                asprintf(&newenv, "%s/%s:%s", prefix, lib_base, oldenv);
            } else {
                asprintf(&newenv, "%s/%s", prefix, lib_base);
            }
            opal_setenv("LD_LIBRARY_PATH", newenv, true, &env);
            OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                 "%s plm:slurmd: reset LD_LIBRARY_PATH: %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 newenv));
            free(newenv);
        }

        fd = open("/dev/null", O_CREAT|O_WRONLY|O_TRUNC, 0666);
        if(fd > 0) {
            dup2(fd, 0);
        }

        /* When not in debug mode and --debug-daemons was not passed,
         * tie stdout/stderr to dev null so we don't see messages from orted */
        if (0 >= opal_output_get_verbosity(orte_plm_globals.output) &&
            !orte_debug_daemons_flag) {
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

        opal_output(0, "plm:slurmd:start_proc: exec failed");
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
