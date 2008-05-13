/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006-2007 Sun Microsystems, Inc.  All rights reserved.
 *                         Use is subject to license terms.
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
/**
 * @file:
 * Part of the gridengine launcher.
 * See plm_gridengine.h for an overview of how it works.
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>
#include <string.h>
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#include <fcntl.h>
#include <signal.h>
#ifdef HAVE_PWD_H
#include <pwd.h>
#endif

#include "opal/mca/installdirs/installdirs.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/if.h"
#include "opal/util/os_path.h"
#include "opal/util/path.h"
#include "opal/event/event.h"
#include "orte/util/show_help.h"
#include "opal/util/argv.h"
#include "opal/util/opal_environ.h"
#include "orte/util/output.h"
#include "opal/util/basename.h"
#include "opal/util/opal_environ.h"

#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/session_dir.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_wakeup.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rmaps/rmaps.h"

#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/base.h"
#include "orte/mca/plm/base/plm_private.h"
#include "orte/mca/plm/gridengine/plm_gridengine.h"

static int plm_gridengine_init(void);
static int plm_gridengine_launch_job(orte_job_t *jdata);
static int plm_gridengine_terminate_job(orte_jobid_t jobid);
static int plm_gridengine_terminate_orteds(void);
static int plm_gridengine_signal_job(orte_jobid_t jobid, int32_t signal);
static int plm_gridengine_finalize(void);

orte_plm_base_module_t orte_plm_gridengine_module = {
    plm_gridengine_init,
    orte_plm_base_set_hnp_name,
    plm_gridengine_launch_job,
    NULL,
    plm_gridengine_terminate_job,
    plm_gridengine_terminate_orteds,
    plm_gridengine_signal_job,
    plm_gridengine_finalize
};

static void set_handler_default(int sig);

/* global storage of active jobid being launched */
static orte_jobid_t active_job=ORTE_JOBID_INVALID;

/**
 * Init the module
 */
int plm_gridengine_init(void)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_plm_base_comm_start())) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}


/**
 * Fill the orted_path variable with the directory to the orted
 */
static int orte_plm_gridengine_fill_orted_path(char** orted_path)
{
    struct stat buf;
   
    asprintf(orted_path, "%s/orted", opal_install_dirs.bindir);
    if (0 != stat(*orted_path, &buf)) {
        char *path = getenv("PATH");
        if (NULL == path) {
            path = ("PATH is empty!");
        }
        orte_show_help("help-plm-gridengine.txt", "no-local-orted",
            true, path, opal_install_dirs.bindir);
        return ORTE_ERR_NOT_FOUND;
    }

    return ORTE_SUCCESS;
}

/**
 * Callback on daemon exit.
 */
static void orte_plm_gridengine_wait_daemon(pid_t pid, int status, void* cbdata)
{
    if (! WIFEXITED(status) || ! WEXITSTATUS(status) == 0) {
        /* Need to catch SIGUSR1/2 for "qrsh/qsub -notify" to work. 
         * With "-notify" set, SIGUSR1/2 becomes the precursor for any pending 
         * SIGSTOP/SIGKILL. So just return and ignore the daemon_failed
         * at the end as that would kill off the user processes */
        if (SIGUSR1 == status || SIGUSR2 == status) {
            orte_output(0, "The daemon received a signal %d", status);
            return;
        }
        /* Otherwise, tell the user something went wrong. */
        orte_output(0, "ERROR: A daemon failed to start as expected.");
        orte_output(0, "ERROR: There may be more information available from");
        orte_output(0, "ERROR: the 'qstat -t' command on the Grid Engine tasks.");
        orte_output(0, "ERROR: If the problem persists, please restart the");
        orte_output(0, "ERROR: Grid Engine PE job");
        if (WIFEXITED(status)) {
            orte_output(0, "ERROR: The daemon exited unexpectedly with status %d.",
                        WEXITSTATUS(status));
        } else if (WIFSIGNALED(status)) {
#ifdef WCOREDUMP
            if (WCOREDUMP(status)) {
                orte_output(0, "The daemon received a signal %d (with core).",
                            WTERMSIG(status));
            } else {
                orte_output(0, "The daemon received a signal %d.", WTERMSIG(status));
            }
#else
            orte_output(0, "The daemon received a signal %d.", WTERMSIG(status));
#endif /* WCOREDUMP */
        } else {
            orte_output(0, "No extra status information is available: %d.", status);
        }
        
        /* report that the daemon has failed so we break out of the daemon
         * callback receive and can exit
         */
        orte_plm_base_launch_failed(active_job, true, pid, status, ORTE_JOB_STATE_FAILED_TO_START);
    }
}

/**
 * Launch a daemon (bootproxy) on each node. The daemon will be responsible
 * for launching the application.
 */
/* When working in this function, ALWAYS jump to "cleanup" if
 * you encounter an error so that orterun will be woken up and
 * the job can cleanly terminate
 */
static int plm_gridengine_launch_job(orte_job_t *jdata)
{
    orte_job_map_t *map;
    int node_name_index1;
    int node_name_index2;
    int proc_vpid_index;
    int orted_index;
    char *prefix_dir;
    char *param;
    char **argv=NULL;
    char **env=NULL;
    int argc;
    int rc;
    sigset_t sigs;
    char *lib_base = NULL, *bin_base = NULL;
    char *sge_root, *sge_arch;
    bool failed_launch = true;
    orte_app_context_t **apps;
    orte_node_t **nodes;
    orte_std_cntr_t nnode;
    
    /* create a jobid for this job */
    if (ORTE_SUCCESS != (rc = orte_plm_base_create_jobid(&jdata->jobid))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    ORTE_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:gridengine: launching job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jdata->jobid)));
    
    /* setup the job */
    if (ORTE_SUCCESS != (rc = orte_plm_base_setup_job(jdata))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
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
    
    if (map->num_new_daemons == 0) {
        /* have all the daemons we need - launch app */
        ORTE_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:gridengine: no new daemons to launch",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        goto launch_apps;
    }
    
    /*
     * Build argv array
     */
    argv = opal_argv_split("qrsh", ' ');
    argc = opal_argv_count(argv);
    /* gridengine specific flags */
    opal_argv_append(&argc, &argv, "-inherit");/*run tasks within curr job*/
    opal_argv_append(&argc, &argv, "-noshell");/*execute w/o wrapping shell*/
    opal_argv_append(&argc, &argv, "-nostdin");/*suppress input stream stdin*/
    opal_argv_append(&argc, &argv, "-V");      /*task to have the env as job*/
    if (mca_plm_gridengine_component.verbose) {
        opal_argv_append(&argc, &argv, "-verbose");
    }

    node_name_index1 = argc;
    opal_argv_append(&argc, &argv, "<template>");
                     
    /* add the orted daemon in command */
    orted_index = argc;
    opal_argv_append(&argc, &argv, mca_plm_gridengine_component.orted);

    /* By default, orteds will not daemonize and will be forced to
     * to stay in the same ptree as sge_shephard. The problem with
     * --daemonize is that the qrsh -inherit connections will stay
     * persistent for the whole duration of the task to the remote nodes,
     * which may not be ideal for large number of nodes
     */
    
    /* Add basic orted command line options, including
     * all debug options
     */
     orte_plm_base_orted_append_basic_args(&argc, &argv,
                                           "env",
                                           &proc_vpid_index,
                                           &node_name_index2);
                     
     /* setup environment. The environment is common to all the daemons
      * so we only need to do this once
      */
     env = opal_argv_copy(environ);
     
     if (0 < orte_output_get_verbosity(orte_plm_globals.output)) {
        param = opal_argv_join(argv, ' ');
        if (NULL != param) {
            orte_output(0, "plm:gridengine: final template argv:");
            orte_output(0, "plm:gridengine:     %s", param);
            free(param);
        }
    }

    /* Figure out the basenames for the libdir and bindir.  There is a
       lengthy comment about this in plm_rsh_module.c explaining all
       the rationale for how / why we're doing this.
     */

    lib_base = opal_basename(opal_install_dirs.libdir);
    bin_base = opal_basename(opal_install_dirs.bindir);

    /* See the note about prefix_dir in the orte/mca/plm/slurm/plm_slurm.c
     * module. Fo here, just note that we must have at least one app_context,
     * and we take the prefix_dir from that first one.
     */
    prefix_dir = apps[0]->prefix_dir;

     /* If we have a prefix, then modify the PATH and
        LD_LIBRARY_PATH environment variables.
      */
     if (NULL != prefix_dir) {
         char *oldenv, *newenv;
         
         /* Reset PATH */
         newenv = opal_os_path( false, prefix_dir, bin_base, NULL );
         oldenv = getenv("PATH");
         if (NULL != oldenv) {
             char *temp;
             asprintf(&temp, "%s:%s", newenv, oldenv);
             free( newenv );
             newenv = temp;
         }
         opal_setenv("PATH", newenv, true, &env);
         ORTE_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                              "%s plm:gridengine: reset PATH: %s",
                              ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                              newenv));
         free(newenv);
         
         /* Reset LD_LIBRARY_PATH */
         newenv = opal_os_path( false, prefix_dir, lib_base, NULL );
         oldenv = getenv("LD_LIBRARY_PATH");
         if (NULL != oldenv) {
             char* temp;
             asprintf(&temp, "%s:%s", newenv, oldenv);
             free(newenv);
             newenv = temp;
         }
         opal_setenv("LD_LIBRARY_PATH", newenv, true, &env);
         ORTE_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                              "%s plm:gridengine: reset LD_LIBRARY_PATH: %s",
                              ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                              newenv));
         free(newenv);
     }
                     
    /*
     * Iterate through the nodes.
     */
    for(nnode=0; nnode < map->num_nodes; nnode++) {
        pid_t pid;
        char *exec_path, *orted_path;
        char **exec_argv;

        /* if this daemon already exists, don't launch it! */
        if (nodes[nnode]->daemon_launched) {
            continue;
        }
        
        /* setup node name */
        free(argv[node_name_index1]);
        if (NULL != nodes[nnode]->username &&
            0 != strlen (nodes[nnode]->username)) {
            asprintf(&argv[node_name_index1], "%s@%s",
                      nodes[nnode]->username, nodes[nnode]->name);
        } else {
            argv[node_name_index1] = strdup(nodes[nnode]->name);
        }

        free(argv[node_name_index2]);
        argv[node_name_index2] = strdup(nodes[nnode]->name);

        /* fork a child to do qrsh */
        pid = fork();

        if (pid < 0) {
            ORTE_ERROR_LOG(ORTE_ERR_SYS_LIMITS_CHILDREN);
            rc = ORTE_ERR_SYS_LIMITS_CHILDREN;
            goto cleanup;
        }

        /* child */
        if (pid == 0) {
            char* var;
            long fd, fdmax = sysconf(_SC_OPEN_MAX);

            ORTE_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                 "%s plm:gridengine: launching on node %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 nodes[nnode]->name));

            /* setting exec_argv and exec_path for qrsh */
            exec_argv = &argv[0];

            sge_root = getenv("SGE_ROOT");
            sge_arch = getenv("ARC");
            asprintf(&exec_path, "%s/bin/%s/qrsh", sge_root, sge_arch);
            exec_path = opal_path_findv(exec_path, X_OK, environ, NULL);
            if (NULL == exec_path) {
                orte_show_help("help-plm-gridengine.txt", "bad-qrsh-path",
                    true, exec_path, sge_root, sge_arch);
                exit(-1);  /* forked child must ALWAYS exit, not return */
            }

            ORTE_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                 "%s plm:gridengine: exec_argv[0]=%s, exec_path=%s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 exec_argv[0], exec_path));
            
            /* setting orted_path for orted */
            orted_path = opal_path_findv(exec_argv[orted_index], 0, environ, NULL);

            if (NULL == orted_path && NULL == prefix_dir) {
                rc = orte_plm_gridengine_fill_orted_path(&orted_path);
                if (ORTE_SUCCESS != rc) {
                    exit(-1);  /* forked child must ALWAYS exit, not return */
                }
            } else {
                if (NULL != prefix_dir) {
                    orted_path = opal_os_path( false, prefix_dir, bin_base, "orted", NULL );
                }
                /* If we yet did not fill up the orted_path, do so now */
                if (NULL == orted_path) {
                    rc = orte_plm_gridengine_fill_orted_path(&orted_path);
                    if (ORTE_SUCCESS != rc) {
                        exit(-1);  /* forked child must ALWAYS exit, not return */
                    }
                }
            }
            asprintf(&argv[orted_index], orted_path);
            ORTE_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                 "%s plm:gridengine: orted_path=%s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 orted_path));
            
            var = opal_home_directory();
            if (NULL != var) {
                ORTE_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                     "%s plm:gridengine: changing to directory %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     var));
                /* Ignore errors -- what are we going to do?
                   (and we ignore errors on the remote nodes
                   in the fork plm, so this is consistent) */
                chdir(var);
            }
        
            /* pass the vpid */
            rc = orte_util_convert_vpid_to_string(&param, nodes[nnode]->daemon->name.vpid);
            if (ORTE_SUCCESS != rc) {
                orte_output(0, "plm:gridengine: unable to get daemon vpid as string");
                exit(-1);
            }
            free(argv[proc_vpid_index]);
            argv[proc_vpid_index] = strdup(param);
            free(param);

            if (0 > orte_output_get_verbosity(orte_plm_globals.output)) {
                /* setup stdin */
                int fd = open("/dev/null", O_RDWR, 0);
                dup2(fd, 0);
                close(fd);
            }

            /* close all file descriptors w/ exception of stdin/stdout/stderr */
            for(fd=3; fd<fdmax; fd++)
                close(fd);

            /* Set signal handlers back to the default.  Do this close
               to the execve() because the event library may (and likely
               will) reset them.  If we don't do this, the event
               library may have left some set that, at least on some
               OS's, don't get reset via fork() or exec().  Hence, the
               orted could be unkillable (for example). */
    
            set_handler_default(SIGTERM);
            set_handler_default(SIGINT);
            set_handler_default(SIGHUP);
            set_handler_default(SIGPIPE);
            set_handler_default(SIGCHLD);

            /* Unblock all signals, for many of the same reasons that
               we set the default handlers, above.  This is noticable
               on Linux where the event library blocks SIGTERM, but we
               don't want that blocked by the orted (or, more
               specifically, we don't want it to be blocked by the
               orted and then inherited by the ORTE processes that it
               forks, making them unkillable by SIGTERM). */
            sigprocmask(0, 0, &sigs);
            sigprocmask(SIG_UNBLOCK, &sigs, 0);
            
            /* exec the daemon */
            if (0 < orte_output_get_verbosity(orte_plm_globals.output)) {
                param = opal_argv_join(exec_argv, ' ');
                if (NULL != param) {
                    orte_output(0, "plm:gridengine: executing: %s", param);
                    free(param);
                }
            }
            execve(exec_path, exec_argv, env);
            orte_output(0, "plm:gridengine: execve failed with errno=%d\n", errno);
            exit(-1);
        } else { /* parent */
            ORTE_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                 "%s plm:gridengine: parent",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            
            /* setup callback on sigchild - wait until setup above is complete
             * as the callback can occur in the call to orte_wait_cb
             */
            orte_wait_cb(pid, orte_plm_gridengine_wait_daemon, NULL);
            
        }
    }

     /* wait for daemons to callback */
     if (ORTE_SUCCESS != (rc = orte_plm_base_daemon_callback(map->num_new_daemons))) {
         ORTE_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                              "%s plm:gridengine: daemon launch failed for job %s on error %s",
                              ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                              ORTE_JOBID_PRINT(active_job), ORTE_ERROR_NAME(rc)));
         goto cleanup;
     }
     
launch_apps:
    if (ORTE_SUCCESS != (rc = orte_plm_base_launch_apps(active_job))) {
        ORTE_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                            "%s plm:gridengine: launch of apps failed for job %s on error %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_JOBID_PRINT(active_job), ORTE_ERROR_NAME(rc)));
        goto cleanup;
    }

     /* get here if launch went okay */
    failed_launch = false;
    
cleanup:
    if (NULL != lib_base) {
        free(lib_base);
    }
    if (NULL != bin_base) {
        free(bin_base);
    }
    if (NULL != argv) {
        opal_argv_free(argv);
    }
    if (NULL != env) {
        opal_argv_free(env);
    }
    
    /* check for failed launch - if so, force terminate */
    if (failed_launch) {
        orte_plm_base_launch_failed(jdata->jobid, false, -1, ORTE_ERROR_DEFAULT_EXIT_CODE, ORTE_JOB_STATE_FAILED_TO_START);
    }
                     
    return rc;
}

/**
 * Terminate the job
 */
static int plm_gridengine_terminate_job(orte_jobid_t jobid)
{
    int rc;
    
    /* order them to kill their local procs for this job */
    if (ORTE_SUCCESS != (rc = orte_plm_base_orted_kill_local_procs(jobid))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

/**
 * Terminate the orteds for a given job
 */
static int plm_gridengine_terminate_orteds(void)
{
    int rc;
    
    /* now tell them to die! */
    if (ORTE_SUCCESS != (rc = orte_plm_base_orted_exit())) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

/**
 * Signal all processes associated with this job
 */
static int plm_gridengine_signal_job(orte_jobid_t jobid, int32_t signal)
{
    int rc;

    /* order them to pass this signal to their local procs */
    if (ORTE_SUCCESS != (rc = orte_plm_base_orted_signal_local_procs(jobid, signal))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

/**
 * Finalize
 */
static int plm_gridengine_finalize(void)
{
    int rc;
    
    /* cleanup any pending recvs */
    if (ORTE_SUCCESS != (rc = orte_plm_base_comm_stop())) {
        ORTE_ERROR_LOG(rc);
    }
    return ORTE_SUCCESS;
}

/**
 * Set signal handler
 */
static void set_handler_default(int sig)
{
    struct sigaction act;
    act.sa_handler = SIG_DFL;
    act.sa_flags = 0;
    sigemptyset(&act.sa_mask);

    sigaction(sig, &act, (struct sigaction *)0);
}
