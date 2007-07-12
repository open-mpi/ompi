/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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
 * See pls_gridengine.h for an overview of how it works.
 */

#include "orte_config.h"
#include "orte/orte_constants.h"

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
#include "opal/util/show_help.h"
#include "opal/util/argv.h"
#include "opal/util/opal_environ.h"
#include "opal/util/output.h"
#include "opal/util/basename.h"

#include "orte/runtime/params.h"
#include "orte/util/univ_info.h"
#include "orte/util/session_dir.h"
#include "orte/util/sys_info.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_wakeup.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ras/ras_types.h"
#include "orte/mca/rmaps/rmaps.h"
#include "orte/mca/smr/smr.h"

#include "orte/mca/pls/pls.h"
#include "orte/mca/pls/base/base.h"
#include "orte/mca/pls/base/pls_private.h"
#include "orte/mca/pls/gridengine/pls_gridengine.h"

orte_pls_base_module_t orte_pls_gridengine_module = {
    orte_pls_gridengine_launch_job,
    orte_pls_gridengine_terminate_job,
    orte_pls_gridengine_terminate_orteds,
    orte_pls_gridengine_terminate_proc,
    orte_pls_gridengine_signal_job,
    orte_pls_gridengine_signal_proc,
    orte_pls_gridengine_finalize
};

static void set_handler_default(int sig);

/* global storage of active jobid being launched */
static orte_jobid_t active_job=ORTE_JOBID_INVALID;

/**
 * Fill the orted_path variable with the directory to the orted
 */
static int orte_pls_gridengine_fill_orted_path(char** orted_path)
{
    struct stat buf;
   
    asprintf(orted_path, "%s/orted", opal_install_dirs.bindir);
    if (0 != stat(*orted_path, &buf)) {
        char *path = getenv("PATH");
        if (NULL == path) {
            path = ("PATH is empty!");
        }
        opal_show_help("help-pls-gridengine.txt", "no-local-orted",
            true, path, opal_install_dirs.bindir);
        return ORTE_ERR_NOT_FOUND;
    }

    return ORTE_SUCCESS;
}

/**
 * Callback on daemon exit.
 */
static void orte_pls_gridengine_wait_daemon(pid_t pid, int status, void* cbdata)
{
    int rc;
    orte_buffer_t ack;
    int src[3] = {-1, -1};
      
    if (! WIFEXITED(status) || ! WEXITSTATUS(status) == 0) {
        /* tell the user something went wrong. We need to do this BEFORE we
         * set the state to ABORTED as that action will cause a trigger to
         * fire that will kill the job before any output would get printed!
         */
        opal_output(0, "ERROR: A daemon failed to start as expected.");
        opal_output(0, "ERROR: There may be more information available from");
        opal_output(0, "ERROR: the 'qstat -t' command on the Grid Engine tasks.");
        opal_output(0, "ERROR: If the problem persists, please restart the");
        opal_output(0, "ERROR: Grid Engine PE job");
        if (WIFEXITED(status)) {
            opal_output(0, "ERROR: The daemon exited unexpectedly with status %d.",
                        WEXITSTATUS(status));
        } else if (WIFSIGNALED(status)) {
#ifdef WCOREDUMP
            if (WCOREDUMP(status)) {
                opal_output(0, "The daemon received a signal %d (with core).",
                            WTERMSIG(status));
            } else {
                opal_output(0, "The daemon received a signal %d.", WTERMSIG(status));
            }
#else
            opal_output(0, "The daemon received a signal %d.", WTERMSIG(status));
#endif /* WCOREDUMP */
        } else {
            opal_output(0, "No extra status information is available: %d.", status);
        }
        
        /* need to fake a message to the daemon callback system so it can break out
         * of its receive loop
         */
        src[2] = pid;
        if(WIFSIGNALED(status)) {
            src[1] = WTERMSIG(status);
        }
        OBJ_CONSTRUCT(&ack, orte_buffer_t);
        if (ORTE_SUCCESS != (rc = orte_dss.pack(&ack, &src, 3, ORTE_INT))) {
            ORTE_ERROR_LOG(rc);
        }
        rc = orte_rml.send_buffer(ORTE_PROC_MY_NAME, &ack, ORTE_RML_TAG_ORTED_CALLBACK, 0);
        if (0 > rc) {
            ORTE_ERROR_LOG(rc);
        }
        OBJ_DESTRUCT(&ack);
        
        /*  The usual reasons for qrsh to exit abnormally all are a pretty good
            indication that the child processes aren't going to start up properly.
            Set the job state to indicate we failed to launch so orterun's exit status
            will be non-zero and forcibly terminate the job so orterun can exit
            */
        if (ORTE_SUCCESS != (rc = orte_smr.set_job_state(active_job, ORTE_JOB_STATE_FAILED_TO_START))) {
            ORTE_ERROR_LOG(rc);
        }
        
        if (ORTE_SUCCESS != (rc = orte_wakeup(active_job))) {
            ORTE_ERROR_LOG(rc);
        }
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
int orte_pls_gridengine_launch_job(orte_jobid_t jobid)
{
    orte_job_map_t *map=NULL;
    opal_list_item_t *n_item;
    orte_std_cntr_t num_nodes;
    int node_name_index1;
    int node_name_index2;
    int proc_name_index;
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
    
    /* set the active jobid */
    active_job = jobid;
    
    /* Get the map for this job.
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
        /* have all the daemons we need - launch app */
        if (mca_pls_gridengine_component.debug) {
            opal_output(0, "pls:rsh: no new daemons to launch");
        }
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
    if (mca_pls_gridengine_component.verbose) {
        opal_argv_append(&argc, &argv, "-verbose");
    }

    node_name_index1 = argc;
    opal_argv_append(&argc, &argv, "<template>");
                     
    /* add the orted daemon in command */
    orted_index = argc;
    opal_argv_append(&argc, &argv, mca_pls_gridengine_component.orted);

    /* By default, --no-daemonize will be used and orted will be forced to
     * to stay in the same ptree as sge_shephard. The problem with
     * --no-daemonize is that the qrsh -inherit connections will stay
     * persistent for the whole duration of the task to the remote nodes,
     * which may not be ideal for large number of nodes */
    if (! mca_pls_gridengine_component.daemonize_orted) {
        /* the actual orted option will be added when we
         * append_basic_args
         */
        orte_no_daemonize_flag = true;
    }
    
    /* Add basic orted command line options, including
     * all debug options
     */
    orte_pls_base_orted_append_basic_args(&argc, &argv,
                                          &proc_name_index,
                                          &node_name_index2,
                                          map->num_nodes);

     /* setup environment. The environment is common to all the daemons
      * so we only need to do this once
      */
     env = opal_argv_copy(environ);
     
     /* clean out any MCA component selection directives that
      * won't work on remote nodes
      */
     orte_pls_base_purge_mca_params(&env);
     
     if (mca_pls_gridengine_component.debug) {
        param = opal_argv_join(argv, ' ');
        if (NULL != param) {
            opal_output(0, "pls:gridengine: final template argv:");
            opal_output(0, "pls:gridengine:     %s", param);
            free(param);
        }
    }

    /* Figure out the basenames for the libdir and bindir.  There is a
       lengthy comment about this in pls_rsh_module.c explaining all
       the rationale for how / why we're doing this.
     */

    lib_base = opal_basename(opal_install_dirs.libdir);
    bin_base = opal_basename(opal_install_dirs.bindir);

    /* See the note about prefix_dir in the orte/mca/pls/slurm/pls_slurm.c
     * module. Fo here, just note that we must have at least one app_context,
     * and we take the prefix_dir from that first one.
     */
    prefix_dir = map->apps[0]->prefix_dir;

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
         if (mca_pls_gridengine_component.debug) {
             opal_output(0, "pls:gridengine: reset PATH: %s", newenv);
         }
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
         if (mca_pls_gridengine_component.debug) {
             opal_output(0, "pls:gridengine: reset LD_LIBRARY_PATH: %s",
                         newenv);
         }
         free(newenv);
     }
                     
    /*
     * Iterate through the nodes.
     */
    for(n_item =  opal_list_get_first(&map->nodes);
        n_item != opal_list_get_end(&map->nodes);
        n_item =  opal_list_get_next(n_item)) {
        orte_mapped_node_t* rmaps_node = (orte_mapped_node_t*)n_item;
        pid_t pid;
        char *exec_path, *orted_path;
        char **exec_argv;

        /* if this daemon already exists, don't launch it! */
        if (rmaps_node->daemon_preexists) {
            continue;
        }
        
        /* setup node name */
        free(argv[node_name_index1]);
        if (NULL != rmaps_node->username &&
            0 != strlen (rmaps_node->username)) {
            asprintf(&argv[node_name_index1], "%s@%s",
                      rmaps_node->username, rmaps_node->nodename);
        } else {
            argv[node_name_index1] = strdup(rmaps_node->nodename);
        }

        free(argv[node_name_index2]);
        argv[node_name_index2] = strdup(rmaps_node->nodename);

#ifdef __WINDOWS__
        printf("Unimplemented feature for windows\n");
        rc = ORTE_ERR_NOT_IMPLEMENTED;
        goto cleanup;
#else
        /* fork a child to do qrsh */
        pid = fork();
#endif
        if (pid < 0) {
            ORTE_ERROR_LOG(ORTE_ERR_SYS_LIMITS_CHILDREN);
            rc = ORTE_ERR_SYS_LIMITS_CHILDREN;
            goto cleanup;
        }

        /* child */
        if (pid == 0) {
            char* name_string;
            char* var;
            long fd, fdmax = sysconf(_SC_OPEN_MAX);

            if (mca_pls_gridengine_component.debug) {
                opal_output(0, "pls:gridengine: launching on node %s",
                    rmaps_node->nodename);
            }

            /* setting exec_argv and exec_path for qrsh */
            exec_argv = &argv[0];

            sge_root = getenv("SGE_ROOT");
            sge_arch = getenv("ARC");
            asprintf(&exec_path, "%s/bin/%s/qrsh", sge_root, sge_arch);
            exec_path = opal_path_findv(exec_path, X_OK, environ, NULL);
            if (NULL == exec_path) {
                opal_show_help("help-pls-gridengine.txt", "bad-qrsh-path",
                    true, exec_path, sge_root, sge_arch);
                exit(-1);  /* forked child must ALWAYS exit, not return */
            }

            if (mca_pls_gridengine_component.debug) {
                opal_output(0, "pls:gridengine: exec_argv[0]=%s, exec_path=%s",
                    exec_argv[0], exec_path);
            }
            
            /* setting orted_path for orted */
            orted_path = opal_path_findv(exec_argv[orted_index], 0, environ, NULL);

            if (NULL == orted_path && NULL == prefix_dir) {
                rc = orte_pls_gridengine_fill_orted_path(&orted_path);
                if (ORTE_SUCCESS != rc) {
                    exit(-1);  /* forked child must ALWAYS exit, not return */
                }
            } else {
                if (NULL != prefix_dir) {
                    orted_path = opal_os_path( false, prefix_dir, bin_base, "orted", NULL );
                }
                /* If we yet did not fill up the orted_path, do so now */
                if (NULL == orted_path) {
                    rc = orte_pls_gridengine_fill_orted_path(&orted_path);
                    if (ORTE_SUCCESS != rc) {
                        exit(-1);  /* forked child must ALWAYS exit, not return */
                    }
                }
            }
            asprintf(&argv[orted_index], orted_path);
            if (mca_pls_gridengine_component.debug) {
                opal_output(0, "pls:gridengine: orted_path=%s", orted_path);
            }
            
            var = getenv("HOME");
            if (NULL != var) {
                if (mca_pls_gridengine_component.debug) {
                    opal_output(0, "pls:gridengine: changing to directory %s",
                        var);
                }
                /* Ignore errors -- what are we going to do?
                   (and we ignore errors on the remote nodes
                   in the fork pls, so this is consistent) */
                chdir(var);
            }
        
            /* setup process name */
            rc = orte_ns.get_proc_name_string(&name_string, rmaps_node->daemon);
            if (ORTE_SUCCESS != rc) {
                opal_output(0, "pls:gridengine: unable to get daemon name as string");
                exit(-1);
            }
            free(argv[proc_name_index]);
            argv[proc_name_index] = strdup(name_string);

            if (!mca_pls_gridengine_component.debug) {
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
#ifndef __WINDOWS__
            set_handler_default(SIGHUP);
            set_handler_default(SIGPIPE);
#endif
            set_handler_default(SIGCHLD);

            /* Unblock all signals, for many of the same reasons that
               we set the default handlers, above.  This is noticable
               on Linux where the event library blocks SIGTERM, but we
               don't want that blocked by the orted (or, more
               specifically, we don't want it to be blocked by the
               orted and then inherited by the ORTE processes that it
               forks, making them unkillable by SIGTERM). */
#ifndef __WINDOWS__
            sigprocmask(0, 0, &sigs);
            sigprocmask(SIG_UNBLOCK, &sigs, 0);
#endif
            
            /* exec the daemon */
            if (mca_pls_gridengine_component.debug) {
                param = opal_argv_join(exec_argv, ' ');
                if (NULL != param) {
                    opal_output(0, "pls:gridengine: executing: %s", param);
                    free(param);
                }
            }
            execve(exec_path, exec_argv, env);
            opal_output(0, "pls:gridengine: execve failed with errno=%d\n", errno);
            exit(-1);
        } else { /* parent */
            if (mca_pls_gridengine_component.debug) {
                opal_output(0, "pls:gridengine: parent");
            }
            
            /* setup callback on sigchild - wait until setup above is complete
             * as the callback can occur in the call to orte_wait_cb
             */
            orte_wait_cb(pid, orte_pls_gridengine_wait_daemon, NULL);
            
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
     

     /* get here if launch went okay */
    failed_launch = false;
    
cleanup:
    if (NULL != map) {
        OBJ_RELEASE(map);
    }
                     
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
        if (ORTE_SUCCESS != (rc = orte_smr.set_job_state(jobid, ORTE_JOB_STATE_FAILED_TO_START))) {
            ORTE_ERROR_LOG(rc);
        }

        if (ORTE_SUCCESS != (rc = orte_wakeup(jobid))) {
            ORTE_ERROR_LOG(rc);
        }        
    }
                     
    return rc;
}

/**
 * Query the registry for all nodes participating in the job
 */
int orte_pls_gridengine_terminate_job(orte_jobid_t jobid, struct timeval *timeout, opal_list_t *attrs)
{
    int rc;
    
    /* order them to kill their local procs for this job */
    if (ORTE_SUCCESS != (rc = orte_pls_base_orted_kill_local_procs(jobid, timeout, attrs))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

int orte_pls_gridengine_terminate_proc(const orte_process_name_t* proc)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

/**
 * Terminate the orteds for a given job
 */
int orte_pls_gridengine_terminate_orteds(struct timeval *timeout, opal_list_t *attrs)
{
    int rc;
    
    /* now tell them to die! */
    if (ORTE_SUCCESS != (rc = orte_pls_base_orted_exit(timeout, attrs))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

/**
 * Signal all processes associated with this job
 */
int orte_pls_gridengine_signal_job(orte_jobid_t jobid, int32_t signal, opal_list_t *attrs)
{
    int rc;

    /* order them to pass this signal to their local procs */
    if (ORTE_SUCCESS != (rc = orte_pls_base_orted_signal_local_procs(jobid, signal, attrs))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

/**
 * Signal a specific process.
 */
int orte_pls_gridengine_signal_proc(const orte_process_name_t* proc, int32_t signal)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}


/**
 * Finalize
 */
int orte_pls_gridengine_finalize(void)
{
    int rc;
    
    /* cleanup any pending recvs */
    if (ORTE_SUCCESS != (rc = orte_pls_base_comm_stop())) {
        ORTE_ERROR_LOG(rc);
    }
    return ORTE_SUCCESS;
}

/**
 * Set signal handler
 */
static void set_handler_default(int sig)
{
#ifndef __WINDOWS__
    struct sigaction act;
    act.sa_handler = SIG_DFL;
    act.sa_flags = 0;
    sigemptyset(&act.sa_mask);

    sigaction(sig, &act, (struct sigaction *)0);
#endif
}
