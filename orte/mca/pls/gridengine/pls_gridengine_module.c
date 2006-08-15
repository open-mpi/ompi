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
 * Copyright (c) 2006      Sun Microsystems, Inc.  All rights reserved.
 *                         Use is subject to license terms.
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

#include "opal/install_dirs.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/if.h"
#include "opal/util/path.h"
#include "opal/event/event.h"
#include "opal/util/show_help.h"
#include "opal/util/argv.h"
#include "opal/util/opal_environ.h"
#include "opal/util/output.h"
#include "opal/util/basename.h"
#include "orte/orte_constants.h"
#include "orte/util/univ_info.h"
#include "orte/util/session_dir.h"
#include "orte/runtime/orte_wait.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/pls/pls.h"
#include "orte/mca/pls/base/base.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ras/base/ras_base_node.h"
#include "orte/mca/rmaps/base/rmaps_base_map.h"
#include "orte/mca/rmgr/base/base.h"
#include "orte/mca/soh/soh.h"
#include "orte/mca/soh/base/base.h"
#include "orte/mca/pls/gridengine/pls_gridengine.h"
#include "orte/util/sys_info.h"

extern char **environ;

orte_pls_base_module_1_0_0_t orte_pls_gridengine_module = {
    orte_pls_gridengine_launch,
    orte_pls_gridengine_terminate_job,
    orte_pls_gridengine_terminate_proc,
    orte_pls_gridengine_signal_job,
    orte_pls_gridengine_signal_proc,
    orte_pls_gridengine_finalize
};

/**
 * struct used to have enough information to clean up the state of the
 * universe if a daemon aborts
 */
struct gridengine_daemon_info_t {
    opal_object_t super;
    orte_ras_node_t* node;
    orte_jobid_t jobid;
};
typedef struct gridengine_daemon_info_t gridengine_daemon_info_t;
static OBJ_CLASS_INSTANCE(gridengine_daemon_info_t,
                          opal_object_t,
                          NULL, NULL);
static void set_handler_default(int sig);
static int update_slot_keyval(orte_ras_node_t* node, int* slot_cnt);

/**
 * Fill the orted_path variable with the directory to the orted
 */
static int orte_pls_gridengine_fill_orted_path(char** orted_path)
{
    struct stat buf;
   
    asprintf(orted_path, "%s/orted", OPAL_BINDIR);
    if (0 != stat(*orted_path, &buf)) {
        char *path = getenv("PATH");
        if (NULL == path) {
            path = ("PATH is empty!");
        }
        opal_show_help("help-pls-gridengine.txt", "no-local-orted",
            true, path, OPAL_BINDIR);
        return ORTE_ERR_NOT_FOUND;
    }

    return ORTE_SUCCESS;
}

/**
 * Callback on daemon exit.
 */
static void orte_pls_gridengine_wait_daemon(pid_t pid, int status, void* cbdata)
{
    gridengine_daemon_info_t *info = (gridengine_daemon_info_t*) cbdata;
    opal_list_t map;
    opal_list_item_t* item;
    int rc;
      
    /* if qrsh exited abnormally, set the child processes to aborted
       and print something useful to the user.  The usual reasons for
       qrsh to exit abnormally all are a pretty good indication that
       the child processes aren't going to start up properly.

       This should somehow be pushed up to the calling level, but we
       don't really have a way to do that just yet.
    */
#ifdef __WINDOWS__
    printf("This is not implemented yet for windows\n");
    ORTE_ERROR_LOG(ORTE_ERROR);
    return;
#else
    if (! WIFEXITED(status) || ! WEXITSTATUS(status) == 0) {
        /* get the mapping for our node so we can cancel the right things */
        OBJ_CONSTRUCT(&map, opal_list_t);
        rc = orte_rmaps_base_get_node_map(orte_process_info.my_name->cellid,
                                          info->jobid,
                                          info->node->node_name,
                                          &map);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        /* set state of all processes associated with the daemon as
           terminated */
        for(item =  opal_list_get_first(&map);
            item != opal_list_get_end(&map);
            item =  opal_list_get_next(item)) {
            orte_rmaps_base_map_t* map = (orte_rmaps_base_map_t*) item;
            orte_std_cntr_t i;

            for (i = 0 ; i < map->num_procs ; ++i) {
                /* Clean up the session directory as if we were the
                   process itself.  This covers the case where the
                   process died abnormally and didn't cleanup its own
                   session directory. */

                orte_session_dir_finalize(&(map->procs[i])->proc_name);

                rc = orte_soh.set_proc_soh(&(map->procs[i]->proc_name),
                                           ORTE_PROC_STATE_ABORTED, status);
            }
            if (ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
            }
        }
        OBJ_DESTRUCT(&map);

 cleanup:
        /* tell the user something went wrong */
        opal_output(0, "ERROR: A daemon on node %s failed to start as expected.",
            info->node->node_name);
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
    }
#endif /* __WINDOWS__ */
    
    /* cleanup */
    OBJ_RELEASE(info->node);
    OBJ_RELEASE(info);
}

/**
 * Launch a daemon (bootproxy) on each node. The daemon will be responsible
 * for launching the application.
 */
int orte_pls_gridengine_launch(orte_jobid_t jobid)
{
    opal_list_t mapping;
    opal_list_item_t* m_item, *n_item;
    orte_std_cntr_t num_nodes;
    orte_vpid_t vpid;
    int node_name_index1;
    int node_name_index2;
    int proc_name_index;
    int orted_index;
    int call_yield_index;
    char *jobid_string;
    char *uri, *param;
    char **argv;
    int argc;
    int rc;
    sigset_t sigs;
    char *lib_base = NULL, *bin_base = NULL;
    char *sge_root, *sge_arch;
    
    /* Query the list of nodes allocated and mapped to this job.
     * We need the entire mapping for a couple of reasons:
     *  - need the prefix to start with.
     *  - need to know if we are launching on a subset of the allocated nodes
     * All other mapping responsibilities fall to orted in the fork PLS
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
    if (num_nodes == 0) {
        return ORTE_ERR_BAD_PARAM;
    }
    rc = orte_ns.reserve_range(0, num_nodes, &vpid);
    if (ORTE_SUCCESS != rc) {
        goto cleanup;
    }

    /* need integer value for command line parameter */
    if (ORTE_SUCCESS != (rc = orte_ns.convert_jobid_to_string(&jobid_string, jobid))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
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

    /* add the orted daemon in command and
     * force orted in the same ptree as sge_shephard with no daemonize */
    orted_index = argc;
    opal_argv_append(&argc, &argv, mca_pls_gridengine_component.orted);
    opal_argv_append(&argc, &argv, "--no-daemonize");

    /* check for debug flags */
    orte_pls_base_proxy_mca_argv(&argc, &argv);

    opal_argv_append(&argc, &argv, "--bootproxy");
    opal_argv_append(&argc, &argv, jobid_string);
    opal_argv_append(&argc, &argv, "--name");
    proc_name_index = argc;
    opal_argv_append(&argc, &argv, "<template>");

    /* tell the daemon how many procs are in the daemon's job */
    opal_argv_append(&argc, &argv, "--num_procs");
    asprintf(&param, "%lu", (unsigned long)(vpid + num_nodes));
    opal_argv_append(&argc, &argv, param);
    free(param);
    
    /* tell the daemon the starting vpid of the daemon's job */
    opal_argv_append(&argc, &argv, "--vpid_start");
    opal_argv_append(&argc, &argv, "0");

    opal_argv_append(&argc, &argv, "--nodename");
    node_name_index2 = argc;
    opal_argv_append(&argc, &argv, "<template>");

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

    opal_argv_append(&argc, &argv, "--mpi-call-yield");
    call_yield_index = argc;
    opal_argv_append(&argc, &argv, "0");

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
       the rationale for how / why we're doing this. */

    lib_base = opal_basename(OPAL_LIBDIR);
    bin_base = opal_basename(OPAL_BINDIR);

    /*
     * Iterate through each of the contexts
     */
    for(m_item = opal_list_get_first(&mapping);
        m_item != opal_list_get_end(&mapping);
        m_item = opal_list_get_next(m_item)) {
        orte_rmaps_base_map_t* map = (orte_rmaps_base_map_t*)m_item;
        char *prefix_dir = map->app->prefix_dir;

        /*
         * For each of the contexts - iterate through the nodes.
         */
        for(n_item =  opal_list_get_first(&map->nodes);
            n_item != opal_list_get_end(&map->nodes);
            n_item =  opal_list_get_next(n_item)) {
            orte_rmaps_base_node_t* rmaps_node = (orte_rmaps_base_node_t*)n_item;
            orte_ras_node_t* ras_node = rmaps_node->node;
            orte_process_name_t* name;
            pid_t pid;
            char *exec_path, *orted_path;
            char **exec_argv;
            int remain_slot_cnt;

            /* already launched on this node */
            if(ras_node->node_launched++ != 0) {
                if (mca_pls_gridengine_component.debug) {
                    opal_output(0, "pls:gridengine: already launched on this node, %s",
                        ras_node->node_name);
                }
                continue;
            }
            
            /* query the registry for the remaining gridengine slot count on
             * this node, and update the registry for the count for the
             * current process launch */
            if (ORTE_SUCCESS != (rc =
                update_slot_keyval(ras_node, &remain_slot_cnt))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }

            /* check for the unlikely scenario, because gridengine ras already
             * checks for it, but still provide a check there. */
            if (remain_slot_cnt < 0) {
                opal_show_help("help-pls-gridengine.txt", "insufficient-pe-slot",
                    true, ras_node->node_name, true);
                exit(-1); /* exit instead of return ORTE_ERR_OUT_OF_RESOURCE */
            }
            
            /* setup node name */
            free(argv[node_name_index1]);
            if (NULL != ras_node->node_username &&
                0 != strlen (ras_node->node_username)) {
                asprintf(&argv[node_name_index1], "%s@%s",
                          ras_node->node_username, ras_node->node_name);
            } else {
                argv[node_name_index1] = strdup(ras_node->node_name);
            }

            free(argv[node_name_index2]);
            argv[node_name_index2] = strdup(ras_node->node_name);

            /* initialize daemons process name */
            rc = orte_ns.create_process_name(&name, ras_node->node_cellid, 0, vpid);
            if (ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }

#ifdef __WINDOWS__
            printf("Unimplemented feature for windows\n");
            return;
#else
            /* fork a child to do qrsh */
            pid = fork();
#endif
            if (pid < 0) {
                rc = ORTE_ERR_OUT_OF_RESOURCE;
                goto cleanup;
            }

            /* child */
            if (pid == 0) {
                char* name_string;
                char** env;
                char* var;
                long fd, fdmax = sysconf(_SC_OPEN_MAX);

                if (mca_pls_gridengine_component.debug) {
                    opal_output(0, "pls:gridengine: launching on node %s",
                        ras_node->node_name);
                }

                /* set the progress engine schedule for this node.
                 * if node_slots is set to zero, then we default to
                 * NOT being oversubscribed
                 */
                if (ras_node->node_slots > 0 &&
                    (orte_std_cntr_t)opal_list_get_size(&rmaps_node->node_procs) > ras_node->node_slots) {
                    if (mca_pls_gridengine_component.debug) {
                        opal_output(0, "pls:gridengine: oversubscribed -- setting mpi_yield_when_idle to 1 (%d %d)",
                            ras_node->node_slots, opal_list_get_size(&rmaps_node->node_procs));
                    }
                    free(argv[call_yield_index]);
                    argv[call_yield_index] = strdup("1");
                } else {
                    if (mca_pls_gridengine_component.debug) {
                        opal_output(0, "pls:gridengine: not oversubscribed -- setting mpi_yield_when_idle to 0");
                    }
                    free(argv[call_yield_index]);
                    argv[call_yield_index] = strdup("0");
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
                    return ORTE_ERR_NOT_FOUND;
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
                        return rc;
                    }
                } else {
                    if (NULL != prefix_dir) {
                        asprintf(&argv[orted_index], "%s/%s/orted", 
                            prefix_dir, bin_base);
                        if (mca_pls_gridengine_component.debug) {
                            opal_output(0, "pls:gridengine: orted path=%s\n",
                                    argv[orted_index]);
                        }
                    }   
                    /* If we yet did not fill up the orted_path, do so now */
                    if (NULL == orted_path) {
                        rc = orte_pls_gridengine_fill_orted_path(&orted_path);
                        if (ORTE_SUCCESS != rc) {
                            return rc;
                        }
                    }
                }
                
                /* If we have a prefix, then modify the PATH and
                   LD_LIBRARY_PATH environment variables.  We're
                   already in the child process, so it's ok to modify
                   environ. */
                if (NULL != prefix_dir) {
                    char *oldenv, *newenv;
                    
                    /* Reset PATH */
                    oldenv = getenv("PATH");
                    if (NULL != oldenv) {
                        asprintf(&newenv, "%s/%s:%s", prefix_dir, 
                            bin_base, oldenv);
                    } else {
                        asprintf(&newenv, "%s/%s", prefix_dir, bin_base);
                    }
                    opal_setenv("PATH", newenv, true, &environ);
                    if (mca_pls_gridengine_component.debug) {
                        opal_output(0, "pls:gridengine: reset PATH: %s", newenv);
                    }
                    free(newenv);
                    
                    /* Reset LD_LIBRARY_PATH */
                    oldenv = getenv("LD_LIBRARY_PATH");
                    if (NULL != oldenv) {
                        asprintf(&newenv, "%s/%s:%s", prefix_dir, 
                            lib_base, oldenv);
                    } else {
                        asprintf(&newenv, "%s/%s", prefix_dir, lib_base);
                    }
                    opal_setenv("LD_LIBRARY_PATH", newenv, true, &environ);
                    if (mca_pls_gridengine_component.debug) {
                        opal_output(0, "pls:gridengine: reset LD_LIBRARY_PATH: %s",
                            newenv);
                    }
                    free(newenv);
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
                rc = orte_ns.get_proc_name_string(&name_string, name);
                if (ORTE_SUCCESS != rc) {
                    opal_output(0, "pls:gridengine: unable to create process name");
                    exit(-1);
                }
                free(argv[proc_name_index]);
                argv[proc_name_index] = strdup(name_string);

                if (!mca_pls_gridengine_component.debug) {
                    /* setup stdin */
                    int fd = open("/dev/null", O_RDWR);
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
                
                /* setup environment */
                env = opal_argv_copy(environ);
                var = mca_base_param_environ_variable("seed",NULL,NULL);
                opal_setenv(var, "0", true, &env);

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
                gridengine_daemon_info_t *daemon_info;

                if (mca_pls_gridengine_component.debug) {
                    opal_output(0, "pls:gridengine: parent");
                }   
                              
                /* save the daemons name on the node */
                if (ORTE_SUCCESS != (rc = orte_pls_base_proxy_set_node_name(ras_node,jobid,name))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }

                /* setup callback on sigchild - wait until setup above is complete
                 * as the callback can occur in the call to orte_wait_cb
                 */
                daemon_info = OBJ_NEW(gridengine_daemon_info_t);
                OBJ_RETAIN(ras_node);
                daemon_info->node = ras_node;
                daemon_info->jobid = jobid;
                orte_wait_cb(pid, orte_pls_gridengine_wait_daemon, daemon_info);
                
                vpid++;
            }
            free(name);
        }
    }
    
  cleanup:
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
    
    free(jobid_string);  /* done with this variable */
    opal_argv_free(argv);
    
    return rc;
}

/**
 * Query the registry for the gridengine slot count, and update it
 */
static int update_slot_keyval(orte_ras_node_t* ras_node, int* slot_cnt)
{
    int rc, *iptr, ivalue;
    orte_std_cntr_t num_tokens, i, get_cnt;
    orte_gpr_value_t** get_values;
    char **tokens;
    char *get_keys[] = {"orte-gridengine-slot-cnt", NULL};
    orte_gpr_keyval_t *condition;

    /* get token */
    if (ORTE_SUCCESS != (rc = orte_schema.get_node_tokens(&tokens,
        &num_tokens, ras_node->node_cellid, ras_node->node_name))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* setup condition/filter for query - return only processes that
     * are assigned to the specified node name
     */
    if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&condition, ORTE_NODE_NAME_KEY, ORTE_STRING, (void*)ras_node->node_name))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    rc = orte_gpr.get_conditional(
        ORTE_GPR_KEYS_OR|ORTE_GPR_TOKENS_OR,
            ORTE_NODE_SEGMENT,
            tokens,
            get_keys,
            1,
            &condition,
            &get_cnt,
            &get_values);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
     
    /* parse the response */
    for(i=0; i<get_cnt; i++) {
        orte_gpr_value_t* value = get_values[i];
        orte_std_cntr_t k;

        /* looking in each GPR container for the keyval */
        for(k=0; k < value->cnt; k++) {
            orte_gpr_keyval_t* keyval = value->keyvals[k];
            orte_data_value_t *put_value;
            
            if(strcmp(keyval->key, "orte-gridengine-slot-cnt") == 0) {
                if (ORTE_SUCCESS != (rc = orte_dss.get(
                    (void**)&iptr, keyval->value, ORTE_INT))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                *slot_cnt = *iptr;
                free(iptr);
                if (mca_pls_gridengine_component.debug) {
                    opal_output(0, "pls:gridengine: %s: registry shows PE slots=%d",
                        ras_node->node_name, *slot_cnt);
                }

                (*slot_cnt)--; /* account for the current launch */

                if (mca_pls_gridengine_component.debug) {
                    opal_output(0,"pls:gridengine: %s: decrementing, PE slots=%d",
                        ras_node->node_name, *slot_cnt);
                }
                
                put_value = OBJ_NEW(orte_data_value_t);
                if (NULL == put_value) {
                    ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                    return ORTE_ERR_OUT_OF_RESOURCE;
                }
                ivalue = *slot_cnt;
                put_value->type = ORTE_INT;
                put_value->data = &ivalue;

                /* put the keyvalue in the segment */
                if (ORTE_SUCCESS != (rc = orte_gpr.put_1(
                    ORTE_GPR_OVERWRITE|ORTE_GPR_TOKENS_XAND,
                        ORTE_NODE_SEGMENT,
                        tokens,
                        "orte-gridengine-slot-cnt",
                        put_value
                    ))) {
                    ORTE_ERROR_LOG(rc);
                }
                continue;
            }
        }
    }

    for(i=1; i<get_cnt; i++)
        OBJ_RELEASE(get_values[i]);
    if (NULL != get_values) free(get_values);
    opal_argv_free(tokens);

    return rc;
}

/**
 * Query the registry for all nodes participating in the job
 */
int orte_pls_gridengine_terminate_job(orte_jobid_t jobid)
{
    return orte_pls_base_proxy_terminate_job(jobid);
}

int orte_pls_gridengine_terminate_proc(const orte_process_name_t* proc)
{
    return orte_pls_base_proxy_terminate_proc(proc);
}

/**
 * Signal all processes associated with this job
 */
int orte_pls_gridengine_signal_job(orte_jobid_t jobid, int32_t signal)
{
    return orte_pls_base_proxy_signal_job(jobid, signal);
}

/**
 * Signal a specific process.
 */
int orte_pls_gridengine_signal_proc(const orte_process_name_t* proc, int32_t signal)
{
    return orte_pls_base_proxy_signal_proc(proc, signal);
}

/**
 * Finalize
 */
int orte_pls_gridengine_finalize(void)
{
    /* cleanup any pending recvs */
    orte_rml.recv_cancel(ORTE_RML_NAME_ANY, ORTE_RML_TAG_RMGR_CLNT);
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
