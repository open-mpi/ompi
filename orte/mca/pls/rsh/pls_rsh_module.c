/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
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

#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>
#include <string.h>
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif
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
#include "opal/util/trace.h"
#include "opal/util/basename.h"

#include "orte/util/sys_info.h"
#include "orte/util/univ_info.h"
#include "orte/util/session_dir.h"

#include "orte/runtime/orte_wait.h"
#include "orte/dss/dss.h"

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
#include "orte/mca/pls/rsh/pls_rsh.h"

#if OMPI_HAVE_POSIX_THREADS && OMPI_THREADS_HAVE_DIFFERENT_PIDS && OMPI_ENABLE_PROGRESS_THREADS
static int orte_pls_rsh_launch_threaded(orte_jobid_t jobid);
#endif


orte_pls_base_module_t orte_pls_rsh_module = {
#if OMPI_HAVE_POSIX_THREADS && OMPI_THREADS_HAVE_DIFFERENT_PIDS && OMPI_ENABLE_PROGRESS_THREADS
    orte_pls_rsh_launch_threaded,
#else
    orte_pls_rsh_launch,
#endif
    orte_pls_rsh_terminate_job,
    orte_pls_rsh_terminate_orteds,
    orte_pls_rsh_terminate_proc,
    orte_pls_rsh_signal_job,
    orte_pls_rsh_signal_proc,
    orte_pls_rsh_cancel_operation,
    orte_pls_rsh_finalize
};

static void set_handler_default(int sig);

enum {
    ORTE_PLS_RSH_SHELL_BASH = 0,
    ORTE_PLS_RSH_SHELL_ZSH,
    ORTE_PLS_RSH_SHELL_TCSH,
    ORTE_PLS_RSH_SHELL_CSH,
    ORTE_PLS_RSH_SHELL_KSH,
    ORTE_PLS_RSH_SHELL_SH,
    ORTE_PLS_RSH_SHELL_UNKNOWN
};

typedef int orte_pls_rsh_shell;

static const char * orte_pls_rsh_shell_name[] = {
    "bash",
    "zsh",
    "tcsh",       /* tcsh has to be first otherwise strstr finds csh */
    "csh",
    "ksh",
    "sh",
    "unknown"
};

/* local global storage of timing variables */
static unsigned long  mintime=999999999, miniter, maxtime=0, maxiter;
static float avgtime=0.0;
static struct timeval *launchstart;   
static struct timeval joblaunchstart, joblaunchstop;

/* local global storage of the list of active daemons */
static opal_list_t active_daemons;


/**
 * Check the Shell variable on the specified node
 */

static int orte_pls_rsh_probe(orte_mapped_node_t * node, orte_pls_rsh_shell * shell)
{
    char ** argv;
    int argc, rc, nfds, i;
    int fd[2];
    pid_t pid;
    fd_set readset;
    fd_set errset;
    char outbuf[4096];

    if (mca_pls_rsh_component.debug) {
        opal_output(0, "pls:rsh: going to check SHELL variable on node %s\n",
                    node->nodename);
    }
    *shell = ORTE_PLS_RSH_SHELL_UNKNOWN;
    /*
     * Build argv array
     */
    argv = opal_argv_copy(mca_pls_rsh_component.agent_argv);
    argc = mca_pls_rsh_component.agent_argc;
    opal_argv_append(&argc, &argv, node->nodename);
    opal_argv_append(&argc, &argv, "echo $SHELL");
    if (pipe(fd)) {
        opal_output(0, "pls:rsh: pipe failed with errno=%d\n", errno);
        return ORTE_ERR_IN_ERRNO;
    }
    if ((pid = fork()) < 0) {
        opal_output(0, "pls:rsh: fork failed with errno=%d\n", errno);
        return ORTE_ERR_IN_ERRNO;
    }
    else if (pid == 0) {          /* child */
        if (dup2(fd[1], 1) < 0) {
            opal_output(0, "pls:rsh: dup2 failed with errno=%d\n", errno);
            exit(01);
        }
        execvp(argv[0], argv);
        exit(errno);
    }
    if (close(fd[1])) {
        opal_output(0, "pls:rsh: close failed with errno=%d\n", errno);
        return ORTE_ERR_IN_ERRNO;
    }
    /* Monitor stdout */
    FD_ZERO(&readset);
    nfds = fd[0]+1;

    memset (outbuf, 0, sizeof (outbuf));
    rc = ORTE_SUCCESS;;
    while (ORTE_SUCCESS == rc) {
        int err;
        FD_SET (fd[0], &readset);
        errset = readset;
        err = select(nfds, &readset, NULL, &errset, NULL);
        if (err == -1) {
            if (errno == EINTR)
                continue;
            else {
                rc = ORTE_ERR_IN_ERRNO;
                break;
            }
        }
        if (FD_ISSET(fd[0], &errset) != 0)
            rc = ORTE_ERR_FATAL;
        /* In case we have something valid to read on stdin */
        if (FD_ISSET(fd[0], &readset) != 0) {
            ssize_t ret = 1;
            char temp[4096];
            char * ptr = outbuf;
            ssize_t outbufsize = sizeof(outbuf);

            memset (temp, 0, sizeof(temp));

            while (ret != 0) {
                ret = read (fd[0], temp, 256);
                if (ret < 0) {
                    if (errno == EINTR)
                        continue;
                    else {
                        rc = ORTE_ERR_IN_ERRNO;
                        break;
                    }
                }
                else {
                    if (outbufsize > 0) {
                        memcpy (ptr, temp, (ret > outbufsize) ? outbufsize : ret);
                        outbufsize -= ret;
                        ptr += ret;
                        if (outbufsize > 0)
                            *ptr = '\0';
                    }
                }
            }
            /* After reading complete string (aka read returns 0), we just break */
            break;
        }
    }

    /* Search for the substring of known shell-names */
    for (i = 0; i < (int)(sizeof (orte_pls_rsh_shell_name)/
                          sizeof(orte_pls_rsh_shell_name[0])); i++) {
        char *sh_name = NULL;

        sh_name = rindex(outbuf, '/');
        if ( sh_name != NULL ) {
            sh_name++; /* skip '/' */
            
            /* We cannot use "echo -n $SHELL" because -n is not portable. Therefore
             * we have to remove the "\n" */
            if ( sh_name[strlen(sh_name)-1] == '\n' ) {
                sh_name[strlen(sh_name)-1] = '\0';
            }
            if ( 0 == strcmp(sh_name, orte_pls_rsh_shell_name[i]) ) {
                *shell = i;
                break;
            }
        }
    }
    if (mca_pls_rsh_component.debug) {
        opal_output(0, "pls:rsh: node:%s has SHELL: %s\n",
                    node->nodename, orte_pls_rsh_shell_name[*shell]);
    }
    return rc;
}

/**
 * Fill the exec_path variable with the directory to the orted
 */

static int orte_pls_rsh_fill_exec_path ( char ** exec_path)
{
    struct stat buf;

    asprintf(exec_path, "%s/orted", opal_install_dirs.bindir);
    if (0 != stat(*exec_path, &buf)) {
        char *path = getenv("PATH");
        if (NULL == path) {
            path = ("PATH is empty!");
        }
        opal_show_help("help-pls-rsh.txt", "no-local-orted",
                        true, path, opal_install_dirs.bindir);
        return ORTE_ERR_NOT_FOUND;
    }
   return ORTE_SUCCESS;
}

/**
 * Callback on daemon exit.
 */

static void orte_pls_rsh_wait_daemon(pid_t pid, int status, void* cbdata)
{
    orte_pls_daemon_info_t *info = (orte_pls_daemon_info_t*) cbdata;
    orte_mapped_node_t *node;
    orte_mapped_proc_t *proc;
    opal_list_item_t *item;
    int rc;
    unsigned long deltat;
    struct timeval launchstop;

    /* if ssh exited abnormally, set the child processes to aborted
       and print something useful to the user.  The usual reasons for
       ssh to exit abnormally all are a pretty good indication that
       the child processes aren't going to start up properly.

       This should somehow be pushed up to the calling level, but we
       don't really have a way to do that just yet.
    */
    if (! WIFEXITED(status) || ! WEXITSTATUS(status) == 0) {
        /* get the mapping for our node so we can cancel the right things */
        rc = orte_rmaps.get_node_map(&node, info->cell,
                                     info->nodename, info->active_job);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        /* set state of all processes associated with the daemon as
           terminated */
        for(item =  opal_list_get_first(&node->procs);
            item != opal_list_get_end(&node->procs);
            item =  opal_list_get_next(item)) {
            proc = (orte_mapped_proc_t*) item;

                /* Clean up the session directory as if we were the
                   process itself.  This covers the case where the
                   process died abnormally and didn't cleanup its own
                   session directory. */

                orte_session_dir_finalize(&(proc->name));

                rc = orte_smr.set_proc_state(&(proc->name),
                                           ORTE_PROC_STATE_ABORTED, status);
            if (ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
            }
        }
        OBJ_RELEASE(node);

 cleanup:
        /* tell the user something went wrong */
        opal_output(0, "ERROR: A daemon on node %s failed to start as expected.",
                    info->nodename);
        opal_output(0, "ERROR: There may be more information available from");
        opal_output(0, "ERROR: the remote shell (see above).");

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
        OPAL_THREAD_LOCK(&mca_pls_rsh_component.lock);
        /* tell the system that this daemon is gone */
        if (ORTE_SUCCESS != (rc = orte_pls_base_remove_daemon(info))) {
            ORTE_ERROR_LOG(rc);
        }
        
        /* remove the daemon from our local list */
        opal_list_remove_item(&active_daemons, &info->super);
        OBJ_RELEASE(info);
        OPAL_THREAD_UNLOCK(&mca_pls_rsh_component.lock);
    } /* if abnormal exit */

    /* release any waiting threads */
    OPAL_THREAD_LOCK(&mca_pls_rsh_component.lock);
    /* first check timing request */
    if (mca_pls_rsh_component.timing) {
        if (0 != gettimeofday(&launchstop, NULL)) {
            opal_output(0, "pls_rsh: could not obtain stop time");
        } else {
            deltat = (launchstop.tv_sec - launchstart[info->name->vpid].tv_sec)*1000000 +
                     (launchstop.tv_usec - launchstart[info->name->vpid].tv_usec);
            avgtime = avgtime + deltat;
            if (deltat < mintime) {
                mintime = deltat;
                miniter = (unsigned long)info->name->vpid;
            }
            if (deltat > maxtime) {
                maxtime = deltat;
                maxiter = (unsigned long)info->name->vpid;
            }
        }
    }

    if (mca_pls_rsh_component.num_children-- >=
        mca_pls_rsh_component.num_concurrent ||
        mca_pls_rsh_component.num_children == 0) {
        opal_condition_signal(&mca_pls_rsh_component.cond);
    }

    if (mca_pls_rsh_component.timing && mca_pls_rsh_component.num_children == 0) {
        if (0 != gettimeofday(&joblaunchstop, NULL)) {
            opal_output(0, "pls_rsh: could not obtain job launch stop time");
        } else {
            deltat = (joblaunchstop.tv_sec - joblaunchstart.tv_sec)*1000000 +
                     (joblaunchstop.tv_usec - joblaunchstart.tv_usec);
            opal_output(0, "pls_rsh: total time to launch job is %lu usec", deltat);
            if (mintime < 999999999) {
                /* had at least one non-local node */
                avgtime = avgtime/opal_list_get_size(&active_daemons);
                opal_output(0, "pls_rsh: average time to launch one daemon %f usec", avgtime);
                opal_output(0, "pls_rsh: min time to launch a daemon was %lu usec for iter %lu", mintime, miniter);
                opal_output(0, "pls_rsh: max time to launch a daemon was %lu usec for iter %lu", maxtime, maxiter);
            } else {
                opal_output(0, "No nonlocal launches to report for timing info");
            }
        }
        free(launchstart);
    }
    
    OPAL_THREAD_UNLOCK(&mca_pls_rsh_component.lock);

}

/**
 * Launch a daemon (bootproxy) on each node. The daemon will be responsible
 * for launching the application.
 */

int orte_pls_rsh_launch(orte_jobid_t jobid)
{
    orte_job_map_t *map;
    opal_list_item_t *n_item;
    orte_mapped_node_t *rmaps_node;
    orte_std_cntr_t num_nodes;
    orte_vpid_t vpid;
    int node_name_index1;
    int node_name_index2;
    int proc_name_index;
    int local_exec_index, local_exec_index_end;
    char *jobid_string = NULL;
    char *uri, *param;
    char **argv = NULL, **tmp;
    char *prefix_dir;
    int argc;
    int rc;
    sigset_t sigs;
    struct passwd *p;
    bool remote_sh = false, remote_csh = false; 
    bool local_sh = false, local_csh = false;
    char *lib_base = NULL, *bin_base = NULL;
    orte_pls_daemon_info_t *dmn;

    if (mca_pls_rsh_component.timing) {
        if (0 != gettimeofday(&joblaunchstart, NULL)) {
            opal_output(0, "pls_rsh: could not obtain start time");
            joblaunchstart.tv_sec = 0;
            joblaunchstart.tv_usec = 0;
        }        
    }
    
    /* setup a list that will contain the info for all the daemons
     * so we can store it on the registry when done and use it
     * locally to track their state
     */
    OBJ_CONSTRUCT(&active_daemons, opal_list_t);

    /* Get the map for this job
     * We need the entire mapping for a couple of reasons:
     *  - need the prefix to start with.
     *  - need to know the nodes we are launching on
     * All other mapping responsibilities fall to orted in the fork PLS
     */
    rc = orte_rmaps.get_job_map(&map, jobid);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&active_daemons);
        return rc;
    }

    /* if the user requested that we re-use daemons,
     * launch the procs on any existing, re-usable daemons
     */
    if (orte_pls_base.reuse_daemons) {
        if (ORTE_SUCCESS != (rc = orte_pls_base_launch_on_existing_daemons(map))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(map);
            OBJ_DESTRUCT(&active_daemons);
            return rc;
        }
    }
    
    num_nodes = (orte_std_cntr_t)opal_list_get_size(&map->nodes);
    if (0 == num_nodes) {
        /* nothing left to do - just return */
        OBJ_RELEASE(map);
        OBJ_DESTRUCT(&active_daemons);
        return ORTE_SUCCESS;
    }

    if (mca_pls_rsh_component.debug_daemons &&
        mca_pls_rsh_component.num_concurrent < num_nodes) {
        /* we can't run in this situation, so pretty print the error
         * and exit
         */
        opal_show_help("help-pls-rsh.txt", "deadlock-params",
                       true, mca_pls_rsh_component.num_concurrent, num_nodes);
        OBJ_RELEASE(map);
        OBJ_DESTRUCT(&active_daemons);
        return ORTE_ERR_FATAL;
    }

    /*
     * After a discussion between Ralph & Jeff, we concluded that we
     * really are handling the prefix dir option incorrectly. It currently
     * is associated with an app_context, yet it really refers to the
     * location where OpenRTE/Open MPI is installed on a NODE. Fixing
     * this right now would involve significant change to orterun as well
     * as elsewhere, so we will intentionally leave this incorrect at this
     * point. The error, however, is identical to that seen in all prior
     * releases of OpenRTE/Open MPI, so our behavior is no worse than before.
     *
     * A note to fix this, along with ideas on how to do so, has been filed
     * on the project's Trac system under "feature enhancement".
     *
     * For now, default to the prefix_dir provided in the first app_context.
     * Since there always MUST be at least one app_context, we are safe in
     * doing this.
     */
    prefix_dir = map->apps[0]->prefix_dir;
    
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

    /* setup the orted triggers for passing their launch info */
    if (ORTE_SUCCESS != (rc = orte_smr.init_orted_stage_gates(jobid, num_nodes, NULL, NULL))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* need integer value for command line parameter */
    if (ORTE_SUCCESS != (rc = orte_ns.convert_jobid_to_string(&jobid_string, jobid))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* What is our local shell? */
    p = getpwuid(getuid());
    if (NULL != p) {
        int i         = 0;
        char *sh_name = NULL;

        sh_name = rindex(p->pw_shell, '/');
        sh_name++;
        for (i = 0; i < (int)(sizeof (orte_pls_rsh_shell_name)/
                              sizeof(orte_pls_rsh_shell_name[0])); i++) {
            if ( 0 == strcmp(sh_name, orte_pls_rsh_shell_name[i]) ) {
                switch (i) {
                case ORTE_PLS_RSH_SHELL_SH:  /* fall through */
                case ORTE_PLS_RSH_SHELL_KSH: /* fall through */
                case ORTE_PLS_RSH_SHELL_ZSH: /* fall through */
                case ORTE_PLS_RSH_SHELL_BASH: local_sh = true; break;
                case ORTE_PLS_RSH_SHELL_TCSH: /* fall through */
                case ORTE_PLS_RSH_SHELL_CSH:  local_csh = true; break;
                default:
                    opal_output(0, "WARNING: local probe returned unhandled shell:%s assuming bash\n",
                                orte_pls_rsh_shell_name[i]);
                    remote_sh = true;
                    break;
                }
            }
        }
        if ( i == ORTE_PLS_RSH_SHELL_UNKNOWN ) {
            opal_output(0, "WARNING: local probe returned unhandled shell:%s assuming bash\n",
                        orte_pls_rsh_shell_name[i]);
            remote_sh = true;
        }
        
        if (mca_pls_rsh_component.debug) {
            opal_output(0, "pls:rsh: local csh: %d, local sh: %d\n",
                        local_csh, local_sh);
        }
    }

    /* What is our remote shell? */
    if (mca_pls_rsh_component.assume_same_shell) {
        remote_sh = local_sh;
        remote_csh = local_csh;
        if (mca_pls_rsh_component.debug) {
            opal_output(0, "pls:rsh: assuming same remote shell as local shell");
        }
    } else {
        orte_pls_rsh_shell shell;
        rmaps_node = (orte_mapped_node_t*)opal_list_get_first(&map->nodes);
        rc = orte_pls_rsh_probe(rmaps_node, &shell);

        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        switch (shell) {
        case ORTE_PLS_RSH_SHELL_SH:  /* fall through */
        case ORTE_PLS_RSH_SHELL_KSH: /* fall through */
        case ORTE_PLS_RSH_SHELL_BASH: remote_sh = true; break;
        case ORTE_PLS_RSH_SHELL_TCSH: /* fall through */
        case ORTE_PLS_RSH_SHELL_CSH:  remote_csh = true; break;
        default:
            opal_output(0, "WARNING: rsh probe returned unhandled shell:%s assuming bash\n",
                        orte_pls_rsh_shell_name[shell]);
            remote_sh = true;
        }
    }
    if (mca_pls_rsh_component.debug) {
        opal_output(0, "pls:rsh: remote csh: %d, remote sh: %d\n",
                    remote_csh, remote_sh);
    }

    /*
     * Build argv array
     */
    argv = opal_argv_copy(mca_pls_rsh_component.agent_argv);
    argc = mca_pls_rsh_component.agent_argc;
    node_name_index1 = argc;
    opal_argv_append(&argc, &argv, "<template>");

    /* Do we need to source .profile on the remote side? */

    if (!(remote_csh || remote_sh)) {
        int i;
        tmp = opal_argv_split("( test ! -r ./.profile || . ./.profile;", ' ');
        if (NULL == tmp) {
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        for (i = 0; NULL != tmp[i]; ++i) {
            opal_argv_append(&argc, &argv, tmp[i]);
        }
        opal_argv_free(tmp);
    }

    /* add the daemon command (as specified by user) */
    local_exec_index = argc;
    opal_argv_append(&argc, &argv, mca_pls_rsh_component.orted);

    /* check for debug flags */
    orte_pls_base_mca_argv(&argc, &argv);

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

    local_exec_index_end = argc;
    if (!(remote_csh || remote_sh)) {
        opal_argv_append(&argc, &argv, ")");
    }
    if (mca_pls_rsh_component.debug) {
        param = opal_argv_join(argv, ' ');
        if (NULL != param) {
            opal_output(0, "pls:rsh: final template argv:");
            opal_output(0, "pls:rsh:     %s", param);
            free(param);
        }
    }

    /* Figure out the basenames for the libdir and bindir.  This
       requires some explanation:

       - Use opal_install_dirs.libdir and opal_install_dirs.bindir instead of -D'ing some macros
         in this directory's Makefile.am because it makes all the
         dependencies work out correctly.  These are defined in
         opal/install_dirs.h.

       - After a discussion on the devel-core mailing list, the
         developers decided that we should use the local directory
         basenames as the basis for the prefix on the remote note.
         This does not handle a few notable cases (e.g., f the
         libdir/bindir is not simply a subdir under the prefix, if the
         libdir/bindir basename is not the same on the remote node as
         it is here in the local node, etc.), but we decided that
         --prefix was meant to handle "the common case".  If you need
         something more complex than this, a) edit your shell startup
         files to set PATH/LD_LIBRARY_PATH properly on the remove
         node, or b) use some new/to-be-defined options that
         explicitly allow setting the bindir/libdir on the remote
         node.  We decided to implement these options (e.g.,
         --remote-bindir and --remote-libdir) to orterun when it
         actually becomes a problem for someone (vs. a hypothetical
         situation).

       Hence, for now, we simply take the basename of this install's
       libdir and bindir and use it to append this install's prefix
       and use that on the remote node.
    */

    lib_base = opal_basename(opal_install_dirs.libdir);
    bin_base = opal_basename(opal_install_dirs.bindir);

    /*
     * Iterate through each of the nodes
     */
    if (mca_pls_rsh_component.timing) {
        /* allocate space to track the start times */
        launchstart = (struct timeval*)malloc((num_nodes+vpid) * sizeof(struct timeval));
    }
    
    for(n_item =  opal_list_get_first(&map->nodes);
        n_item != opal_list_get_end(&map->nodes);
        n_item =  opal_list_get_next(n_item)) {
        orte_process_name_t* name;
        pid_t pid;
        char *exec_path;
        char **exec_argv;
        
        rmaps_node = (orte_mapped_node_t*)n_item;
        
        if (mca_pls_rsh_component.timing) {
            if (0 != gettimeofday(&launchstart[vpid], NULL)) {
                opal_output(0, "pls_rsh: could not obtain start time");
            }
        }
        
        /* new daemon - setup to record its info */
        dmn = OBJ_NEW(orte_pls_daemon_info_t);
        dmn->active_job = jobid;
        opal_list_append(&active_daemons, &dmn->super);
        
        /* setup node name */
        free(argv[node_name_index1]);
        if (NULL != rmaps_node->username &&
            0 != strlen (rmaps_node->username)) {
            asprintf (&argv[node_name_index1], "%s@%s",
                      rmaps_node->username, rmaps_node->nodename);
        } else {
            argv[node_name_index1] = strdup(rmaps_node->nodename);
        }

        free(argv[node_name_index2]);
        argv[node_name_index2] = strdup(rmaps_node->nodename);
        
        /* save it in the daemon info */
        dmn->nodename = strdup(rmaps_node->nodename);

        /* initialize daemons process name */
        rc = orte_ns.create_process_name(&name, rmaps_node->cell, 0, vpid);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        
        /* save it in the daemon info */
        dmn->cell = rmaps_node->cell;
        if (ORTE_SUCCESS != (rc = orte_dss.copy((void**)&(dmn->name), name, ORTE_NAME))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        /* fork a child to exec the rsh/ssh session */
        
        /* set the process state to "launched" */
        if (ORTE_SUCCESS != (rc = orte_smr.set_proc_state(name, ORTE_PROC_STATE_LAUNCHED, 0))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        pid = fork();
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

            if (mca_pls_rsh_component.debug) {
                opal_output(0, "pls:rsh: launching on node %s\n",
                            rmaps_node->nodename);
            }

            /* We don't need to sense an oversubscribed condition and set the sched_yield
             * for the node as we are only launching the daemons at this time. The daemons
             * are now smart enough to set the oversubscribed condition themselves when
             * they launch the local procs.
             */

            /* Is this a local launch?
             *
             * Not all node names may be resolvable (if we found
             * localhost in the hostfile, for example).  So first
             * check trivial case of node_name being same as the
             * current nodename, which must be local.  If that doesn't
             * match, check using ifislocal().
             */
            if (!mca_pls_rsh_component.force_rsh &&
                (0 == strcmp(rmaps_node->nodename, orte_system_info.nodename) ||
                opal_ifislocal(rmaps_node->nodename))) {
                if (mca_pls_rsh_component.debug) {
                    opal_output(0, "pls:rsh: %s is a LOCAL node\n",
                                rmaps_node->nodename);
                }
                if (mca_pls_rsh_component.timing) {
                    /* since this is a local launch, the daemon will never reach
                     * the waitpid callback - so set the start value to
                     * something nonsensical
                     */
                    launchstart[vpid].tv_sec = 0;
                    launchstart[vpid].tv_usec = 0;
                }
                
                exec_path = opal_path_findv(argv[local_exec_index], 0, environ, NULL);

                if (NULL == exec_path && NULL == prefix_dir) {
                    rc = orte_pls_rsh_fill_exec_path (&exec_path);
                    if (ORTE_SUCCESS != rc) {
                        exit(-1);  /* the forked process MUST exit */
                    }
                } else {
                    if (NULL != prefix_dir) {
                        exec_path = opal_os_path( false, prefix_dir, bin_base, "orted", NULL );
                    }
                    /* If we yet did not fill up the execpath, do so now */
                    if (NULL == exec_path) {
                        rc = orte_pls_rsh_fill_exec_path (&exec_path);
                        if (ORTE_SUCCESS != rc) {
                            exit(-1);  /* the forked process MUST exit */
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
                    newenv = opal_os_path( false, prefix_dir, bin_base, NULL );
                    oldenv = getenv("PATH");
                    if (NULL != oldenv) {
                        char *temp;
                        asprintf(&temp, "%s:%s", newenv, oldenv );
                        free( newenv );
                        newenv = temp;
                    }
                    opal_setenv("PATH", newenv, true, &environ);
                    if (mca_pls_rsh_component.debug) {
                        opal_output(0, "pls:rsh: reset PATH: %s", newenv);
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
                    opal_setenv("LD_LIBRARY_PATH", newenv, true, &environ);
                    if (mca_pls_rsh_component.debug) {
                        opal_output(0, "pls:rsh: reset LD_LIBRARY_PATH: %s",
                                    newenv);
                    }
                    free(newenv);
                }

                /* Since this is a local execution, we need to
                   potentially whack the final ")" in the argv (if
                   sh/csh conditionals, from above).  Note that we're
                   modifying the argv[] in the child process, so
                   there's no need to save this and restore it
                   afterward -- the parent's argv[] is unmodified. */
                if (NULL != argv[local_exec_index_end]) {
                    free(argv[local_exec_index_end]);
                    argv[local_exec_index_end] = NULL;
                }

                /* tell the daemon to setup its own process session/group */
                opal_argv_append(&argc, &argv, "--set-sid");
                exec_argv = &argv[local_exec_index];
                
                /* Finally, chdir($HOME) because we're making the
                   assumption that this is what will happen on
                   remote nodes (via rsh/ssh).  This allows a user
                   to specify a path that is relative to $HOME for
                   both the cwd and argv[0] and it will work on
                   all nodes -- including the local nost.
                   Otherwise, it would work on remote nodes and
                   not the local node.  If the user does not start
                   in $HOME on the remote nodes... well... let's
                   hope they start in $HOME.  :-) */
                var = getenv("HOME");
                if (NULL != var) {
                    if (mca_pls_rsh_component.debug) {
                        opal_output(0, "pls:rsh: changing to directory %s", var);
                    }
                    /* Ignore errors -- what are we going to do?
                       (and we ignore errors on the remote nodes
                       in the fork pls, so this is consistent) */
                    chdir(var);
                }
            } else {
                if (mca_pls_rsh_component.debug) {
                    opal_output(0, "pls:rsh: %s is a REMOTE node\n",
                                rmaps_node->nodename);
                }
                exec_argv = argv;
                exec_path = strdup(mca_pls_rsh_component.agent_path);

                if (NULL != prefix_dir) {
                    char *opal_prefix = getenv("OPAL_PREFIX");
                    if (remote_sh) {
                        asprintf (&argv[local_exec_index],
                                  "%s%s%s PATH=%s/%s:$PATH ; export PATH ; "
                                  "LD_LIBRARY_PATH=%s/%s:$LD_LIBRARY_PATH ; export LD_LIBRARY_PATH ; "
                                  "%s/%s/%s",
                                  (opal_prefix != NULL ? "OPAL_PREFIX=" : ""),
                                  (opal_prefix != NULL ? opal_prefix : ""),
                                  (opal_prefix != NULL ? " ;" : ""),
                                  prefix_dir, bin_base,
                                  prefix_dir, lib_base,
                                  prefix_dir, bin_base,
                                  mca_pls_rsh_component.orted);
                    }
                    if (remote_csh) {
                        /* [t]csh is a bit more challenging -- we
                           have to check whether LD_LIBRARY_PATH
                           is already set before we try to set it.
                           Must be very careful about obeying
                           [t]csh's order of evaluation and not
                           using a variable before it is defined.
                           See this thread for more details:
                           http://www.open-mpi.org/community/lists/users/2006/01/0517.php. */
                        asprintf (&argv[local_exec_index],
                                  "%s%s%s set path = ( %s/%s $path ) ; "
                                  "if ( $?LD_LIBRARY_PATH == 1 ) "
                                  "set OMPI_have_llp ; "
                                  "if ( $?LD_LIBRARY_PATH == 0 ) "
                                  "setenv LD_LIBRARY_PATH %s/%s ; "
                                  "if ( $?OMPI_have_llp == 1 ) "
                                  "setenv LD_LIBRARY_PATH %s/%s:$LD_LIBRARY_PATH ; "
                                  "%s/%s/%s",
                                  (opal_prefix != NULL ? "setenv OPAL_PREFIX " : ""),
                                  (opal_prefix != NULL ? opal_prefix : ""),
                                  (opal_prefix != NULL ? " ;" : ""),
                                  prefix_dir, bin_base,
                                  prefix_dir, lib_base,
                                  prefix_dir, lib_base,
                                  prefix_dir, bin_base,
                                  mca_pls_rsh_component.orted);
                    }
                }
            }

            /* setup process name */
            rc = orte_ns.get_proc_name_string(&name_string, name);
            if (ORTE_SUCCESS != rc) {
                opal_output(0, "orte_pls_rsh: unable to create process name");
                exit(-1);
            }
            free(argv[proc_name_index]);
            argv[proc_name_index] = strdup(name_string);

            if (!mca_pls_rsh_component.debug) {
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
            
            /* setup environment */
            env = opal_argv_copy(environ);
            var = mca_base_param_environ_variable("seed",NULL,NULL);
            opal_setenv(var, "0", true, &env);

            /* exec the daemon */
            if (mca_pls_rsh_component.debug) {
                param = opal_argv_join(exec_argv, ' ');
                if (NULL != param) {
                    char* env_array = opal_argv_join( env, ' ' );
                    opal_output(0, "pls:rsh: executing: (%s) %s [%s]",
                                exec_path, param, env_array);
                    free(param); free(env_array);
                }
            }
            execve(exec_path, exec_argv, env);
            opal_output(0, "pls:rsh: execv of %s failed with errno=%s(%d)\n",
                        exec_path, strerror(errno), errno);
            exit(-1);

        } else { /* father */
            OPAL_THREAD_LOCK(&mca_pls_rsh_component.lock);
            /* JJH Bug:
             * If we are in '--debug-daemons' we keep the ssh connection 
             * alive for the span of the run. If we use this option 
             * AND we launch on more than "num_concurrent" machines
             * then we will deadlock. No connections are terminated 
             * until the job is complete, no job is started
             * since all the orteds are waiting for all the others
             * to come online, and the others ore not launched because
             * we are waiting on those that have started to terminate
             * their ssh tunnels. :(
             */
            if (mca_pls_rsh_component.num_children++ >=
                mca_pls_rsh_component.num_concurrent) {
                opal_condition_wait(&mca_pls_rsh_component.cond, &mca_pls_rsh_component.lock);
            }
            OPAL_THREAD_UNLOCK(&mca_pls_rsh_component.lock);
            
            /* setup callback on sigchild - wait until setup above is complete
             * as the callback can occur in the call to orte_wait_cb
             */
            orte_wait_cb(pid, orte_pls_rsh_wait_daemon, dmn);

            /* if required - add delay to avoid problems w/ X11 authentication */
            if (mca_pls_rsh_component.debug && mca_pls_rsh_component.delay) {
                sleep(mca_pls_rsh_component.delay);
            }
            vpid++;
        }
        free(name);
    }
    
    /* all done, so store the daemon info on the registry */
    if (ORTE_SUCCESS != (rc = orte_pls_base_store_active_daemons(&active_daemons))) {
        ORTE_ERROR_LOG(rc);
    }

cleanup:
    OBJ_RELEASE(map);

    if (NULL != lib_base) {
        free(lib_base);
    }
    if (NULL != bin_base) {
        free(bin_base);
    }

    if (NULL != jobid_string) free(jobid_string);  /* done with this variable */
    if (NULL != argv) opal_argv_free(argv);

    return rc;
}


/**
 * Terminate all processes for a given job
 */
int orte_pls_rsh_terminate_job(orte_jobid_t jobid, struct timeval *timeout, opal_list_t *attrs)
{
    int rc;
    opal_list_t daemons;
    opal_list_item_t *item;
    
    OPAL_TRACE(1);
    
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
int orte_pls_rsh_terminate_orteds(orte_jobid_t jobid, struct timeval *timeout, opal_list_t *attrs)
{
    int rc;
    opal_list_t daemons;
    opal_list_item_t *item;
    
    OPAL_TRACE(1);
    
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
 * Terminate a specific process
 */
int orte_pls_rsh_terminate_proc(const orte_process_name_t* proc)
{
    OPAL_TRACE(1);
    
    return ORTE_ERR_NOT_IMPLEMENTED;
}

int orte_pls_rsh_signal_job(orte_jobid_t jobid, int32_t signal, opal_list_t *attrs)
{
    int rc;
    opal_list_t daemons;
    opal_list_item_t *item;
    
    OPAL_TRACE(1);
    
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

int orte_pls_rsh_signal_proc(const orte_process_name_t* proc, int32_t signal)
{
    OPAL_TRACE(1);
    
    return ORTE_ERR_NOT_IMPLEMENTED;
}

/**
 * Cancel an operation involving comm to an orted
 */
int orte_pls_rsh_cancel_operation(void)
{
    int rc;
    
    OPAL_TRACE(1);
    
    if (ORTE_SUCCESS != (rc = orte_pls_base_orted_cancel_operation())) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}


int orte_pls_rsh_finalize(void)
{
    int rc;
    
    /* cleanup any pending recvs */
    if (ORTE_SUCCESS != (rc = orte_pls_base_comm_stop())) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}


/**
 * Handle threading issues.
 */

#if OMPI_HAVE_POSIX_THREADS && OMPI_THREADS_HAVE_DIFFERENT_PIDS && OMPI_ENABLE_PROGRESS_THREADS

struct orte_pls_rsh_stack_t {
    opal_condition_t cond;
    opal_mutex_t mutex;
    bool complete;
    orte_jobid_t jobid;
    int rc;
};
typedef struct orte_pls_rsh_stack_t orte_pls_rsh_stack_t;

static void orte_pls_rsh_stack_construct(orte_pls_rsh_stack_t* stack)
{
    OBJ_CONSTRUCT(&stack->mutex, opal_mutex_t);
    OBJ_CONSTRUCT(&stack->cond, opal_condition_t);
    stack->rc = 0;
    stack->complete = false;
}

static void orte_pls_rsh_stack_destruct(orte_pls_rsh_stack_t* stack)
{
    OBJ_DESTRUCT(&stack->mutex);
    OBJ_DESTRUCT(&stack->cond);
}

static OBJ_CLASS_INSTANCE(
    orte_pls_rsh_stack_t,
    opal_object_t,
    orte_pls_rsh_stack_construct,
    orte_pls_rsh_stack_destruct);

static void orte_pls_rsh_launch_cb(int fd, short event, void* args)
{
    orte_pls_rsh_stack_t *stack = (orte_pls_rsh_stack_t*)args;
    OPAL_THREAD_LOCK(&stack->mutex);
    stack->rc = orte_pls_rsh_launch(stack->jobid);
    stack->complete = true;
    opal_condition_signal(&stack->cond);
    OPAL_THREAD_UNLOCK(&stack->mutex);
}

static int orte_pls_rsh_launch_threaded(orte_jobid_t jobid)
{
    struct timeval tv = { 0, 0 };
    struct opal_event event;
    struct orte_pls_rsh_stack_t stack;

    OBJ_CONSTRUCT(&stack, orte_pls_rsh_stack_t);

    stack.jobid = jobid;
    if( opal_event_progress_thread() ) {
        stack.rc = orte_pls_rsh_launch( jobid );
    } else {
        opal_evtimer_set(&event, orte_pls_rsh_launch_cb, &stack);
        opal_evtimer_add(&event, &tv);

        OPAL_THREAD_LOCK(&stack.mutex);
        while (stack.complete == false) {
            opal_condition_wait(&stack.cond, &stack.mutex);
        }
        OPAL_THREAD_UNLOCK(&stack.mutex);
    }
    OBJ_DESTRUCT(&stack);
    return stack.rc;
}

#endif


static void set_handler_default(int sig)
{
    struct sigaction act;

    act.sa_handler = SIG_DFL;
    act.sa_flags = 0;
    sigemptyset(&act.sa_mask);

    sigaction(sig, &act, (struct sigaction *)0);
}
