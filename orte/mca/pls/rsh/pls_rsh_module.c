/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
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
#include "orte/runtime/orte_wakeup.h"
#include "orte/runtime/params.h"
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
    orte_pls_rsh_finalize
};

typedef enum {
    ORTE_PLS_RSH_SHELL_BASH = 0,
    ORTE_PLS_RSH_SHELL_ZSH,
    ORTE_PLS_RSH_SHELL_TCSH,
    ORTE_PLS_RSH_SHELL_CSH,
    ORTE_PLS_RSH_SHELL_KSH,
    ORTE_PLS_RSH_SHELL_SH,
    ORTE_PLS_RSH_SHELL_UNKNOWN
} orte_pls_rsh_shell_t;

/* These strings *must* follow the same order as the enum
   ORTE_PLS_RSH_SHELL_* */
static const char * orte_pls_rsh_shell_name[] = {
    "bash",
    "zsh",
    "tcsh",       /* tcsh has to be first otherwise strstr finds csh */
    "csh",
    "ksh",
    "sh",
    "unknown"
};

/*
 * Local functions
 */
static void set_handler_default(int sig);
static orte_pls_rsh_shell_t find_shell(char *shell);

/* local global storage of timing variables */
static struct timeval joblaunchstart, joblaunchstop;

/* global storage of active jobid being launched */
static orte_jobid_t active_job=ORTE_JOBID_INVALID;


/**
 * Check the Shell variable on the specified node
 */

static int orte_pls_rsh_probe(orte_mapped_node_t *node, 
                              orte_pls_rsh_shell_t *shell)
{
    char ** argv;
    int argc, rc = ORTE_SUCCESS, i;
    int fd[2];
    pid_t pid;
    char outbuf[4096];

    if (mca_pls_rsh_component.debug) {
        opal_output(0, "pls:rsh: going to check SHELL variable on node %s\n",
                    node->nodename);
    }
    *shell = ORTE_PLS_RSH_SHELL_UNKNOWN;
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
        /* Build argv array */
        argv = opal_argv_copy(mca_pls_rsh_component.agent_argv);
        argc = mca_pls_rsh_component.agent_argc;
        opal_argv_append(&argc, &argv, node->nodename);
        opal_argv_append(&argc, &argv, "echo $SHELL");

        execvp(argv[0], argv);
        exit(errno);
    }
    if (close(fd[1])) {
        opal_output(0, "pls:rsh: close failed with errno=%d\n", errno);
        return ORTE_ERR_IN_ERRNO;
    }

    {
        ssize_t ret = 1;
        char* ptr = outbuf;
        size_t outbufsize = sizeof(outbuf);

        do {
            ret = read (fd[0], ptr, outbufsize-1);
            if (ret < 0) {
                if (errno == EINTR)
                    continue;
                opal_output( 0, "Unable to detect the remote shell (error %s)\n",
                             strerror(errno) );
                rc = ORTE_ERR_IN_ERRNO;
                break;
            }
            if( outbufsize > 1 ) {
                outbufsize -= ret;
                ptr += ret;
            }
        } while( 0 != ret );
        *ptr = '\0';
    }
    close(fd[0]);

    if( outbuf[0] != '\0' ) {
        char *sh_name = rindex(outbuf, '/');
        if( NULL != sh_name ) {
            sh_name++; /* skip '/' */
            /* We cannot use "echo -n $SHELL" because -n is not portable. Therefore
             * we have to remove the "\n" */
            if ( sh_name[strlen(sh_name)-1] == '\n' ) {
                sh_name[strlen(sh_name)-1] = '\0';
            }
            /* Search for the substring of known shell-names */
            for (i = 0; i < (int)(sizeof (orte_pls_rsh_shell_name)/
                                  sizeof(orte_pls_rsh_shell_name[0])); i++) {
                if ( 0 == strcmp(sh_name, orte_pls_rsh_shell_name[i]) ) {
                    *shell = i;
                    break;
                }
            }
        }
    }
    if (mca_pls_rsh_component.debug) {
        if( ORTE_PLS_RSH_SHELL_UNKNOWN == *shell ) {
            opal_output(0, "pls:rsh: node:%s has unhandled SHELL\n",
                        node->nodename);
        } else {
            opal_output(0, "pls:rsh: node:%s has SHELL: %s\n",
                        node->nodename, orte_pls_rsh_shell_name[*shell]);
        }
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
    int rc;
    unsigned long deltat;
    orte_buffer_t ack;
    int src[3] = {-1, -1};
    
    if (! WIFEXITED(status) || ! WEXITSTATUS(status) == 0) {
        /* tell the user something went wrong */
        opal_output(0, "ERROR: A daemon failed to start as expected.");
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
        
        /*  The usual reasons for ssh to exit abnormally all are a pretty good
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
        
    } /* if abnormal exit */

    /* release any waiting threads */
    OPAL_THREAD_LOCK(&mca_pls_rsh_component.lock);

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
        }
    }
    
    OPAL_THREAD_UNLOCK(&mca_pls_rsh_component.lock);

}

/**
 * Launch a daemon (bootproxy) on each node. The daemon will be responsible
 * for launching the application.
 */

/* When working in this function, ALWAYS jump to "cleanup" if
 * you encounter an error so that orterun will be woken up and
 * the job can cleanly terminate
 */
int orte_pls_rsh_launch(orte_jobid_t jobid)
{
    orte_job_map_t *map=NULL;
    opal_list_item_t *n_item;
    orte_mapped_node_t *rmaps_node;
    int node_name_index1;
    int node_name_index2;
    int proc_name_index;
    int local_exec_index, local_exec_index_end;
    char *param;
    char **argv = NULL;
    char *prefix_dir;
    int argc;
    int rc;
    sigset_t sigs;
    struct passwd *p;
    bool remote_sh = false, remote_csh = false; 
    bool local_sh = false, local_csh = false;
    char *lib_base = NULL, *bin_base = NULL;
    bool failed_launch = true;
    orte_pls_rsh_shell_t shell;

    if (mca_pls_rsh_component.debug) {
        opal_output(0, "pls:rsh: launching job %ld", (long)jobid);
    }
    
    if (mca_pls_rsh_component.timing) {
        if (0 != gettimeofday(&joblaunchstart, NULL)) {
            opal_output(0, "pls_rsh: could not obtain start time");
            joblaunchstart.tv_sec = 0;
            joblaunchstart.tv_usec = 0;
        }        
    }
    
    /* set the active jobid */
    active_job = jobid;
    
    /* Get the map for this job
     * We need the entire mapping for a couple of reasons:
     *  - need the prefix to start with.
     *  - need to know the nodes we are launching on
     * All other mapping responsibilities fall to orted in the fork PLS
     */
    rc = orte_rmaps.get_job_map(&map, jobid);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    if (0 == map->num_new_daemons) {
        /* have all the daemons we need - launch app */
        if (mca_pls_rsh_component.debug) {
            opal_output(0, "pls:rsh: no new daemons to launch");
        }
        goto launch_apps;
    }
    
    if (mca_pls_rsh_component.debug_daemons &&
        mca_pls_rsh_component.num_concurrent < map->num_new_daemons) {
        /**
         * If we are in '--debug-daemons' we keep the ssh connection 
         * alive for the span of the run. If we use this option 
         * AND we launch on more than "num_concurrent" machines
         * then we will deadlock. No connections are terminated 
         * until the job is complete, no job is started
         * since all the orteds are waiting for all the others
         * to come online, and the others ore not launched because
         * we are waiting on those that have started to terminate
         * their ssh tunnels. :(
         * As we cannot run in this situation, pretty print the error
         * and return an error code.
         */
        opal_show_help("help-pls-rsh.txt", "deadlock-params",
                       true, mca_pls_rsh_component.num_concurrent, map->num_new_daemons);
        rc = ORTE_ERR_FATAL;
        goto cleanup;
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
    
    /* What is our local shell? */
    p = getpwuid(getuid());
    if( NULL == p ) {
        /* This user is unknown to the system. Therefore, there is no reason we
         * spawn whatsoever in his name. Give up with a HUGE error message.
         */
        opal_show_help( "help-pls-rsh.txt", "unknown-user", true, (int)getuid() );
        rc = ORTE_ERR_FATAL;
        goto cleanup;
    } else {
        param = p->pw_shell;
        shell = find_shell(p->pw_shell);
    }
    /* If we didn't find it in getpwuid(), try looking at the $SHELL
       environment variable (see
       https://svn.open-mpi.org/trac/ompi/ticket/1060) */
    if (ORTE_PLS_RSH_SHELL_UNKNOWN == shell && 
        NULL != (param = getenv("SHELL"))) {
        shell = find_shell(param);
    }

    switch (shell) {
    case ORTE_PLS_RSH_SHELL_SH:  /* fall through */
    case ORTE_PLS_RSH_SHELL_KSH: /* fall through */
    case ORTE_PLS_RSH_SHELL_ZSH: /* fall through */
    case ORTE_PLS_RSH_SHELL_BASH: local_sh = true; break;
    case ORTE_PLS_RSH_SHELL_TCSH: /* fall through */
    case ORTE_PLS_RSH_SHELL_CSH:  local_csh = true; break;
    default:
        opal_output(0, "WARNING: local probe returned unhandled shell:%s assuming bash\n",
                    (NULL != param) ? param : "unknown");
        remote_sh = true;
        break;
    }

    if (mca_pls_rsh_component.debug) {
        opal_output(0, "pls:rsh: local csh: %d, local sh: %d\n",
                    local_csh, local_sh);
    }

    /* What is our remote shell? */
    if (mca_pls_rsh_component.assume_same_shell) {
        remote_sh = local_sh;
        remote_csh = local_csh;
        if (mca_pls_rsh_component.debug) {
            opal_output(0, "pls:rsh: assuming same remote shell as local shell");
        }
    } else {
        orte_pls_rsh_shell_t shell;
        rmaps_node = (orte_mapped_node_t*)opal_list_get_first(&map->nodes);
        rc = orte_pls_rsh_probe(rmaps_node, &shell);

        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        switch (shell) {
        case ORTE_PLS_RSH_SHELL_SH:  /* fall through */
        case ORTE_PLS_RSH_SHELL_KSH: /* fall through */
        case ORTE_PLS_RSH_SHELL_ZSH: /* fall through */
        case ORTE_PLS_RSH_SHELL_BASH: remote_sh = true; break;
        case ORTE_PLS_RSH_SHELL_TCSH: /* fall through */
        case ORTE_PLS_RSH_SHELL_CSH:  remote_csh = true; break;
        default:
            opal_output(0, "WARNING: rsh probe returned unhandled shell; assuming bash\n");
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

    /* add the daemon command (as specified by user) */
    local_exec_index = argc;
    opal_argv_append(&argc, &argv, mca_pls_rsh_component.orted);

    /*
     * Add the basic arguments to the orted command line, including
     * all debug options
     */
    orte_pls_base_orted_append_basic_args(&argc, &argv,
                                          &proc_name_index,
                                          &node_name_index2,
                                          map->num_nodes);
    
    local_exec_index_end = argc;
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

       - Use opal_install_dirs.libdir and opal_install_dirs.bindir.

       - After a discussion on the devel-core mailing list, the
       developers decided that we should use the local directory
       basenames as the basis for the prefix on the remote note.
       This does not handle a few notable cases (e.g., if the
       libdir/bindir is not simply a subdir under the prefix, if the
       libdir/bindir basename is not the same on the remote node as
       it is here on the local node, etc.), but we decided that
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
    
    for(n_item =  opal_list_get_first(&map->nodes);
        n_item != opal_list_get_end(&map->nodes);
        n_item =  opal_list_get_next(n_item)) {
        pid_t pid;
        char *exec_path;
        char **exec_argv;
        
        rmaps_node = (orte_mapped_node_t*)n_item;
        
        /* if this daemon already exists, don't launch it! */
        if (rmaps_node->daemon_preexists) {
            continue;
        }
        
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
        
        /* fork a child to exec the rsh/ssh session */
        
        pid = fork();
        if (pid < 0) {
            ORTE_ERROR_LOG(ORTE_ERR_SYS_LIMITS_CHILDREN);
            rc = ORTE_ERR_SYS_LIMITS_CHILDREN;
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
            
            /* setup environment */
            env = opal_argv_copy(environ);
            
            /* clean any mca params that shouldn't go to the backend */
            orte_pls_base_purge_mca_params(&env);
            
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
                
                exec_path = opal_path_findv(argv[local_exec_index], 0, environ, NULL);
                
                if (NULL == exec_path && NULL == prefix_dir) {
                    rc = orte_pls_rsh_fill_exec_path (&exec_path);
                    if (ORTE_SUCCESS != rc) {
                        /* don't normally ERROR_LOG this problem as the function has already
                        * printed out a nice error message for us - do the ERROR_LOG only
                        * when we are in debug mode so we can see where it occurred
                        */
                        if (mca_pls_rsh_component.debug) {
                            ORTE_ERROR_LOG(rc);
                        }
                        exit(-1);  /* the forked process MUST exit */                        
                    }
                } else {
                    if (NULL != prefix_dir) {
                        exec_path = opal_os_path( false, prefix_dir, bin_base, mca_pls_rsh_component.orted, NULL );
                    }
                    /* If we yet did not fill up the execpath, do so now */
                    if (NULL == exec_path) {
                        rc = orte_pls_rsh_fill_exec_path (&exec_path);
                        if (ORTE_SUCCESS != rc) {
                            /* don't normally ERROR_LOG this problem as the function has already
                            * printed out a nice error message for us - do the ERROR_LOG only
                            * when we are in debug mode so we can see where it occurred
                            */
                            if (mca_pls_rsh_component.debug) {
                                ORTE_ERROR_LOG(rc);
                            }
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
                    opal_setenv("PATH", newenv, true, &env);
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
                    opal_setenv("LD_LIBRARY_PATH", newenv, true, &env);
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
                    } else if (remote_csh) {
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
            rc = orte_ns.get_proc_name_string(&name_string, rmaps_node->daemon);
            if (ORTE_SUCCESS != rc) {
                opal_output(0, "orte_pls_rsh: unable to get daemon name as string");
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
            
            /* exec the daemon */
            if (mca_pls_rsh_component.debug) {
                param = opal_argv_join(exec_argv, ' ');
                if (NULL != param) {
                    opal_output(0, "pls:rsh: executing: (%s) [%s]", exec_path, param);
                    free(param);
                }
            }
            execve(exec_path, exec_argv, env);
            opal_output(0, "pls:rsh: execv of %s failed with errno=%s(%d)\n",
                        exec_path, strerror(errno), errno);
            exit(-1);

        } else { /* father */
            /* indicate this daemon has been launched in case anyone is sitting on that trigger */
            if (ORTE_SUCCESS != (rc = orte_smr.set_proc_state(rmaps_node->daemon, ORTE_PROC_STATE_LAUNCHED, 0))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            
            OPAL_THREAD_LOCK(&mca_pls_rsh_component.lock);
            /* This situation can lead to a deadlock if '--debug-daemons' is set.
             * However, the deadlock condition is tested at the begining of this
             * function, so we're quite confident it should not happens here.
             */
            if (mca_pls_rsh_component.num_children++ >=
                mca_pls_rsh_component.num_concurrent) {
                opal_condition_wait(&mca_pls_rsh_component.cond, &mca_pls_rsh_component.lock);
            }
            OPAL_THREAD_UNLOCK(&mca_pls_rsh_component.lock);
            
            /* setup callback on sigchild - wait until setup above is complete
             * as the callback can occur in the call to orte_wait_cb
             */
            orte_wait_cb(pid, orte_pls_rsh_wait_daemon, NULL);

            /* if required - add delay to avoid problems w/ X11 authentication */
            if (mca_pls_rsh_component.debug && mca_pls_rsh_component.delay) {
                sleep(mca_pls_rsh_component.delay);
            }
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
 * Terminate all processes for a given job
 */
int orte_pls_rsh_terminate_job(orte_jobid_t jobid, struct timeval *timeout, opal_list_t *attrs)
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
int orte_pls_rsh_terminate_orteds(struct timeval *timeout, opal_list_t *attrs)
{
    int rc;
    
    /* now tell them to die! */
    if (ORTE_SUCCESS != (rc = orte_pls_base_orted_exit(timeout, attrs))) {
        ORTE_ERROR_LOG(rc);
    }
    
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
    
    /* order them to pass this signal to their local procs */
    if (ORTE_SUCCESS != (rc = orte_pls_base_orted_signal_local_procs(jobid, signal, attrs))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

int orte_pls_rsh_signal_proc(const orte_process_name_t* proc, int32_t signal)
{
    OPAL_TRACE(1);
    
    return ORTE_ERR_NOT_IMPLEMENTED;
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


static orte_pls_rsh_shell_t find_shell(char *shell) 
{
    int i         = 0;
    char *sh_name = NULL;

    sh_name = rindex(shell, '/');
    /* skip the '/' */
    ++sh_name;
    for (i = 0; i < (int)(sizeof (orte_pls_rsh_shell_name) /
                          sizeof(orte_pls_rsh_shell_name[0])); ++i) {
        if (0 == strcmp(sh_name, orte_pls_rsh_shell_name[i])) {
            return i;
        }
    }

    /* We didn't find it */
    return ORTE_PLS_RSH_SHELL_UNKNOWN;
}
