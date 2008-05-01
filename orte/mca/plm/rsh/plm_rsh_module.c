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
#include "orte/constants.h"

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
#include "opal/util/bit_ops.h"

#include "orte/util/session_dir.h"

#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_wakeup.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"
#include "orte/util/nidmap.h"

#include "orte/mca/rml/rml.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ras/ras_types.h"
#include "orte/mca/rmaps/rmaps.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/odls/odls.h"
#include "orte/mca/rml/base/rml_contact.h"

#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/base.h"
#include "orte/mca/plm/base/plm_private.h"
#include "orte/mca/plm/rsh/plm_rsh.h"

#if OMPI_HAVE_POSIX_THREADS && OMPI_THREADS_HAVE_DIFFERENT_PIDS && OMPI_ENABLE_PROGRESS_THREADS
static int orte_plm_rsh_launch_threaded(orte_job_t *jdata);
#endif

static int remote_spawn(opal_buffer_t *launch);

orte_plm_base_module_t orte_plm_rsh_module = {
    orte_plm_rsh_init,
    orte_plm_base_set_hnp_name,
#if OMPI_HAVE_POSIX_THREADS && OMPI_THREADS_HAVE_DIFFERENT_PIDS && OMPI_ENABLE_PROGRESS_THREADS
    orte_plm_rsh_launch_threaded,
#else
    orte_plm_rsh_launch,
#endif
    remote_spawn,
    orte_plm_rsh_terminate_job,
    orte_plm_rsh_terminate_orteds,
    orte_plm_rsh_signal_job,
    orte_plm_rsh_finalize
};

typedef enum {
    ORTE_PLM_RSH_SHELL_BASH = 0,
    ORTE_PLM_RSH_SHELL_ZSH,
    ORTE_PLM_RSH_SHELL_TCSH,
    ORTE_PLM_RSH_SHELL_CSH,
    ORTE_PLM_RSH_SHELL_KSH,
    ORTE_PLM_RSH_SHELL_SH,
    ORTE_PLM_RSH_SHELL_UNKNOWN
} orte_plm_rsh_shell_t;

/* These strings *must* follow the same order as the enum
   ORTE_PLM_RSH_SHELL_* */
static const char * orte_plm_rsh_shell_name[] = {
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
static orte_plm_rsh_shell_t find_shell(char *shell);
static int find_children(int rank, int parent, int me, int num_procs);
static int daemon_callback(orte_std_cntr_t num_children);

/* local global storage of timing variables */
static struct timeval joblaunchstart, joblaunchstop;

/* local global storage */
static orte_jobid_t active_job=ORTE_JOBID_INVALID;
static orte_job_t *jdatorted;
static orte_proc_t **pdatorted;
static opal_buffer_t *launch_cmd;

/**
 * Init the module
 */
int orte_plm_rsh_init(void)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_plm_base_comm_start())) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}


/**
 * Check the Shell variable on the specified node
 */

static int orte_plm_rsh_probe(char *nodename, 
                              orte_plm_rsh_shell_t *shell)
{
    char ** argv;
    int argc, rc = ORTE_SUCCESS, i;
    int fd[2];
    pid_t pid;
    char outbuf[4096];

    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:rsh: going to check SHELL variable on node %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         nodename));

    *shell = ORTE_PLM_RSH_SHELL_UNKNOWN;
    if (pipe(fd)) {
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:rsh: pipe failed with errno=%d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             errno));
        return ORTE_ERR_IN_ERRNO;
    }
    if ((pid = fork()) < 0) {
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:rsh: fork failed with errno=%d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             errno));
        return ORTE_ERR_IN_ERRNO;
    }
    else if (pid == 0) {          /* child */
        if (dup2(fd[1], 1) < 0) {
            OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                 "%s plm:rsh: dup2 failed with errno=%d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 errno));
            exit(01);
        }
        /* Build argv array */
        argv = opal_argv_copy(mca_plm_rsh_component.agent_argv);
        argc = mca_plm_rsh_component.agent_argc;
        opal_argv_append(&argc, &argv, nodename);
        opal_argv_append(&argc, &argv, "echo $SHELL");

        execvp(argv[0], argv);
        exit(errno);
    }
    if (close(fd[1])) {
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:rsh: close failed with errno=%d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             errno));
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
                OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                     "%s plm:rsh: Unable to detect the remote shell (error %s)",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     strerror(errno)));
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
            for (i = 0; i < (int)(sizeof (orte_plm_rsh_shell_name)/
                                  sizeof(orte_plm_rsh_shell_name[0])); i++) {
                if ( 0 == strcmp(sh_name, orte_plm_rsh_shell_name[i]) ) {
                    *shell = i;
                    break;
                }
            }
        }
    }

    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:rsh: node %s has SHELL: %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         nodename,
                         (ORTE_PLM_RSH_SHELL_UNKNOWN == *shell) ? "UNHANDLED" : orte_plm_rsh_shell_name[*shell]));

    return rc;
}

static int total_num_daemons_calledback;
static bool total_callback_failed;


static void process_remote_launch_report(int fd, short event, void *data)
{
    orte_message_event_t *mev = (orte_message_event_t*)data;
    opal_buffer_t *buffer = mev->buffer;
    orte_vpid_t vpid=ORTE_VPID_INVALID;
    orte_std_cntr_t cnt, numd, i;
    int rc;
    uint8_t flag;
    char *rml_uri;
    orte_process_name_t daemon;
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:ssh:report_remote_launch from daemon %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&mev->sender)));
    
    /* unpack number of daemons being reported */
    cnt = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &numd, &cnt, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        total_callback_failed = true;
        return;
    }
    
    /* unpack flag that indicates if any failed */
    cnt = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &flag, &cnt, OPAL_UINT8))) {
        ORTE_ERROR_LOG(rc);
        total_callback_failed = true;
       return;
    }
    
    /* did any fail? */
    if (0 != flag) {
        /* unpack the failed vpid */
        cnt = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &vpid, &cnt, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            total_callback_failed = true;
        }
        if (ORTE_VPID_INVALID != vpid) {
            /* note that this daemon failed */
            pdatorted[vpid]->state = ORTE_PROC_STATE_FAILED_TO_START;
        }
        /* report that the daemon has failed so we can exit */
        orte_plm_base_launch_failed(active_job, true, -1, ORTE_ERROR_DEFAULT_EXIT_CODE, ORTE_JOB_STATE_FAILED_TO_START);
        return;
    }
    
    /* get their uri info */
    for (i=0; i < numd; i++) {
        cnt=1;
        opal_dss.unpack(buffer, &rml_uri, &cnt, OPAL_STRING);
        orte_rml.set_contact_info(rml_uri);
        orte_rml_base_parse_uris(rml_uri, &daemon, NULL);
        pdatorted[daemon.vpid]->rml_uri = strdup(rml_uri);
        orte_routed.update_route(&daemon, &daemon);
    }
    
    /* update num recvd */
    total_num_daemons_calledback += numd;
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:ssh:report_remote_launch reported %d for total of %d daemons reported",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (int)numd, total_num_daemons_calledback));

}

/*
 * Need a callback function to report failure of a remote daemon's launch
 */
static void report_remote_launch(int status, orte_process_name_t* sender,
                                 opal_buffer_t *buffer,
                                 orte_rml_tag_t tag, void *cbdata)
{
    int rc;
    
    /* don't process this right away - we need to get out of the recv before
     * we process the message as it may ask us to do something that involves
     * more messaging! Instead, setup an event so that the message gets processed
     * as soon as we leave the recv.
     *
     * The macro makes a copy of the buffer, which we release when processed - the incoming
     * buffer, however, is NOT released here, although its payload IS transferred
     * to the message buffer for later processing
     */
    ORTE_MESSAGE_EVENT(sender, buffer, tag, process_remote_launch_report);
    
    /* reissue the recv */
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_REPORT_REMOTE_LAUNCH,
                                 ORTE_RML_NON_PERSISTENT, report_remote_launch, NULL);
    if (rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        total_callback_failed = true;
    }
}


/**
 * Callback on daemon exit.
 */

static void orte_plm_rsh_wait_daemon(pid_t pid, int status, void* cbdata)
{
    unsigned long deltat;
    orte_std_cntr_t cnt=1;
    uint8_t flag;
    
    if (! WIFEXITED(status) || ! WEXITSTATUS(status) == 0) { /* if abnormal exit */
        /* if we are not the HNP, send a message to the HNP alerting it
         * to the failure
         */
        if (!orte_process_info.hnp) {
            opal_buffer_t buf;
            orte_vpid_t *vpid=(orte_vpid_t*)cbdata;
            OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                 "%s daemon %d failed with status %d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 (int)*vpid, WEXITSTATUS(status)));
            OBJ_CONSTRUCT(&buf, opal_buffer_t);
            opal_dss.pack(&buf, &cnt, 1, ORTE_STD_CNTR);
            flag = 1;
            opal_dss.pack(&buf, &flag, 1, OPAL_UINT8);
            opal_dss.pack(&buf, vpid, 1, ORTE_VPID);
            orte_rml.send_buffer(ORTE_PROC_MY_HNP, &buf, ORTE_RML_TAG_REPORT_REMOTE_LAUNCH, 0);
            OBJ_DESTRUCT(&buf);
        } else {
            orte_proc_t *daemon=(orte_proc_t*)cbdata;
            OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                 "%s daemon %d failed with status %d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 (int)daemon->name.vpid, WEXITSTATUS(status)));
            /* note that this daemon failed */
            daemon->state = ORTE_PROC_STATE_FAILED_TO_START;
            /* report that the daemon has failed so we can exit */
            orte_plm_base_launch_failed(active_job, true, pid, status, ORTE_JOB_STATE_FAILED_TO_START);
        }
    }

    /* release any waiting threads */
    OPAL_THREAD_LOCK(&mca_plm_rsh_component.lock);

    if (mca_plm_rsh_component.num_children-- >=
        mca_plm_rsh_component.num_concurrent ||
        mca_plm_rsh_component.num_children == 0) {
        opal_condition_signal(&mca_plm_rsh_component.cond);
    }

    if (orte_timing && mca_plm_rsh_component.num_children == 0) {
        if (0 != gettimeofday(&joblaunchstop, NULL)) {
            opal_output(0, "plm_rsh: could not obtain job launch stop time");
        } else {
            deltat = (joblaunchstop.tv_sec - joblaunchstart.tv_sec)*1000000 +
            (joblaunchstop.tv_usec - joblaunchstart.tv_usec);
            opal_output(0, "plm_rsh: total time to launch job is %lu usec", deltat);
        }
    }
    
    OPAL_THREAD_UNLOCK(&mca_plm_rsh_component.lock);

}

static int setup_launch(int *argcptr, char ***argvptr,
                        char *nodename,
                        int *node_name_index1, int *node_name_index2,
                        int *local_exec_index,
                        int *proc_vpid_index, char **lib_base, char **bin_base,
                        bool *remote_sh, bool *remote_csh)
{
    struct passwd *p;
    int argc;
    char **argv;
    char *param;
    orte_plm_rsh_shell_t shell;
    bool local_sh = false, local_csh = false;
    int rc;

    /* What is our local shell? */
    p = getpwuid(getuid());
    if( NULL == p ) {
        /* This user is unknown to the system. Therefore, there is no reason we
         * spawn whatsoever in his name. Give up with a HUGE error message.
         */
        opal_show_help( "help-plm-rsh.txt", "unknown-user", true, (int)getuid() );
        return ORTE_ERR_FATAL;
    } else {
        param = p->pw_shell;
        shell = find_shell(p->pw_shell);
    }
    /* If we didn't find it in getpwuid(), try looking at the $SHELL
     environment variable (see https://svn.open-mpi.org/trac/ompi/ticket/1060)
     */
    if (ORTE_PLM_RSH_SHELL_UNKNOWN == shell && 
        NULL != (param = getenv("SHELL"))) {
        shell = find_shell(param);
    }
    
    switch (shell) {
        case ORTE_PLM_RSH_SHELL_SH:  /* fall through */
        case ORTE_PLM_RSH_SHELL_KSH: /* fall through */
        case ORTE_PLM_RSH_SHELL_ZSH: /* fall through */
        case ORTE_PLM_RSH_SHELL_BASH: local_sh = true; break;
        case ORTE_PLM_RSH_SHELL_TCSH: /* fall through */
        case ORTE_PLM_RSH_SHELL_CSH:  local_csh = true; break;
        default:
            opal_output(0, "WARNING: local probe returned unhandled shell:%s assuming bash\n",
                        (NULL != param) ? param : "unknown");
            *remote_sh = true;
            break;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:rsh: local csh: %d, local sh: %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         local_csh, local_sh));
    
    /* What is our remote shell? */
    if (mca_plm_rsh_component.assume_same_shell) {
        *remote_sh = local_sh;
        *remote_csh = local_csh;
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:rsh: assuming same remote shell as local shell",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    } else {
        orte_plm_rsh_shell_t shell;
        rc = orte_plm_rsh_probe(nodename, &shell);
        
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        switch (shell) {
            case ORTE_PLM_RSH_SHELL_SH:  /* fall through */
            case ORTE_PLM_RSH_SHELL_KSH: /* fall through */
            case ORTE_PLM_RSH_SHELL_ZSH: /* fall through */
            case ORTE_PLM_RSH_SHELL_BASH: *remote_sh = true; break;
            case ORTE_PLM_RSH_SHELL_TCSH: /* fall through */
            case ORTE_PLM_RSH_SHELL_CSH:  *remote_csh = true; break;
            default:
                opal_output(0, "WARNING: rsh probe returned unhandled shell; assuming bash\n");
                *remote_sh = true;
        }
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:rsh: remote csh: %d, remote sh: %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         *remote_csh, *remote_sh));
    
    /*
     * Build argv array
     */
    argv = opal_argv_copy(mca_plm_rsh_component.agent_argv);
    argc = mca_plm_rsh_component.agent_argc;
    *node_name_index1 = argc;
    opal_argv_append(&argc, &argv, "<template>");
    
    /* add the daemon command (as specified by user) */
    *local_exec_index = argc;
    opal_argv_append(&argc, &argv, mca_plm_rsh_component.orted);
    
    /* if we are not tree launching or debugging, tell the daemon
     * to daemonize so we can launch the next group
     */
    if (!mca_plm_rsh_component.tree_spawn &&
        !orte_debug_flag &&
        !orte_debug_daemons_flag &&
        !orte_debug_daemons_file_flag) {
        opal_argv_append(&argc, &argv, "--daemonize");
    }
    
    /*
     * Add the basic arguments to the orted command line, including
     * all debug options
     */
    orte_plm_base_orted_append_basic_args(&argc, &argv,
                                          "env",
                                          proc_vpid_index,
                                          node_name_index2);
    
    if (0 < opal_output_get_verbosity(orte_plm_globals.output)) {
        param = opal_argv_join(argv, ' ');
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:rsh: final template argv:\n\t%s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (NULL == param) ? "NULL" : param));
        if (NULL != param) free(param);
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
    
    *lib_base = opal_basename(opal_install_dirs.libdir);
    *bin_base = opal_basename(opal_install_dirs.bindir);
    
    /* all done */
    *argcptr = argc;
    *argvptr = argv;
    return ORTE_SUCCESS;
}

/* actually ssh the child */
static void ssh_child(int argc, char **argv,
                      orte_vpid_t vpid, int proc_vpid_index,
                      int local_exec_index, char *prefix_dir,
                      char *bin_base, char *lib_base,
                      bool remote_sh, bool remote_csh)
{
    char** env;
    char* var;
    long fd, fdmax = sysconf(_SC_OPEN_MAX);
    int rc;
    char *exec_path;
    char **exec_argv;
    int fdin;
    sigset_t sigs;

    /* setup environment */
    env = opal_argv_copy(environ);
    
    /* ensure that only the ssh plm is selected on the remote daemon */
    var = mca_base_param_environ_variable("plm", NULL, NULL);
    opal_setenv(var, "ssh", true, &env);
    free(var);
    
    /* We don't need to sense an oversubscribed condition and set the sched_yield
     * for the node as we are only launching the daemons at this time. The daemons
     * are now smart enough to set the oversubscribed condition themselves when
     * they launch the local procs.
     */
    
    /* We cannot launch locally as this would cause multiple daemons to
     * exist on a node (HNP counts as a daemon). This is taken care of
     * by the earlier check for daemon_preexists, so we only have to worry
     * about remote launches here
     */
    exec_argv = argv;
    exec_path = strdup(mca_plm_rsh_component.agent_path);
    
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
                      mca_plm_rsh_component.orted);
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
                      mca_plm_rsh_component.orted);
        }
    }
    
    /* pass the vpid */
    rc = orte_util_convert_vpid_to_string(&var, vpid);
    if (ORTE_SUCCESS != rc) {
        opal_output(0, "orte_plm_rsh: unable to get daemon vpid as string");
        exit(-1);
    }
    free(argv[proc_vpid_index]);
    argv[proc_vpid_index] = strdup(var);
    free(var);
    
    /* setup stdin if verbosity is not set */
    if (0 > opal_output_get_verbosity(orte_plm_globals.output)) {
        fdin = open("/dev/null", O_RDWR);
        dup2(fdin, 0);
        close(fdin);
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
    var = opal_argv_join(argv, ' ');
    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:rsh: executing: (%s) [%s]",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         exec_path, (NULL == var) ? "NULL" : var));
    if (NULL != var) free(var);
    
    execve(exec_path, exec_argv, env);
    opal_output(0, "plm:rsh: execv of %s failed with errno=%s(%d)\n",
                exec_path, strerror(errno), errno);
    exit(-1);
}

static opal_buffer_t collected_uris;

static int construct_daemonmap(opal_buffer_t *data)
{
    opal_byte_object_t *bo;
    orte_std_cntr_t cnt;
    int rc;
    
    /* extract the byte object holding the daemonmap */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &bo, &cnt, OPAL_BYTE_OBJECT))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* unpack the nodemap - this will free the bytes in bo */
    if (ORTE_SUCCESS != (rc = orte_util_decode_nodemap(bo, &orte_daemonmap))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ORTE_SUCCESS;
}


/*
 * launch a set of daemons from a remote daemon
 */
static int remote_spawn(opal_buffer_t *launch)
{
    opal_list_item_t *item;
    orte_vpid_t vpid;
    orte_nid_t **nodes;
    int node_name_index1;
    int node_name_index2;
    int proc_vpid_index;
    int local_exec_index;
    char **argv = NULL;
    char *param, *rml_uri;
    char *prefix;
    int argc;
    int rc;
    bool remote_sh = false, remote_csh = false; 
    char *lib_base = NULL, *bin_base = NULL;
    bool failed_launch = true;
    pid_t pid;
    int num_children;
    orte_std_cntr_t n;

    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:rsh: remote spawn called",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* extract the prefix from the launch buffer */
    n = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(launch, &prefix, &n, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }            
    
    /* construct the daemonmap, if required - the decode function
     * will know what to do
     */
    if (ORTE_SUCCESS != (rc = construct_daemonmap(launch))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    nodes = (orte_nid_t**)orte_daemonmap.addr;
    vpid=ORTE_PROC_MY_NAME->vpid;
    
    /* rewind the buffer for use by our children */
    launch->unpack_ptr = launch->base_ptr;
    
    /* setup the launch cmd */
    launch_cmd = OBJ_NEW(opal_buffer_t);
    opal_dss.copy_payload(launch_cmd, launch);
    
    /* clear out any previous child info */
    while (NULL != (item = opal_list_remove_first(&mca_plm_rsh_component.children))) {
        OBJ_RELEASE(item);
    }
    /* reconstruct the child list */
    find_children(0, 0, ORTE_PROC_MY_NAME->vpid, orte_process_info.num_procs);
    
    /* if I have no children, just return */
    if (opal_list_is_empty(&mca_plm_rsh_component.children)) {
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:rsh: remote spawn - have no children!",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        failed_launch = false;
        rc = ORTE_SUCCESS;
        goto cleanup;
    }
    
    /* setup the launch */
    if (ORTE_SUCCESS != (rc = setup_launch(&argc, &argv, orte_process_info.nodename, &node_name_index1, &node_name_index2,
                                           &local_exec_index, &proc_vpid_index, &lib_base, &bin_base,
                                           &remote_sh, &remote_csh))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* tell the child that I am its parent so it reports back to me */
    rml_uri = orte_rml.get_contact_info();
    asprintf(&param, "\"%s\"", rml_uri);
    opal_argv_append(&argc, &argv, "--parent");
    opal_argv_append(&argc, &argv, param);
    free(param);
    free(rml_uri);
    
    /* setup the collection buffer so I can report all the URI's back
     * to the HNP when the launch completes
     */
    OBJ_CONSTRUCT(&collected_uris, opal_buffer_t);
    
    num_children = 0;
    for (item = opal_list_get_first(&mca_plm_rsh_component.children);
         item != opal_list_get_end(&mca_plm_rsh_component.children);
         item = opal_list_get_next(item)) {
        orte_namelist_t *child = (orte_namelist_t*)item;
        vpid = child->name.vpid;
        
        if (NULL == nodes[vpid]) {
            opal_output(0, "%s NULL in daemonmap at position %d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (int)vpid);
            rc = ORTE_ERR_NOT_FOUND;
            goto cleanup;
        }
        
        free(argv[node_name_index1]);
        argv[node_name_index1] = strdup(nodes[vpid]->name);
        
        free(argv[node_name_index2]);
        argv[node_name_index2] = strdup(nodes[vpid]->name);

        /* fork a child to exec the rsh/ssh session */
        pid = fork();
        if (pid < 0) {
            ORTE_ERROR_LOG(ORTE_ERR_SYS_LIMITS_CHILDREN);
            rc = ORTE_ERR_SYS_LIMITS_CHILDREN;
            goto cleanup;
        }
        
        /* child */
        if (pid == 0) {
            OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                 "%s plm:rsh: launching on node %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 nodes[vpid]->name));
            
            /* do the ssh launch - this will exit if it fails */
            ssh_child(argc, argv, vpid,
                      proc_vpid_index, local_exec_index, prefix, bin_base,
                      lib_base, remote_sh, remote_csh);
            
        } else { /* father */
            ++num_children;
            OPAL_THREAD_LOCK(&mca_plm_rsh_component.lock);
            /* This situation can lead to a deadlock if '--debug-daemons' is set.
             * However, the deadlock condition is tested at the begining of this
             * function, so we're quite confident it should not happens here.
             */
            if (mca_plm_rsh_component.num_children++ >=
                mca_plm_rsh_component.num_concurrent) {
                opal_condition_wait(&mca_plm_rsh_component.cond, &mca_plm_rsh_component.lock);
            }
            OPAL_THREAD_UNLOCK(&mca_plm_rsh_component.lock);
            
            /* setup callback on sigchild - wait until setup above is complete
             * as the callback can occur in the call to orte_wait_cb
             */
            orte_wait_cb(pid, orte_plm_rsh_wait_daemon, (void*)&vpid);
        }
    }
    
    /* wait for daemons to callback */
    if (ORTE_SUCCESS != (rc = daemon_callback(num_children))) {
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:rsh: daemon launch failed for job %s on error %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(active_job), ORTE_ERROR_NAME(rc)));
        goto cleanup;
    }
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
    
    /* check for failed launch */
    if (failed_launch) {
        /* report cannot launch this daemon to HNP */
        opal_buffer_t buf;
        orte_std_cntr_t cnt=1;
        uint8_t flag=1;
        OBJ_CONSTRUCT(&buf, opal_buffer_t);
        opal_dss.pack(&buf, &cnt, 1, ORTE_STD_CNTR);
        opal_dss.pack(&buf, &flag, 1, OPAL_UINT8);
        opal_dss.pack(&buf, &vpid, 1, ORTE_VPID);
        orte_rml.send_buffer(ORTE_PROC_MY_HNP, &buf, ORTE_RML_TAG_REPORT_REMOTE_LAUNCH, 0);
        OBJ_DESTRUCT(&buf);
    }
    
    return rc;
}


/**
 * Launch a daemon (bootproxy) on each node. The daemon will be responsible
 * for launching the application.
 */

/* When working in this function, ALWAYS jump to "cleanup" if
 * you encounter an error so that orterun will be woken up and
 * the job can cleanly terminate
 */
int orte_plm_rsh_launch(orte_job_t *jdata)
{
    orte_job_map_t *map;
    int node_name_index1;
    int node_name_index2;
    int proc_vpid_index;
    int local_exec_index;
    char **argv = NULL;
    char *prefix_dir;
    int argc;
    int rc;
    bool remote_sh = false, remote_csh = false; 
    char *lib_base = NULL, *bin_base = NULL;
    bool failed_launch = true;
    orte_app_context_t **apps;
    orte_node_t **nodes;
    orte_std_cntr_t nnode;
    orte_std_cntr_t num_children;
    
    if (orte_timing) {
        if (0 != gettimeofday(&joblaunchstart, NULL)) {
            opal_output(0, "plm_rsh: could not obtain start time");
            joblaunchstart.tv_sec = 0;
            joblaunchstart.tv_usec = 0;
        }        
    }
    
    /* create a jobid for this job */
    if (ORTE_SUCCESS != (rc = orte_plm_base_create_jobid(&jdata->jobid))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:rsh: setting up job %s",
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
    
    /* get the orted job data object */
    if (NULL == (jdatorted = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    pdatorted = (orte_proc_t**)jdatorted->procs->addr;
    
    if (0 == map->num_new_daemons) {
        /* have all the daemons we need - launch app */
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:rsh: no new daemons to launch",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        goto launch_apps;
    }
    
    if (0 < opal_output_get_verbosity(orte_plm_globals.output) &&
        mca_plm_rsh_component.num_concurrent < map->num_new_daemons) {
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
        opal_show_help("help-plm-rsh.txt", "deadlock-params",
                       true, mca_plm_rsh_component.num_concurrent, map->num_new_daemons);
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
    prefix_dir = apps[0]->prefix_dir;
    
    /* setup the launch */
    if (ORTE_SUCCESS != (rc = setup_launch(&argc, &argv, nodes[0]->name, &node_name_index1, &node_name_index2,
                                           &local_exec_index, &proc_vpid_index, &lib_base, &bin_base,
                                           &remote_sh, &remote_csh))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* if we are tree launching, find our children, get the launch cmd,
     * and setup the recv to hear of any remote failures
     */
    if (mca_plm_rsh_component.tree_spawn) {
        orte_daemon_cmd_flag_t command = ORTE_DAEMON_ADD_AND_SPAWN;

        launch_cmd= OBJ_NEW(opal_buffer_t);
        /* insert the add_and_spawn cmd */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(launch_cmd, &command, 1, ORTE_DAEMON_CMD))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(launch_cmd);
            goto cleanup;
        }
        /* pack the prefix since this will be needed by the next wave */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(launch_cmd, &prefix_dir, 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(launch_cmd);
            goto cleanup;
        }
        if (ORTE_SUCCESS != (rc = orte_odls.get_add_procs_data(launch_cmd, active_job))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(launch_cmd);
            goto cleanup;
        }
        find_children(0, 0, 0, jdatorted->num_procs);
        total_num_daemons_calledback = 1;  /* need to account for myself */
        total_callback_failed = false;
        num_children = opal_list_get_size(&mca_plm_rsh_component.children);
        /* we don't really need the collection buffer, but it needs to
         * be setup to avoid problems in the callback
         */
        OBJ_CONSTRUCT(&collected_uris, opal_buffer_t);
        rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_REPORT_REMOTE_LAUNCH,
                                     ORTE_RML_NON_PERSISTENT, report_remote_launch, NULL);
        if (rc != ORTE_SUCCESS) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(launch_cmd);
            goto cleanup;
        }
    }
    
    /*
     * Iterate through each of the nodes
     */
    
    nnode=0;
    while (nnode < map->num_nodes) {
        pid_t pid;
        opal_list_item_t *item;
        
        /* if we are tree launching, only launch our own children */
        if (mca_plm_rsh_component.tree_spawn) {
            for (item = opal_list_get_first(&mca_plm_rsh_component.children);
                 item != opal_list_get_end(&mca_plm_rsh_component.children);
                 item = opal_list_get_next(item)) {
                orte_namelist_t *child = (orte_namelist_t*)item;
                if (child->name.vpid == nodes[nnode]->daemon->name.vpid) {
                    goto launch;
                }
            }
            /* didn't find it - ignore this node */
            goto next_node;
        }
    
launch:
        /* if this daemon already exists, don't launch it! */
        if (nodes[nnode]->daemon_launched) {
            OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                 "%s plm:rsh:launch daemon already exists on node %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 nodes[nnode]->name));
            goto next_node;
        }
        
        /* if the node's daemon has not been defined, then we
         * have an error!
         */
        if (NULL == nodes[nnode]->daemon) {
            ORTE_ERROR_LOG(ORTE_ERR_FATAL);
            OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                 "%s plm:rsh:launch daemon failed to be defined on node %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 nodes[nnode]->name));
            rc = ORTE_ERR_FATAL;
            goto cleanup;
        }
        
        /* setup node name */
        free(argv[node_name_index1]);
        if (NULL != nodes[nnode]->username &&
            0 != strlen (nodes[nnode]->username)) {
            asprintf (&argv[node_name_index1], "%s@%s",
                      nodes[nnode]->username, nodes[nnode]->name);
        } else {
            argv[node_name_index1] = strdup(nodes[nnode]->name);
        }

        free(argv[node_name_index2]);
        argv[node_name_index2] = strdup(nodes[nnode]->name);
        
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:rsh: launching on node %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             nodes[nnode]->name));

        /* fork a child to exec the rsh/ssh session */
        pid = fork();
        if (pid < 0) {
            ORTE_ERROR_LOG(ORTE_ERR_SYS_LIMITS_CHILDREN);
            rc = ORTE_ERR_SYS_LIMITS_CHILDREN;
            goto cleanup;
        }

        /* child */
        if (pid == 0) {
            
            /* do the ssh launch - this will exit if it fails */
            ssh_child(argc, argv, nodes[nnode]->daemon->name.vpid,
                      proc_vpid_index, local_exec_index, prefix_dir, bin_base,
                      lib_base, remote_sh, remote_csh);
            
            
        } else { /* father */
            /* indicate this daemon has been launched */
            nodes[nnode]->daemon->state = ORTE_PROC_STATE_LAUNCHED;
            /* record the pid */
            nodes[nnode]->daemon->pid = pid;
            
            OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                 "%s plm:rsh: recording launch of daemon %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                ORTE_NAME_PRINT(&nodes[nnode]->daemon->name)));

            OPAL_THREAD_LOCK(&mca_plm_rsh_component.lock);
            /* This situation can lead to a deadlock if '--debug-daemons' is set.
             * However, the deadlock condition is tested at the begining of this
             * function, so we're quite confident it should not happens here.
             */
            if (mca_plm_rsh_component.num_children++ >=
                mca_plm_rsh_component.num_concurrent) {
                opal_condition_wait(&mca_plm_rsh_component.cond, &mca_plm_rsh_component.lock);
            }
            OPAL_THREAD_UNLOCK(&mca_plm_rsh_component.lock);
            
            /* setup callback on sigchild - wait until setup above is complete
             * as the callback can occur in the call to orte_wait_cb
             */
            orte_wait_cb(pid, orte_plm_rsh_wait_daemon, (void*)nodes[nnode]->daemon);

            /* if required - add delay to avoid problems w/ X11 authentication */
            if (0 < opal_output_get_verbosity(orte_plm_globals.output)
                && mca_plm_rsh_component.delay) {
                sleep(mca_plm_rsh_component.delay);
            }
        }
next_node:
    nnode++;
    }

    /* wait for daemons to callback */
    if (mca_plm_rsh_component.tree_spawn) {
        if (ORTE_SUCCESS != (rc = daemon_callback(num_children))) {
            OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                 "%s plm:rsh: daemon launch failed for job %s on error %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_JOBID_PRINT(active_job), ORTE_ERROR_NAME(rc)));
            goto cleanup;
        }
        failed_launch = false;
        goto cleanup;  /* apps are launched via callback */
    } else {
        if (ORTE_SUCCESS != (rc = orte_plm_base_daemon_callback(map->num_new_daemons))) {
            OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                 "%s plm:rsh: daemon launch failed for job %s on error %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_JOBID_PRINT(active_job), ORTE_ERROR_NAME(rc)));
            goto cleanup;
        }
    }
    
launch_apps:
    if (ORTE_SUCCESS != (rc = orte_plm_base_launch_apps(active_job))) {
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:rsh: launch of apps failed for job %s on error %s",
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

    /* check for failed launch - if so, force terminate */
    if (failed_launch) {
        orte_plm_base_launch_failed(jdata->jobid, false, -1, ORTE_ERROR_DEFAULT_EXIT_CODE, ORTE_JOB_STATE_FAILED_TO_START);
    }

    return rc;
}


static int find_children(int rank, int parent, int me, int num_procs)
{
    int i, bitmap, peer, hibit, mask, found;
    orte_namelist_t *child;
    
    /* is this me? */
    if (me == rank) {
        bitmap = opal_cube_dim(num_procs);
        
        hibit = opal_hibit(rank, bitmap);
        --bitmap;
        
        for (i = hibit + 1, mask = 1 << i; i <= bitmap; ++i, mask <<= 1) {
            peer = rank | mask;
            if (peer < num_procs) {
                    child = OBJ_NEW(orte_namelist_t);
                    child->name.jobid = ORTE_PROC_MY_NAME->jobid;
                    child->name.vpid = peer;
                    OPAL_OUTPUT_VERBOSE((3, orte_plm_globals.output,
                                         "%s plm:rsh find-children found child %s",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                         ORTE_NAME_PRINT(&child->name)));
                    
                    opal_list_append(&mca_plm_rsh_component.children, &child->item);
            }
        }
        return parent;
    }
    
    /* find the children of this rank */
    bitmap = opal_cube_dim(num_procs);
    
    hibit = opal_hibit(rank, bitmap);
    --bitmap;
    
    for (i = hibit + 1, mask = 1 << i; i <= bitmap; ++i, mask <<= 1) {
        peer = rank | mask;
        if (peer < num_procs) {
            /* execute compute on this child */
            if (0 <= (found = find_children(peer, rank, me, num_procs))) {
                return found;
            }
        }
    }
    return -1;
}

static orte_std_cntr_t num_callback;
static bool failed_launch;

static void process_launch_report(int fd, short event, void *data)
{
    orte_message_event_t *mev = (orte_message_event_t*)data;
    opal_buffer_t *buffer = mev->buffer;
    char *rml_uri;
    int rc, idx;
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:ssh:report_launch from daemon %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&mev->sender)));
    
    /* add this data to the collection buffer */
    opal_dss.copy_payload(&collected_uris, buffer);
    
    /* unpack its contact info */
    idx = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &rml_uri, &idx, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        failed_launch = true;
        goto CLEANUP;
    }
    
    /* set the contact info into the hash table */
    if (ORTE_SUCCESS != (rc = orte_rml.set_contact_info(rml_uri))) {
        ORTE_ERROR_LOG(rc);
        free(rml_uri);
        failed_launch = true;
        goto CLEANUP;
    }
    /* if I'm the HNP, lookup and record this daemon's contact info */
    if (orte_process_info.hnp) {
        /* this counts towards my total callback count */
        ++total_num_daemons_calledback;
        pdatorted[mev->sender.vpid]->rml_uri = strdup(rml_uri);
        OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                             "%s plm:ssh:report_launch now at %d total called back",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             total_num_daemons_calledback));
        
    }
    free(rml_uri);
    
    /* set the route to be direct */
    if (ORTE_SUCCESS != (rc = orte_routed.update_route(&mev->sender, &mev->sender))) {
        ORTE_ERROR_LOG(rc);
        failed_launch = true;
        goto CLEANUP;
    }
    
    /* send out the add-and-spawn command */
    if (0 > (rc = orte_rml.send_buffer(&mev->sender, launch_cmd, ORTE_RML_TAG_DAEMON, 0))) {
        ORTE_ERROR_LOG(rc);
        failed_launch = true;
        goto CLEANUP;
        
    }
    
CLEANUP:
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:ssh:report_launch %s for daemon %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         failed_launch ? "failed" : "completed",
                         ORTE_NAME_PRINT(&mev->sender)));
    
    if (failed_launch && orte_process_info.hnp) {
        orte_errmgr.incomplete_start(ORTE_PROC_MY_NAME->jobid, jdatorted->aborted_proc->exit_code);
    } else {
        num_callback++;
    }
    
}

static void report_launch(int status, orte_process_name_t* sender,
                          opal_buffer_t *buffer,
                          orte_rml_tag_t tag, void *cbdata)
{
    int rc;
    
    ORTE_MESSAGE_EVENT(sender, buffer, tag, process_launch_report);
    /* reissue the recv */
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_ORTED_CALLBACK,
                                 ORTE_RML_NON_PERSISTENT, report_launch, NULL);
    if (rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        failed_launch = true;
    }
}


static int daemon_callback(orte_std_cntr_t num_children)
{
    int rc;
    opal_buffer_t wireup, buf;
    orte_rml_cmd_flag_t cmd = ORTE_RML_TAG_RML_INFO_UPDATE;
    uint8_t flag;
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:ssh:daemon_callback",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* if I have no children, just return ok */
    if (0 == num_children) {
        return ORTE_SUCCESS;
    }
    
    num_callback = 0;
    failed_launch = false;
    
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_ORTED_CALLBACK,
                                 ORTE_RML_NON_PERSISTENT, report_launch, NULL);
    if (rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    ORTE_PROGRESSED_WAIT(failed_launch, num_callback, num_children);
    
    /* cancel the lingering recv */
    if (ORTE_SUCCESS != (rc = orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_ORTED_CALLBACK))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:ssh:daemon_callback completed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* all done launching my children - if I am NOT the HNP, then
     * send my number of children and their URI's to the HNP so it can know
     * when everyone is done and how to talk to them directly, if necessary
     */
    if (!orte_process_info.hnp) {
        OBJ_CONSTRUCT(&buf, opal_buffer_t);
        opal_dss.pack(&buf, &num_children, 1, ORTE_STD_CNTR);
        flag=0;
        opal_dss.pack(&buf, &flag, 1, OPAL_UINT8);
        /* copy over the collection buffer data */
        opal_dss.copy_payload(&buf, &collected_uris);
        orte_rml.send_buffer(ORTE_PROC_MY_HNP, &buf, ORTE_RML_TAG_REPORT_REMOTE_LAUNCH, 0);
        OBJ_DESTRUCT(&buf);
        return ORTE_SUCCESS;
    }

     
    /*
     * If I am the HNP, then I need to update the #procs in my process_info
     */    
    orte_process_info.num_procs = jdatorted->num_procs;
    /* update the grpcomm xcast tree(s) */
    if (ORTE_SUCCESS != (rc = orte_grpcomm.update_trees())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:ssh:daemon_callback trees updated\n\twaiting for all %d daemons to launch, currently at %d, %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (int)orte_process_info.num_procs, total_num_daemons_calledback,
                         total_callback_failed ? "Callback has failed" : "Callback continues"));
    
    /* wait for all daemons to complete launching - setup a recv
     * so that each daemon can tell me how many they launched.
     * When that number == total num daemons to be launched, then
     * we are done
     */
    ORTE_PROGRESSED_WAIT(total_callback_failed, total_num_daemons_calledback, (int)orte_process_info.num_procs);
        
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:ssh:daemon_callback - all daemons have launched!",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* get our wireup info so we can tell the entire daemon tree how
     * to communicate with each other
     */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    if (ORTE_SUCCESS != (rc = orte_routed.get_wireup_info(ORTE_PROC_MY_NAME->jobid, &buf))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buf);
        return rc;
    }
    /* if anything was inserted, send it out */
    if (0 < buf.bytes_used) {
        OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                             "%s plm:ssh:daemon_callback updating contact info",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        
        /* setup a buffer */
        OBJ_CONSTRUCT(&wireup, opal_buffer_t);
        /* pack the update_rml_info command */
        opal_dss.pack(&wireup, &cmd, 1, ORTE_RML_CMD);
        /* copy the data */
        opal_dss.copy_payload(&wireup, &buf);
        /* xcast it */
        orte_grpcomm.xcast(ORTE_PROC_MY_NAME->jobid, &wireup, ORTE_RML_TAG_RML_INFO_UPDATE);
        OBJ_DESTRUCT(&wireup);
    }
    OBJ_DESTRUCT(&buf);

    /* now we are good to go! */
    return ORTE_SUCCESS;
}


/**
 * Terminate all processes for a given job
 */
int orte_plm_rsh_terminate_job(orte_jobid_t jobid)
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
int orte_plm_rsh_terminate_orteds(void)
{
    int rc;
    
    /* now tell them to die! */
    if (ORTE_SUCCESS != (rc = orte_plm_base_orted_exit())) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

int orte_plm_rsh_signal_job(orte_jobid_t jobid, int32_t signal)
{
    int rc;
    
    /* order them to pass this signal to their local procs */
    if (ORTE_SUCCESS != (rc = orte_plm_base_orted_signal_local_procs(jobid, signal))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

int orte_plm_rsh_finalize(void)
{
    int rc;
    
    /* cleanup any pending recvs */
    if (ORTE_SUCCESS != (rc = orte_plm_base_comm_stop())) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}


/**
 * Handle threading issues.
 */

#if OMPI_HAVE_POSIX_THREADS && OMPI_THREADS_HAVE_DIFFERENT_PIDS && OMPI_ENABLE_PROGRESS_THREADS

struct orte_plm_rsh_stack_t {
    opal_condition_t cond;
    opal_mutex_t mutex;
    bool complete;
    orte_jobid_t jobid;
    int rc;
};
typedef struct orte_plm_rsh_stack_t orte_plm_rsh_stack_t;

static void orte_plm_rsh_stack_construct(orte_plm_rsh_stack_t* stack)
{
    OBJ_CONSTRUCT(&stack->mutex, opal_mutex_t);
    OBJ_CONSTRUCT(&stack->cond, opal_condition_t);
    stack->rc = 0;
    stack->complete = false;
}

static void orte_plm_rsh_stack_destruct(orte_plm_rsh_stack_t* stack)
{
    OBJ_DESTRUCT(&stack->mutex);
    OBJ_DESTRUCT(&stack->cond);
}

static OBJ_CLASS_INSTANCE(
    orte_plm_rsh_stack_t,
    opal_object_t,
    orte_plm_rsh_stack_construct,
    orte_plm_rsh_stack_destruct);

static void orte_plm_rsh_launch_cb(int fd, short event, void* args)
{
    orte_plm_rsh_stack_t *stack = (orte_plm_rsh_stack_t*)args;
    OPAL_THREAD_LOCK(&stack->mutex);
    stack->rc = orte_plm_rsh_launch(stack->jobid);
    stack->complete = true;
    opal_condition_signal(&stack->cond);
    OPAL_THREAD_UNLOCK(&stack->mutex);
}

static int orte_plm_rsh_launch_threaded(orte_jobid_t jobid)
{
    struct timeval tv = { 0, 0 };
    struct opal_event event;
    struct orte_plm_rsh_stack_t stack;

    OBJ_CONSTRUCT(&stack, orte_plm_rsh_stack_t);

    stack.jobid = jobid;
    if( opal_event_progress_thread() ) {
        stack.rc = orte_plm_rsh_launch( jobid );
    } else {
        opal_evtimer_set(&event, orte_plm_rsh_launch_cb, &stack);
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


static orte_plm_rsh_shell_t find_shell(char *shell) 
{
    int i         = 0;
    char *sh_name = NULL;

    sh_name = rindex(shell, '/');
    /* skip the '/' */
    ++sh_name;
    for (i = 0; i < (int)(sizeof (orte_plm_rsh_shell_name) /
                          sizeof(orte_plm_rsh_shell_name[0])); ++i) {
        if (0 == strcmp(sh_name, orte_plm_rsh_shell_name[i])) {
            return i;
        }
    }

    /* We didn't find it */
    return ORTE_PLM_RSH_SHELL_UNKNOWN;
}
