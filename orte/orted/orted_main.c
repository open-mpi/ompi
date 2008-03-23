/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco, Inc.  All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <stdio.h>
#include <ctype.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#include <fcntl.h>
#include <errno.h>
#include <signal.h>


#include "opal/event/event.h"
#include "opal/mca/base/base.h"
#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"
#include "opal/util/bit_ops.h"
#include "opal/util/cmd_line.h"
#include "opal/util/opal_environ.h"
#include "opal/util/os_path.h"
#include "opal/util/output.h"
#include "opal/util/printf.h"
#include "opal/util/show_help.h"
#include "opal/util/trace.h"
#include "opal/util/argv.h"
#include "opal/runtime/opal.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/daemon_init.h"
#include "opal/dss/dss.h"

#include "orte/util/sys_info.h"
#include "orte/util/proc_info.h"
#include "orte/util/session_dir.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_locks.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/odls/odls.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/ras/ras.h"

/* need access to the create_jobid fn used by plm components
* so we can set singleton name, if necessary
*/
#include "orte/mca/plm/base/plm_private.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"

#include "orte/orted/orted.h"

/*
 * Globals
 */

static opal_event_t term_handler;
static opal_event_t int_handler;
static opal_event_t pipe_handler;
#ifndef __WINDOWS__
static opal_event_t sigusr1_handler;
static opal_event_t sigusr2_handler;
#endif  /* __WINDOWS__ */
char *log_path = NULL;
static opal_event_t *orted_exit_event;

static void shutdown_callback(int fd, short flags, void *arg);
static void signal_callback(int fd, short event, void *arg);

static struct {
    bool help;
    bool set_sid;
    bool hnp;
    bool daemonize;
    char* name;
    char* vpid_start;
    char* num_procs;
    int uri_pipe;
    int singleton_died_pipe;
} orted_globals;

/*
 * define the orted context table for obtaining parameters
 */
opal_cmd_line_init_t orte_cmd_line_opts[] = {
    /* Various "obvious" options */
    { NULL, NULL, NULL, 'h', NULL, "help", 0,
      &orted_globals.help, OPAL_CMD_LINE_TYPE_BOOL,
      "This help message" },

    { "orted", "spin", NULL, 'd', NULL, "spin", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Have the orted spin until we can connect a debugger to it" },

    { "orte", "debug", NULL, 'd', NULL, "debug", 0,
        NULL, OPAL_CMD_LINE_TYPE_BOOL,
        "Debug the OpenRTE" },
        
    { "orte", "daemonize", NULL, '\0', NULL, "daemonize", 0,
      &orted_globals.daemonize, OPAL_CMD_LINE_TYPE_BOOL,
      "Daemonize the orted into the background" },

    { "orte", "debug", "daemons", '\0', NULL, "debug-daemons", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Enable debugging of OpenRTE daemons" },

    { "orte", "debug", "daemons_file", '\0', NULL, "debug-daemons-file", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Enable debugging of OpenRTE daemons, storing output in files" },

    { NULL, NULL, NULL, '\0', NULL, "hnp", 0,
      &orted_globals.hnp, OPAL_CMD_LINE_TYPE_BOOL,
      "Direct the orted to act as the HNP"},

    { "orte", "hnp", "uri", '\0', NULL, "hnp-uri", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "URI for the HNP"},
    
    { NULL, NULL, NULL, '\0', NULL, "set-sid", 0,
      &orted_globals.set_sid, OPAL_CMD_LINE_TYPE_BOOL,
      "Direct the orted to separate from the current session"},
    
    { NULL, NULL, NULL, '\0', NULL, "nodename", 1,
      &orte_system_info.nodename, OPAL_CMD_LINE_TYPE_STRING,
      "Node name as specified by host/resource description." },

    { "tmpdir", "base", NULL, '\0', NULL, "tmpdir", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Set the root for the session directory tree" },

    { NULL, NULL, NULL, '\0', NULL, "report-uri", 1,
      &orted_globals.uri_pipe, OPAL_CMD_LINE_TYPE_INT,
      "Report this process' uri on indicated pipe"},

    { NULL, NULL, NULL, '\0', NULL, "singleton-died-pipe", 1,
      &orted_globals.singleton_died_pipe, OPAL_CMD_LINE_TYPE_INT,
      "Watch on indicated pipe for singleton termination"},
    
    /* End of list */
    { NULL, NULL, NULL, '\0', NULL, NULL, 0,
      NULL, OPAL_CMD_LINE_TYPE_NULL, NULL }
};

int orte_daemon(int argc, char *argv[])
{
    int ret = 0;
    int fd;
    opal_cmd_line_t *cmd_line = NULL;
    char log_file[PATH_MAX];
    char *jobidstring;
    char *rml_uri;
    char *tmp1, *tmp2;
    int i;
    opal_buffer_t *buffer;
    char hostname[100];

    /* initialize the globals */
    memset(&orted_globals, 0, sizeof(orted_globals));
    /* initialize the singleton died pipe to an illegal value so we can detect it was set */
    orted_globals.singleton_died_pipe = -1;
 
    /* setup to check common command line options that just report and die */
    cmd_line = OBJ_NEW(opal_cmd_line_t);
    opal_cmd_line_create(cmd_line, orte_cmd_line_opts);
    mca_base_cmd_line_setup(cmd_line);
    if (ORTE_SUCCESS != (ret = opal_cmd_line_parse(cmd_line, false,
                                                   argc, argv))) {
        char *args = NULL;
        args = opal_cmd_line_get_usage_msg(cmd_line);
        opal_show_help("help-orted.txt", "orted:usage", false,
                       argv[0], args);
        free(args);
        return ret;
    }
    
    /*
     * Since this process can now handle MCA/GMCA parameters, make sure to
     * process them.
     */
    mca_base_cmd_line_process_args(cmd_line, &environ, &environ);
    
    /* Ensure that enough of OPAL is setup for us to be able to run */
    /*
     * NOTE: (JJH)
     *  We need to allow 'mca_base_cmd_line_process_args()' to process command
     *  line arguments *before* calling opal_init_util() since the command
     *  line could contain MCA parameters that affect the way opal_init_util()
     *  functions. AMCA parameters are one such option normally received on the
     *  command line that affect the way opal_init_util() behaves.
     *  It is "safe" to call mca_base_cmd_line_process_args() before 
     *  opal_init_util() since mca_base_cmd_line_process_args() does *not*
     *  depend upon opal_init_util() functionality.
     */
    if (OPAL_SUCCESS != opal_init_util()) {
        fprintf(stderr, "OPAL failed to initialize -- orted aborting\n");
        exit(1);
    }

    /* save the environment for use when launching application processes */
    orte_launch_environ = opal_argv_copy(environ);
    
    /* register and process the orte params */
    if (ORTE_SUCCESS != (ret = orte_register_params())) {
        return ret;
    }
    
    /* if orte_daemon_debug is set, let someone know we are alive right
     * away just in case we have a problem along the way
     */
    if (orte_debug_daemons_flag) {
        gethostname(hostname, 100);
        fprintf(stderr, "Daemon was launched on %s - beginning to initialize\n", hostname);
    }
    
    /* check for help request */
    if (orted_globals.help) {
        char *args = NULL;
        args = opal_cmd_line_get_usage_msg(cmd_line);
        opal_show_help("help-orted.txt", "orted:usage", false,
                       argv[0], args);
        free(args);
        return 1;
    }
#if defined(HAVE_SETSID) && !defined(__WINDOWS__)
    /* see if we were directed to separate from current session */
    if (orted_globals.set_sid) {
        setsid();
    }
#endif  /* !defined(__WINDOWS__) */
    /* see if they want us to spin until they can connect a debugger to us */
    i=0;
	/*orted_globals.spin = 1;*/
    while (orted_spin_flag) {
        i++;
        if (1000 < i) i=0;        
    }

    /* _After_ opal_init_util() (and various other bookkeeping) but
       _before_ orte_init(), we need to set an MCA param that tells
       the orted not to use any other libevent mechanism except
       "select" or "poll" (per potential pty issues with scalable
       fd-monitoring mechanisms such as epoll() and friends -- these
       issues *may* have been fixed in later OS releases and/or newer
       versions of libevent, but we weren't willing to do all the
       testing to figure it out.  So force the orted to use
       select()/poll() *only* -- there's so few fd's in the orted that
       it really doesn't matter. 
       
       Note that pty's work fine with poll() on most systems, so we
       prefer that (because it's more scalable than select()).
       However, poll() does *not* work with ptys on OS X, so we use
       select() there. */
    mca_base_param_reg_string_name("opal", "event_include",
                                   "Internal orted MCA param: tell opal_init() to use a specific mechanism in libevent",
                                   true, true, 
#ifdef __APPLE__
                                   "select",
#else
                                   "poll",
#endif
                                   NULL);
    tmp1 = mca_base_param_environ_variable("opal", NULL, "event_include");
    asprintf(&tmp2, "%s=select", tmp1);
    putenv(tmp2);
    free(tmp1);
    free(tmp2);
    
    /* Okay, now on to serious business! */
    
    if (orted_globals.hnp) {
        /* we are to be the hnp, so set that flag */
        orte_process_info.hnp = true;
        orte_process_info.daemon = false;
    } else {
        /* set ourselves to be just a daemon */
        orte_process_info.hnp = false;
        orte_process_info.daemon = true;
#if 0
        /* since I am a daemon, I need to ensure that orte_init selects
         * the rsh PLM module to support local spawns, if an rsh agent is
         * available
         */
        param = mca_base_param_environ_variable("plm","rsh",NULL);
        putenv(param);
        free(param);
#endif
    }

#if OPAL_ENABLE_FT == 1
    /* Mark as a tool program */
    opal_setenv(mca_base_param_env_var("opal_cr_is_tool"),
                "1",
                true, &environ);
#endif

    /* detach from controlling terminal
     * otherwise, remain attached so output can get to us
     */
    if(!orte_debug_flag &&
       !orte_debug_daemons_flag &&
       orted_globals.daemonize) {
        opal_daemon_init(NULL);
    }
    
    /* Set the flag telling OpenRTE that I am NOT a
     * singleton, but am "infrastructure" - prevents setting
     * up incorrect infrastructure that only a singleton would
     * require.
     */
    if (ORTE_SUCCESS != (ret = orte_init(ORTE_NON_TOOL))) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    
    /* insert our contact info into our process_info struct so we
     * have it for later use and set the local daemon field to our name
     */
    orte_process_info.my_daemon_uri = orte_rml.get_contact_info();
    ORTE_PROC_MY_DAEMON->jobid = ORTE_PROC_MY_NAME->jobid;
    ORTE_PROC_MY_DAEMON->vpid = ORTE_PROC_MY_NAME->vpid;
    
    /* if I am also the hnp, then update that contact info field too */
    if (orte_process_info.hnp) {
        orte_process_info.my_hnp_uri = orte_rml.get_contact_info();
        ORTE_PROC_MY_HNP->jobid = ORTE_PROC_MY_NAME->jobid;
        ORTE_PROC_MY_HNP->vpid = ORTE_PROC_MY_NAME->vpid;
    }
    
    /* setup an event we can wait for to tell
     * us to terminate - both normal and abnormal
     * termination will call us here. Use the same exit
     * fd as orterun so that orte_comm can wake either of us up
     * since we share that code
     */
    if (ORTE_SUCCESS != (ret = orte_wait_event(&orted_exit_event, &orte_exit, shutdown_callback))) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    
    /* setup our receive function - this will allow us to relay messages
     * during start for better scalability
     */
    ret = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DAEMON,
                                  ORTE_RML_NON_PERSISTENT, orte_daemon_recv, NULL);
    if (ret != ORTE_SUCCESS && ret != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    
    /* Set signal handlers to catch kill signals so we can properly clean up
     * after ourselves. 
     */
    opal_event_set(&term_handler, SIGTERM, OPAL_EV_SIGNAL,
                   shutdown_callback, NULL);
    opal_event_add(&term_handler, NULL);
    opal_event_set(&int_handler, SIGINT, OPAL_EV_SIGNAL,
                   shutdown_callback, NULL);
    opal_event_add(&int_handler, NULL);

#ifndef __WINDOWS__
    /** setup callbacks for signals we should ignore */
    opal_signal_set(&sigusr1_handler, SIGUSR1,
                    signal_callback, &sigusr1_handler);
    opal_signal_add(&sigusr1_handler, NULL);
    opal_signal_set(&sigusr2_handler, SIGUSR2,
                    signal_callback, &sigusr2_handler);
    opal_signal_add(&sigusr2_handler, NULL);
#endif  /* __WINDOWS__ */

    /* setup stdout/stderr */
    if (orte_debug_daemons_file_flag) {
        /* if we are debugging to a file, then send stdout/stderr to
         * the orted log file
         */

        /* get my jobid */
        if (ORTE_SUCCESS != (ret = orte_util_convert_jobid_to_string(&jobidstring,
                                        ORTE_PROC_MY_NAME->jobid))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }

        /* define a log file name in the session directory */
        sprintf(log_file, "output-orted-%s-%s.log",
                jobidstring, orte_system_info.nodename);
        log_path = opal_os_path(false,
                                orte_process_info.tmpdir_base,
                                orte_process_info.top_session_dir,
                                log_file,
                                NULL);

        fd = open(log_path, O_RDWR|O_CREAT|O_TRUNC, 0640);
        if (fd < 0) {
            /* couldn't open the file for some reason, so
             * just connect everything to /dev/null
             */
             fd = open("/dev/null", O_RDWR|O_CREAT|O_TRUNC, 0666);
        } else {
            dup2(fd, STDOUT_FILENO);
            dup2(fd, STDERR_FILENO);
            if(fd != STDOUT_FILENO && fd != STDERR_FILENO) {
               close(fd);
            }
        }
    }

    /* output a message indicating we are alive, our name, and our pid
     * for debugging purposes
     */
    if (orte_debug_daemons_flag) {
        fprintf(stderr, "Daemon %s checking in as pid %ld on host %s\n",
                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (long)orte_process_info.pid,
                orte_system_info.nodename);
    }

    /* We actually do *not* want the orted to voluntarily yield() the
       processor more than necessary.  The orted already blocks when
       it is doing nothing, so it doesn't use any more CPU cycles than
       it should; but when it *is* doing something, we do not want it
       to be unnecessarily delayed because it voluntarily yielded the
       processor in the middle of its work.

       For example: when a message arrives at the orted, we want the
       OS to wake up the orted in a timely fashion (which most OS's
       seem good about doing) and then we want the orted to process
       the message as fast as possible.  If the orted yields and lets
       aggressive MPI applications get the processor back, it may be a
       long time before the OS schedules the orted to run again
       (particularly if there is no IO event to wake it up).  Hence,
       routed OOB messages (for example) may be significantly delayed
       before being delivered to MPI processes, which can be
       problematic in some scenarios (e.g., COMM_SPAWN, BTL's that
       require OOB messages for wireup, etc.). */
    opal_progress_set_yield_when_idle(false);

    /* Change the default behavior of libevent such that we want to
       continually block rather than blocking for the default timeout
       and then looping around the progress engine again.  There
       should be nothing in the orted that cannot block in libevent
       until "something" happens (i.e., there's no need to keep
       cycling through progress because the only things that should
       happen will happen in libevent).  This is a minor optimization,
       but what the heck... :-) */
    opal_progress_set_event_flag(OPAL_EVLOOP_ONCE);

    /* if requested, obtain and report a new process name and my uri to the indicated pipe */
    if (orted_globals.uri_pipe > 0) {
        orte_job_t *jdata;
        orte_proc_t *proc;
        orte_node_t **nodes;
        orte_app_context_t *app;
        char *tmp, *nptr;
        int rc;

        /* setup the singleton's job */
        jdata = OBJ_NEW(orte_job_t);
        orte_plm_base_create_jobid(&jdata->jobid);
        opal_pointer_array_add(orte_job_data, jdata);
        
        /* setup an app_context for the singleton */
        app = OBJ_NEW(orte_app_context_t);
        app->app = strdup("singleton");
        app->num_procs = 1;
        opal_pointer_array_add(jdata->apps, app);
        
        /* run our local allocator to read the available
         * allocation in case this singleton decides to
         * comm_spawn other procs
         */
        if (ORTE_SUCCESS != (rc = orte_ras.allocate(jdata))) {
            ORTE_ERROR_LOG(rc);
            /* don't quit as this would cause the singleton
             * to hang!
             */
        }
        
        nodes = (orte_node_t**)orte_node_pool->addr;
        
        /* setup a proc object for the singleton - since we
         * -must- be the HNP, and therefore we stored our
         * node on the global node pool, and since the singleton
         * -must- be on the same node as us, indicate that
         */
        proc = OBJ_NEW(orte_proc_t);
        proc->name.jobid = jdata->jobid;
        proc->name.vpid = 0;
        proc->state = ORTE_PROC_STATE_RUNNING;
        proc->app_idx = 0;
        proc->node = nodes[0]; /* hnp node must be there */
        OBJ_RETAIN(nodes[0]);  /* keep accounting straight */
        opal_pointer_array_add(jdata->procs, proc);
        jdata->num_procs = 1;
        
        /* create a string that contains our uri + the singleton's name */
        orte_util_convert_process_name_to_string(&nptr, &proc->name);
        asprintf(&tmp, "%s[%s]", orte_process_info.my_daemon_uri, nptr);
        free(nptr);

        /* pass that info to the singleton */
        write(orted_globals.uri_pipe, tmp, strlen(tmp)+1); /* need to add 1 to get the NULL */

        /* cleanup */
        free(tmp);
    }

    /* if we were given a pipe to monitor for singleton termination, set that up */
    if (orted_globals.singleton_died_pipe > 0) {
        /* register shutdown handler */
        opal_event_set(&pipe_handler,
                       orted_globals.singleton_died_pipe,
                       OPAL_EV_READ|OPAL_EV_PERSIST,
                       shutdown_callback,
                       &orted_globals.singleton_died_pipe);
        opal_event_add(&pipe_handler, NULL);
    }

    /* if we are not the HNP...the only time we will be an HNP
     * is if we are launched by a singleton to provide support
     * for it
     */
    if (!orte_process_info.hnp) {
        /* send the information to the orted report-back point - this function
         * will process the data, but also counts the number of
         * orteds that reported back so the launch procedure can continue.
         * We need to do this at the last possible second as the HNP
         * can turn right around and begin issuing orders to us
         */
        buffer = OBJ_NEW(opal_buffer_t);
        rml_uri = orte_rml.get_contact_info();
        if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &rml_uri, 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(ret);
            OBJ_RELEASE(buffer);
            return ret;
        }
        if (0 > (ret = orte_rml.send_buffer(ORTE_PROC_MY_HNP, buffer,
                                            ORTE_RML_TAG_ORTED_CALLBACK, 0))) {
            ORTE_ERROR_LOG(ret);
            OBJ_RELEASE(buffer);
            return ret;
        }
        OBJ_RELEASE(buffer);  /* done with this */
    }

    if (orte_debug_daemons_flag) {
        opal_output(0, "%s orted: up and running - waiting for commands!", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
    }

    /* wait to hear we are done */
    opal_event_dispatch();

    /* should never get here, but if we do... */
    
    /* cleanup any lingering session directories */
    orte_session_dir_cleanup(ORTE_JOBID_WILDCARD);
    
    /* Finalize and clean up ourselves */
    if (ORTE_SUCCESS != (ret = orte_finalize())) {
        ORTE_ERROR_LOG(ret);
    }
    return ret;
}

static void shutdown_callback(int fd, short flags, void *arg)
{
    int ret;
    
    /* protect against multiple calls */
    if (!opal_atomic_trylock(&orted_exit_lock)) { /* returns 1 if already locked */
        return;
    }

    if (NULL != arg) {
        /* it's the singleton pipe...  remove that handler */
        opal_event_del(&pipe_handler);
    }
    
    if (orte_debug_daemons_flag) {
        opal_output(0, "%s orted: finalizing", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
    }
    
    /* cleanup */
    if (NULL != log_path) {
        unlink(log_path);
    }
    
    /* make sure our local procs are dead - but don't update their state
     * on the HNP as this may be redundant
     */
    orte_odls.kill_local_procs(ORTE_JOBID_WILDCARD, false);
    
   /* Finalize and clean up ourselves */
    if (ORTE_SUCCESS != (ret = orte_finalize())) {
        ORTE_ERROR_LOG(ret);
    }
    exit(ret);
}

static void signal_callback(int fd, short event, void *arg)
{
    /* just ignore these signals */
}
