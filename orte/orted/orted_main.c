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

#include "orte/orte_constants.h"

#include "opal/event/event.h"
#include "opal/mca/base/base.h"
#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"
#include "opal/util/bit_ops.h"
#include "opal/util/cmd_line.h"
#include "opal/util/daemon_init.h"
#include "opal/util/opal_environ.h"
#include "opal/util/os_path.h"
#include "opal/util/output.h"
#include "opal/util/printf.h"
#include "opal/util/show_help.h"
#include "opal/util/trace.h"
#include "opal/util/argv.h"
#include "opal/runtime/opal.h"
#include "opal/mca/base/mca_base_param.h"


#include "orte/dss/dss.h"
#include "orte/class/orte_value_array.h"
#include "orte/util/sys_info.h"
#include "orte/util/proc_info.h"
#include "orte/util/univ_info.h"
#include "orte/util/session_dir.h"
#include "orte/util/universe_setup_file_io.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/ras/ras.h"
#include "orte/mca/rds/rds.h"
#include "orte/mca/rmaps/rmaps.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/smr/smr.h"
#include "orte/mca/rmgr/rmgr.h"
#include "orte/mca/rmgr/base/rmgr_private.h"
#include "orte/mca/odls/odls.h"
#include "orte/mca/pls/pls.h"


#include "orte/runtime/runtime.h"
#include "orte/runtime/params.h"

#include "orte/orted/orted.h"

/*
 * Globals
 */

static struct opal_event term_handler;
static struct opal_event int_handler;
static struct opal_event pipe_handler;

static void shutdown_callback(int fd, short flags, void *arg);

static struct {
    bool help;
    bool set_sid;
    char* ns_nds;
    char* name;
    char* vpid_start;
    char* num_procs;
    char* universe;
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
        
    { "orte", "no_daemonize", NULL, '\0', NULL, "no-daemonize", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Don't daemonize into the background" },

    { "orte", "debug", "daemons", '\0', NULL, "debug-daemons", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Enable debugging of OpenRTE daemons" },

    { "orte", "debug", "daemons_file", '\0', NULL, "debug-daemons-file", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Enable debugging of OpenRTE daemons, storing output in files" },

    { NULL, NULL, NULL, '\0', NULL, "set-sid", 0,
      &orted_globals.set_sid, OPAL_CMD_LINE_TYPE_BOOL,
      "Direct the orted to separate from the current session"},
    
    { NULL, NULL, NULL, '\0', NULL, "name", 1,
      &orted_globals.name, OPAL_CMD_LINE_TYPE_STRING,
      "Set the orte process name"},

    { NULL, NULL, NULL, '\0', NULL, "vpid_start", 1,
      &orted_globals.vpid_start, OPAL_CMD_LINE_TYPE_STRING,
      "Set the starting vpid for this job"},

    { NULL, NULL, NULL, '\0', NULL, "num_procs", 1,
      &orted_globals.num_procs, OPAL_CMD_LINE_TYPE_STRING,
      "Set the number of process in this job"},

    { NULL, NULL, NULL, '\0', NULL, "ns-nds", 1,
      &orted_globals.ns_nds, OPAL_CMD_LINE_TYPE_STRING,
      "set sds/nds component to use for daemon (normally not needed)"},

    { NULL, NULL, NULL, '\0', NULL, "nsreplica", 1,
      &orte_process_info.ns_replica_uri, OPAL_CMD_LINE_TYPE_STRING,
      "Name service contact information."},

    { NULL, NULL, NULL, '\0', NULL, "gprreplica", 1,
      &orte_process_info.gpr_replica_uri, OPAL_CMD_LINE_TYPE_STRING,
      "Registry contact information."},

    { NULL, NULL, NULL, '\0', NULL, "nodename", 1,
      &orte_system_info.nodename, OPAL_CMD_LINE_TYPE_STRING,
      "Node name as specified by host/resource description." },

    { "universe", NULL, NULL, '\0', NULL, "universe", 1,
      &orted_globals.universe, OPAL_CMD_LINE_TYPE_STRING,
      "Set the universe name as username@hostname:universe_name for this application" },

    { "tmpdir", "base", NULL, '\0', NULL, "tmpdir", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Set the root for the session directory tree" },

    { "seed", NULL, NULL, '\0', NULL, "seed", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Host replicas for the core universe services"},

    { "universe", "persistence", NULL, '\0', NULL, "persistent", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Remain alive after the application process completes"},

    { "universe", "scope", NULL, '\0', NULL, "scope", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Set restrictions on who can connect to this universe"},

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
    char *log_path = NULL;
    char log_file[PATH_MAX];
    char *jobidstring;
    int i;
    orte_buffer_t *buffer;
    int zero = 0;
    char hostname[100];

    /* initialize the globals */
    memset(&orted_globals, 0, sizeof(orted_globals));
    /* initialize the singleton died pipe to an illegal value so we can detect it was set */
    orted_globals.singleton_died_pipe = -1;
 
    /* save the environment for use when launching application processes */
    orte_launch_environ = opal_argv_copy(environ);

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

    /* register and process the orte params */
    if (ORTE_SUCCESS != (ret = orte_register_params(ORTE_INFRASTRUCTURE))) {
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
#if !defined(__WINDOWS__)
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
    
    /* Okay, now on to serious business! */
    
    /* Ensure the process info structure is instantiated and initialized
     * and set the daemon flag to true
     */
    orte_process_info.daemon = true;

    /*
     * If the daemon was given a name on the command line, need to set the
     * proper indicators in the environment so the name discovery service
     * can find it
     */
    if (orted_globals.name) {
        if (ORTE_SUCCESS != (ret = opal_setenv("OMPI_MCA_ns_nds",
                                              "env", true, &environ))) {
            opal_show_help("help-orted.txt", "orted:environ", false,
                           "OMPI_MCA_ns_nds", "env", ret);
            return ret;
        }
        if (ORTE_SUCCESS != (ret = opal_setenv("OMPI_MCA_ns_nds_name",
                                  orted_globals.name, true, &environ))) {
            opal_show_help("help-orted.txt", "orted:environ", false,
                           "OMPI_MCA_ns_nds_name", orted_globals.name, ret);
            return ret;
        }
        /* the following values are meaningless to the daemon, but may have
         * been passed in anyway. we set them here because the nds_env component
         * requires that they be set
         */
        if (ORTE_SUCCESS != (ret = opal_setenv("OMPI_MCA_ns_nds_vpid_start",
                                  orted_globals.vpid_start, true, &environ))) {
            opal_show_help("help-orted.txt", "orted:environ", false,
                           "OMPI_MCA_ns_nds_vpid_start", orted_globals.vpid_start, ret);
            return ret;
        }
        if (ORTE_SUCCESS != (ret = opal_setenv("OMPI_MCA_ns_nds_num_procs",
                                  orted_globals.num_procs, true, &environ))) {
            opal_show_help("help-orted.txt", "orted:environ", false,
                           "OMPI_MCA_ns_nds_num_procs", orted_globals.num_procs, ret);
            return ret;
        }
    }
    if (orted_globals.ns_nds) {
        if (ORTE_SUCCESS != (ret = opal_setenv("OMPI_MCA_ns_nds",
                                               orted_globals.ns_nds, true, &environ))) {
            opal_show_help("help-orted.txt", "orted:environ", false,
                           "OMPI_MCA_ns_nds", "env", ret);
            return ret;
        }
    }

    /* detach from controlling terminal
     * otherwise, remain attached so output can get to us
     */
    if(orte_debug_flag == false &&
       orte_debug_daemons_flag == false &&
       orte_no_daemonize_flag == false) {
        opal_daemon_init(NULL);
    }

    /* Intialize OPAL */
    if (ORTE_SUCCESS != (ret = opal_init())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /* setup the thread lock and condition variables */
    OBJ_CONSTRUCT(&orted_comm_mutex, opal_mutex_t);
    OBJ_CONSTRUCT(&orted_comm_cond, opal_condition_t);
    orted_comm_exit_cond = false;
    orte_orterun = false;
    
    /* Set the flag telling OpenRTE that I am NOT a
     * singleton, but am "infrastructure" - prevents setting
     * up incorrect infrastructure that only a singleton would
     * require.
     */
    if (ORTE_SUCCESS != (ret = orte_init_stage1(ORTE_INFRASTRUCTURE))) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    
    /* setup our receive functions - this will allow us to relay messages
     * during start for better scalability
     */
    /* register the daemon main receive functions */
    /* setup to listen for broadcast commands via routed messaging algorithms */
    ret = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_ORTED_ROUTED,
                                  ORTE_RML_NON_PERSISTENT, orte_daemon_recv_routed, NULL);
    if (ret != ORTE_SUCCESS && ret != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    /* setup to listen for commands sent specifically to me */
    ret = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DAEMON, ORTE_RML_NON_PERSISTENT, orte_daemon_recv, NULL);
    if (ret != ORTE_SUCCESS && ret != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    
    /* if we are not a seed, prep a return buffer to say we started okay */
    buffer = OBJ_NEW(orte_buffer_t);
    if (!orte_process_info.seed) {
        if (ORTE_SUCCESS != (ret = orte_dss.pack(buffer, &zero, 1, ORTE_INT))) {
            ORTE_ERROR_LOG(ret);
            OBJ_RELEASE(buffer);
            return ret;
        }
        if (ORTE_SUCCESS != (ret = orte_dss.pack(buffer, ORTE_PROC_MY_NAME, 1, ORTE_NAME))) {
            ORTE_ERROR_LOG(ret);
            OBJ_RELEASE(buffer);
            return ret;
        }
        
        /* Begin recording registry actions */
        if (ORTE_SUCCESS != (ret = orte_gpr.begin_compound_cmd(buffer))) {
            ORTE_ERROR_LOG(ret);
            OBJ_RELEASE(buffer);
            return ret;
        }
    }
    
    /* tell orte_init that we don't want any subscriptions registered by passing
     * a NULL trigger name
     */
    if (ORTE_SUCCESS != (ret = orte_init_stage2(NULL))) {
        ORTE_ERROR_LOG(ret);
        OBJ_RELEASE(buffer);
        return ret;
    }

    /* if we aren't a seed, then we need to stop the compound_cmd mode here so
     * that other subsystems can use it
     */
    if (!orte_process_info.seed) {
        if (ORTE_SUCCESS != (ret = orte_gpr.stop_compound_cmd())) {
            ORTE_ERROR_LOG(ret);
            OBJ_RELEASE(buffer);
            return ret;
        }
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

    /* if requested, report my uri to the indicated pipe */
    if (orted_globals.uri_pipe > 0) {
        write(orted_globals.uri_pipe, orte_universe_info.seed_uri,
                    strlen(orte_universe_info.seed_uri)+1); /* need to add 1 to get the NULL */
        close(orted_globals.uri_pipe);
    }

    /* setup stdout/stderr */
    if (orte_debug_daemons_file_flag) {
        /* if we are debugging to a file, then send stdout/stderr to
         * the orted log file
         */

        /* get my jobid */
        if (ORTE_SUCCESS != (ret = orte_ns.get_jobid_string(&jobidstring,
                                        orte_process_info.my_name))) {
            ORTE_ERROR_LOG(ret);
            OBJ_RELEASE(buffer);
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
                ORTE_NAME_PRINT(orte_process_info.my_name), (long)orte_process_info.pid,
                orte_system_info.nodename);
    }

    /* a daemon should *always* yield the processor when idle */
    opal_progress_set_yield_when_idle(true);

    /* setup to listen for xcast stage gate commands. We need to do this because updates to the
     * contact info for dynamically spawned daemons will come to the gate RML-tag
     */
    ret = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_XCAST_BARRIER,
                                  ORTE_RML_NON_PERSISTENT, orte_daemon_recv_gate, NULL);
    if (ret != ORTE_SUCCESS && ret != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(ret);
        OBJ_RELEASE(buffer);
        return ret;
    }

    /* if requested, report my uri to the indicated pipe */
    if (orted_globals.uri_pipe > 0) {
        write(orted_globals.uri_pipe, orte_universe_info.seed_uri,
              strlen(orte_universe_info.seed_uri)+1); /* need to add 1 to get the NULL */
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

    /* setup and enter the event monitor */
    OPAL_THREAD_LOCK(&orted_comm_mutex);

    /* if we are not a seed... */
    if (!orte_process_info.seed) {
        /* send the information to the orted report-back point - this function
         * will kindly hand the gpr compound cmds contained in the buffer
         * over to the gpr for processing, but also counts the number of
         * orteds that reported back so the launch procedure can continue.
         * We need to do this at the last possible second as the HNP
         * can turn right around and begin issuing orders to us
         */
        if (0 > (ret = orte_rml.send_buffer(ORTE_PROC_MY_HNP, buffer,
                                            ORTE_RML_TAG_ORTED_CALLBACK, 0))) {
            ORTE_ERROR_LOG(ret);
            OBJ_RELEASE(buffer);
            return ret;
        }
    }
    OBJ_RELEASE(buffer);  /* done with this */

    if (orte_debug_daemons_flag) {
        opal_output(0, "%s orted: up and running - waiting for commands!", ORTE_NAME_PRINT(orte_process_info.my_name));
    }

    while (false == orted_comm_exit_cond) {
        opal_condition_wait(&orted_comm_cond, &orted_comm_mutex);
    }

    OPAL_THREAD_UNLOCK(&orted_comm_mutex);

    if (orte_debug_daemons_flag) {
       opal_output(0, "%s orted: mutex cleared - finalizing", ORTE_NAME_PRINT(orte_process_info.my_name));
    }

    /* cleanup */
    if (NULL != log_path) {
        unlink(log_path);
    }

    /* make sure our local procs are dead - but don't update their state
    * on the HNP as this may be redundant
    */
    orte_odls.kill_local_procs(ORTE_JOBID_WILDCARD, false);

    /* cleanup the orted communication mutex and condition objects */
    OBJ_DESTRUCT(&orted_comm_mutex);
    OBJ_DESTRUCT(&orted_comm_cond);

    /* cleanup any lingering session directories */
    orte_session_dir_cleanup(ORTE_JOBID_WILDCARD);

    /* Finalize and clean up ourselves */
    if (ORTE_SUCCESS != (ret = orte_finalize())) {
        ORTE_ERROR_LOG(ret);
    }
    exit(ret);
}

static void shutdown_callback(int fd, short flags, void *arg)
{
    OPAL_TRACE(1);
    if (NULL != arg) {
        /* it's the pipe...  remove that handler */
        opal_event_del(&pipe_handler);
    }
    orted_comm_exit_cond = true;
    opal_condition_signal(&orted_comm_cond);
}

