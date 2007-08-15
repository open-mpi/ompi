/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
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
#include <stdlib.h>
#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#include "orte/orte_constants.h"

#include "opal/event/event.h"
#include "opal/mca/base/base.h"
#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"
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

#include "orte/dss/dss.h"
#include "orte/class/orte_value_array.h"
#include "orte/util/sys_info.h"
#include "orte/util/proc_info.h"
#include "orte/util/univ_info.h"
#include "orte/util/session_dir.h"
#include "orte/util/universe_setup_file_io.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/ns/base/base.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/smr/smr.h"
#include "orte/mca/rmgr/rmgr.h"
#include "orte/mca/rmgr/base/base.h"
#include "orte/mca/odls/odls.h"
#include "orte/mca/pls/pls.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/params.h"

#include "orte/tools/orted/orted.h"

/*
 * Globals
 */
orted_globals_t orted_globals;

static struct opal_event term_handler;
static struct opal_event int_handler;

static void signal_callback(int fd, short flags, void *arg);
static void orte_daemon_recv(int status, orte_process_name_t* sender,
                             orte_buffer_t *buffer, orte_rml_tag_t tag,
                             void* cbdata);
static void orte_daemon_recv_pls(int status, orte_process_name_t* sender,
                                 orte_buffer_t *buffer, orte_rml_tag_t tag,
                                 void* cbdata);
static void orted_local_cb_launcher(orte_gpr_notify_data_t *data, void *user_tag);

/*
 * define the orted context table for obtaining parameters
 */
opal_cmd_line_init_t orte_cmd_line_opts[] = {
    /* Various "obvious" options */
    { NULL, NULL, NULL, 'h', NULL, "help", 0,
      &orted_globals.help, OPAL_CMD_LINE_TYPE_BOOL,
      "This help message" },

    { "orted", "spin", NULL, 'd', NULL, "spin", 0,
      &orted_globals.spin, OPAL_CMD_LINE_TYPE_BOOL,
      "Have the orted spin until we can connect a debugger to it" },

    { "orte", "debug", NULL, 'd', NULL, "debug", 0,
        &orted_globals.debug, OPAL_CMD_LINE_TYPE_BOOL,
        "Debug the OpenRTE" },
        
    { "orte", "no_daemonize", NULL, '\0', NULL, "no-daemonize", 0,
      &orted_globals.no_daemonize, OPAL_CMD_LINE_TYPE_BOOL,
      "Don't daemonize into the background" },

    { "orte", "debug", "daemons", '\0', NULL, "debug-daemons", 0,
      &orted_globals.debug_daemons, OPAL_CMD_LINE_TYPE_BOOL,
      "Enable debugging of OpenRTE daemons" },

    { "orte", "debug", "daemons_file", '\0', NULL, "debug-daemons-file", 0,
      &orted_globals.debug_daemons_file, OPAL_CMD_LINE_TYPE_BOOL,
      "Enable debugging of OpenRTE daemons, storing output in files" },

    { "rmgr", "bootproxy", "jobid", '\0', NULL, "bootproxy", 1,
      &orted_globals.bootproxy, OPAL_CMD_LINE_TYPE_INT,
      "Run as boot proxy for <job-id>" },

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

    { NULL, NULL, NULL, '\0', NULL, "mpi-call-yield", 1,
      &orted_globals.mpi_call_yield, OPAL_CMD_LINE_TYPE_INT,
      "Have MPI (or similar) applications call yield when idle" },

    /* End of list */
    { NULL, NULL, NULL, '\0', NULL, NULL, 0,
      NULL, OPAL_CMD_LINE_TYPE_NULL, NULL }
};

int main(int argc, char *argv[])
{
    int ret = 0;
    int fd;
    opal_cmd_line_t *cmd_line = NULL;
    char *log_path = NULL;
    char log_file[PATH_MAX];
    char *jobidstring;
    orte_gpr_value_t *value;
    char *segment;
    int i;
    orte_buffer_t answer;
    char *umask_str;

    /* Allow the PLS starters to pass us a umask to use, if required.
       Most starters by default can do something sane with the umask,
       but some (like TM) do not pass on the umask but instead inherit
       it form the root level process starter.  This has to happen
       before opal_init and everything else so that the couple of
       places that stash a umask end up with the correct value.  Only
       do it here (and not in orte_daemon) mainly to make it clear
       that this should only happen when starting an orted for the
       first time.  All startes I'm aware of that don't require an
       orted are smart enough to pass on a reasonable umask, so they
       wouldn't need this functionality anyway. */
    umask_str = getenv("ORTE_DAEMON_UMASK_VALUE");
    if (NULL != umask_str) {
        char *endptr;
        long mask = strtol(umask_str, &endptr, 8);
        if ((! (0 == mask && (EINVAL == errno || ERANGE == errno))) &&
            (*endptr == '\0')) {
            umask(mask);
        }
    }

    /* initialize the globals */
    memset(&orted_globals, 0, sizeof(orted_globals_t));

    /* Ensure that enough of OPAL is setup for us to be able to run */
    if (OPAL_SUCCESS != opal_init_util()) {
        fprintf(stderr, "OPAL failed to initialize -- orted aborting\n");
        exit(1);
    }

    /* save the environment for use when launching application processes */
    orted_globals.saved_environ = opal_argv_copy(environ);

    /* setup mca param system */
    mca_base_param_init();
    
    /* setup to check common command line options that just report and die */
    cmd_line = OBJ_NEW(opal_cmd_line_t);
    opal_cmd_line_create(cmd_line, orte_cmd_line_opts);
    if (ORTE_SUCCESS != (ret = opal_cmd_line_parse(cmd_line, false,
                                                   argc, argv))) {
        char *args = NULL;
        args = opal_cmd_line_get_usage_msg(cmd_line);
        opal_show_help("help-orted.txt", "orted:usage", false,
                       argv[0], args);
        free(args);
        return ret;
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

    /* see if we were directed to separate from current session */
    if (orted_globals.set_sid) {
        setsid();
    }
    
    /* see if they want us to spin until they can connect a debugger to us */
    i=0;
    while (orted_globals.spin) {
        i++;
        if (1000 < i) i=0;        
    }
    
    /* Okay, now on to serious business! */
    
    /* Ensure the process info structure in instantiated and initialized
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

    /* turn on debug if debug_file is requested so output will be generated */
    if (orted_globals.debug_daemons_file) {
        orted_globals.debug_daemons = true;
    }

    /* detach from controlling terminal
     * otherwise, remain attached so output can get to us
     */
    if(orted_globals.debug == false &&
       orted_globals.debug_daemons == false &&
       orted_globals.no_daemonize == false) {
        opal_daemon_init(NULL);
    }

    /* Intialize the Open RTE */
    /* Set the flag telling orte_init that I am NOT a
     * singleton, but am "infrastructure" - prevents setting
     * up incorrect infrastructure that only a singleton would
     * require
     */
    if (ORTE_SUCCESS != (ret = orte_init(true))) {
        opal_show_help("help-orted.txt", "orted:init-failure", false,
                       "orte_init()", ret);
        return ret;
    }

    /* Set signal handlers to catch kill signals so we can properly clean up
     * after ourselves. 
     */
    opal_event_set(&term_handler, SIGTERM, OPAL_EV_SIGNAL,
                   signal_callback, NULL);
    opal_event_add(&term_handler, NULL);
    opal_event_set(&int_handler, SIGINT, OPAL_EV_SIGNAL,
                   signal_callback, NULL);
    opal_event_add(&int_handler, NULL);

    /* if requested, report my uri to the indicated pipe */
    if (orted_globals.uri_pipe > 0) {
        write(orted_globals.uri_pipe, orte_universe_info.seed_uri,
                    strlen(orte_universe_info.seed_uri)+1); /* need to add 1 to get the NULL */
        close(orted_globals.uri_pipe);
    }

    /* setup stdout/stderr */
    if (orted_globals.debug_daemons_file) {
        /* if we are debugging to a file, then send stdout/stderr to
         * the orted log file
         */

        /* get my jobid */
        if (ORTE_SUCCESS != (ret = orte_ns.get_jobid_string(&jobidstring,
                                        orte_process_info.my_name))) {
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
    if (orted_globals.debug_daemons) {
        fprintf(stderr, "Daemon [%ld,%ld,%ld] checking in as pid %ld on host %s\n",
                ORTE_NAME_ARGS(orte_process_info.my_name), (long)orte_process_info.pid,
                orte_system_info.nodename);
    }

    /* setup the thread lock and condition variables */
    OBJ_CONSTRUCT(&orted_globals.mutex, opal_mutex_t);
    OBJ_CONSTRUCT(&orted_globals.condition, opal_condition_t);

    /* register the daemon main receive functions */
    ret = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_PLS_ORTED, ORTE_RML_NON_PERSISTENT, orte_daemon_recv_pls, NULL);
    if (ret != ORTE_SUCCESS && ret != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    ret = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DAEMON, ORTE_RML_NON_PERSISTENT, orte_daemon_recv, NULL);
    if (ret != ORTE_SUCCESS && ret != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /* check to see if I'm a bootproxy */
    if (orted_globals.bootproxy) { /* perform bootproxy-specific things */
        if (orted_globals.mpi_call_yield > 0) {
            char *var;
            var = mca_base_param_environ_variable("mpi", NULL, "yield_when_idle");
            opal_setenv(var, "1", true, &environ);
        }

        /* attach a subscription to the orted standard trigger so I can get
         * information on the processes I am to locally launch as soon as all
         * the orteds for this job are started.
         *
         * Once the registry gets to 2.0, we will be able to setup the
         * subscription so we only get our own launch info back. In the interim,
         * we setup the subscription so that ALL launch info for this job
         * is returned. We will then have to parse that message to get our
         * own local launch info.
         *
         * Since we have chosen this approach, we can take advantage of the
         * fact that the callback function will directly receive this data.
         * By setting up that callback function to actually perform the launch
         * based on the received data, all we have to do here is go into our
         * conditioned wait until the job completes!
         *
         * Sometimes, life can be good! :-)
         */

        /** put all this registry stuff in a compound command to limit communications */
        if (ORTE_SUCCESS != (ret = orte_gpr.begin_compound_cmd())) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }

        /* let the local launcher setup a subscription for its required data. We
         * pass the local_cb_launcher function so that this gets called back - this
         * allows us to wakeup the orted so it can exit cleanly if the callback
         * generates an error
         */
        if (ORTE_SUCCESS != (ret = orte_odls.subscribe_launch_data(orted_globals.bootproxy, orted_local_cb_launcher))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }

        /* get the job segment name */
        if (ORTE_SUCCESS != (ret = orte_schema.get_job_segment_name(&segment, orted_globals.bootproxy))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }

       /** increment the orted stage gate counter */
        if (ORTE_SUCCESS != (ret = orte_gpr.create_value(&value, ORTE_GPR_KEYS_OR|ORTE_GPR_TOKENS_AND,
                                                         segment, 1, 1))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
        free(segment); /* done with this now */

        value->tokens[0] = strdup(ORTE_JOB_GLOBALS);
        if (ORTE_SUCCESS != (ret = orte_gpr.create_keyval(&(value->keyvals[0]), ORTED_LAUNCH_STAGE_GATE_CNTR, ORTE_UNDEF, NULL))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }

        /* do the increment */
        if (ORTE_SUCCESS != (ret = orte_gpr.increment_value(value))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
        OBJ_RELEASE(value);  /* done with this now */

        /** send the compound command */
        if (ORTE_SUCCESS != (ret = orte_gpr.exec_compound_cmd())) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }

        /* setup and enter the event monitor to wait for a wakeup call */
        OPAL_THREAD_LOCK(&orted_globals.mutex);
        while (false == orted_globals.exit_condition) {
            opal_condition_wait(&orted_globals.condition, &orted_globals.mutex);
        }
        OPAL_THREAD_UNLOCK(&orted_globals.mutex);

        /* make sure our local procs are dead - but don't update their state
         * on the HNP as this may be redundant
         */
        orte_odls.kill_local_procs(ORTE_JOBID_WILDCARD, false);

        /* cleanup their session directory */
        orte_session_dir_cleanup(orted_globals.bootproxy);

        /* send an ack - we are as close to done as we can be while
         * still able to communicate
         */
        OBJ_CONSTRUCT(&answer, orte_buffer_t);
        if (0 > orte_rml.send_buffer(ORTE_PROC_MY_HNP, &answer, ORTE_RML_TAG_PLS_ORTED_ACK, 0)) {
            ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        }
        OBJ_DESTRUCT(&answer);


        /* Finalize and clean up ourselves */
        if (ORTE_SUCCESS != (ret = orte_finalize())) {
            ORTE_ERROR_LOG(ret);
        }
        exit(ret);
    }

    /*
     *  Set my process status to "running". Note that this must be done
     *  after the rte init is completed.
     */
    if (ORTE_SUCCESS != (ret = orte_smr.set_proc_state(orte_process_info.my_name,
                                                     ORTE_PROC_STATE_RUNNING, 0))) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    if (orted_globals.debug_daemons) {
        opal_output(0, "[%lu,%lu,%lu] ompid: issuing callback", ORTE_NAME_ARGS(orte_process_info.my_name));
    }

   /* go through the universe fields and see what else I need to do
     * - could be setup a virtual machine, spawn a console, etc.
     */

    if (orted_globals.debug_daemons) {
        opal_output(0, "[%lu,%lu,%lu] ompid: setting up event monitor", ORTE_NAME_ARGS(orte_process_info.my_name));
    }

     /* setup and enter the event monitor */
    OPAL_THREAD_LOCK(&orted_globals.mutex);

    while (false == orted_globals.exit_condition) {
        opal_condition_wait(&orted_globals.condition, &orted_globals.mutex);
    }

    OPAL_THREAD_UNLOCK(&orted_globals.mutex);

    if (orted_globals.debug_daemons) {
       opal_output(0, "[%lu,%lu,%lu] orted: mutex cleared - finalizing", ORTE_NAME_ARGS(orte_process_info.my_name));
    }

    /* cleanup */
    if (NULL != log_path) {
        unlink(log_path);
    }

    /* finalize the system */
    orte_finalize();

    if (orted_globals.debug_daemons) {
       opal_output(0, "[%lu,%lu,%lu] orted: done - exiting", ORTE_NAME_ARGS(orte_process_info.my_name));
    }

    exit(0);
}

/* this function receives the trigger callback from the orted launch stage gate
 * and passes it to the orted local launcher for processing. We do this intermediate
 * step so that we can get an error code if anything went wrong and, if so, wakeup the
 * orted so we can gracefully die
 */
static void orted_local_cb_launcher(orte_gpr_notify_data_t *data, void *user_tag)
{
    int rc;
    
    if (orted_globals.debug_daemons) {
        opal_output(0, "[%lu,%lu,%lu] orted: received launch callback", ORTE_NAME_ARGS(orte_process_info.my_name));
    }
    
    /* pass the data to the orted_local_launcher and get a report on
     * success or failure of the launch
     */
    if (ORTE_SUCCESS != (rc = orte_odls.launch_local_procs(data, orted_globals.saved_environ))) {
        /* if there was an error, report it.
         * NOTE: it is absolutely imperative that we do not cause the orted to EXIT when
         * this happens!!! If we do, then the HNP will "hang" as the orted will no longer
         * be around to receive messages telling it what to do in response to the failure
         */
        ORTE_ERROR_LOG(rc);
    }
    
    /* all done - return and let the orted sleep until something happens */
    return;
}


static void signal_callback(int fd, short flags, void *arg)
{
    OPAL_TRACE(1);
    orted_globals.exit_condition = true;
    opal_condition_signal(&orted_globals.condition);
}

static void orte_daemon_recv_pls(int status, orte_process_name_t* sender,
                 orte_buffer_t *buffer, orte_rml_tag_t tag,
                 void* cbdata)
{
    orte_daemon_cmd_flag_t command;
    orte_buffer_t answer;
    int ret;
    orte_std_cntr_t n;
    int32_t signal;
    orte_gpr_notify_data_t *ndat;
    orte_jobid_t job;

    OPAL_TRACE(1);

    OPAL_THREAD_LOCK(&orted_globals.mutex);

    if (orted_globals.debug_daemons) {
       opal_output(0, "[%lu,%lu,%lu] orted_recv_pls: received message from [%ld,%ld,%ld]",
                   ORTE_NAME_ARGS(orte_process_info.my_name),
                   ORTE_NAME_ARGS(sender));
    }

    /* unpack the command */
    n = 1;
    if (ORTE_SUCCESS != (ret = orte_dss.unpack(buffer, &command, &n, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(ret);
        goto CLEANUP;
    }
    
    switch(command) {

        /****    KILL_LOCAL_PROCS   ****/
        case ORTE_DAEMON_KILL_LOCAL_PROCS:
            if (orted_globals.debug_daemons) {
                opal_output(0, "[%lu,%lu,%lu] orted_recv_pls: received kill_local_procs",
                            ORTE_NAME_ARGS(orte_process_info.my_name));
            }
            /* unpack the jobid - could be JOBID_WILDCARD, which would indicatge
             * we should kill all local procs. Otherwise, only kill those within
             * the specified jobid
             */
            n = 1;
            if (ORTE_SUCCESS != (ret = orte_dss.unpack(buffer, &job, &n, ORTE_JOBID))) {
                ORTE_ERROR_LOG(ret);
                goto CLEANUP;
            }

            if (ORTE_SUCCESS != (ret = orte_odls.kill_local_procs(job, true))) {
                ORTE_ERROR_LOG(ret);
            }
            break;
            
        /****    SIGNAL_LOCAL_PROCS   ****/
        case ORTE_DAEMON_SIGNAL_LOCAL_PROCS:
            if (orted_globals.debug_daemons) {
                opal_output(0, "[%lu,%lu,%lu] orted_recv_pls: received signal_local_procs",
                            ORTE_NAME_ARGS(orte_process_info.my_name));
            }
            /* get the signal */
            n = 1;
            if (ORTE_SUCCESS != (ret = orte_dss.unpack(buffer, &signal, &n, ORTE_INT32))) {
                ORTE_ERROR_LOG(ret);
                goto CLEANUP;
            }
                
            /* see if they specified a process to signal, or if we
             * should just signal them all
             *
             * NOTE: FOR NOW, WE JUST SIGNAL ALL CHILDREN
             */

            if (ORTE_SUCCESS != (ret = orte_odls.signal_local_procs(NULL, signal))) {
                ORTE_ERROR_LOG(ret);
            }
            break;

            /****    ADD_LOCAL_PROCS   ****/
        case ORTE_DAEMON_ADD_LOCAL_PROCS:
            if (orted_globals.debug_daemons) {
                opal_output(0, "[%lu,%lu,%lu] orted_recv_pls: received add_local_procs",
                            ORTE_NAME_ARGS(orte_process_info.my_name));
            }
            /* unpack the notify data object */
            n = 1;
            if (ORTE_SUCCESS != (ret = orte_dss.unpack(buffer, &ndat, &n, ORTE_GPR_NOTIFY_DATA))) {
                ORTE_ERROR_LOG(ret);
                goto CLEANUP;
            }
            
            /* launch the processes */
            if (ORTE_SUCCESS != (ret = orte_odls.launch_local_procs(ndat, orted_globals.saved_environ))) {
                ORTE_ERROR_LOG(ret);
            }

            /* cleanup the memory */
            OBJ_RELEASE(ndat);
            break;
           
            /****    EXIT COMMAND    ****/
        case ORTE_DAEMON_EXIT_CMD:
            if (orted_globals.debug_daemons) {
                opal_output(0, "[%lu,%lu,%lu] orted_recv_pls: received exit",
                            ORTE_NAME_ARGS(orte_process_info.my_name));
            }
            /* no response to send here - we'll send it when nearly exit'd */
            orted_globals.exit_condition = true;
            opal_condition_signal(&orted_globals.condition);
            OPAL_THREAD_UNLOCK(&orted_globals.mutex);
            return;
            break;

        default:
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
            break;
    }

 CLEANUP:
    /* send an ack that command is done */
    OBJ_CONSTRUCT(&answer, orte_buffer_t);
    if (0 > orte_rml.send_buffer(sender, &answer, ORTE_RML_TAG_PLS_ORTED_ACK, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
    }
    OBJ_DESTRUCT(&answer);
    
    OPAL_THREAD_UNLOCK(&orted_globals.mutex);

    /* reissue the non-blocking receive */
    ret = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_PLS_ORTED, ORTE_RML_NON_PERSISTENT, orte_daemon_recv_pls, NULL);
    if (ret != ORTE_SUCCESS && ret != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(ret);
    }

    return;
}

static void exit_callback(int fd, short event, void *arg)
{
    /* Trigger the normal exit conditions */
    orted_globals.exit_condition = true;
    opal_condition_signal(&orted_globals.condition);
    OPAL_THREAD_UNLOCK(&orted_globals.mutex);
}

static void halt_vm(void)
{
    int ret;
    struct timeval tv = { 1, 0 };
    opal_event_t* event;
    opal_list_t attrs;
    opal_list_item_t *item;
    
    /* terminate the vm - this will also wake us up so we can exit */
    OBJ_CONSTRUCT(&attrs, opal_list_t);
    orte_rmgr.add_attribute(&attrs, ORTE_NS_INCLUDE_DESCENDANTS, ORTE_UNDEF, NULL, ORTE_RMGR_ATTR_OVERRIDE);
    ret = orte_pls.terminate_orteds(0, &orte_abort_timeout, &attrs);
    while (NULL != (item = opal_list_remove_first(&attrs))) OBJ_RELEASE(item);
    OBJ_DESTRUCT(&attrs);
    
    /* setup a delay to give the orteds time to complete their departure */
    if (NULL != (event = (opal_event_t*)malloc(sizeof(opal_event_t)))) {
        opal_evtimer_set(event, exit_callback, NULL);
        opal_evtimer_add(event, &tv);
    }
}

static void orte_daemon_recv(int status, orte_process_name_t* sender,
                             orte_buffer_t *buffer, orte_rml_tag_t tag,
                             void* cbdata)
{
    orte_buffer_t *answer;
    orte_daemon_cmd_flag_t command;
    int ret;
    orte_std_cntr_t n;
    char *contact_info;
    
    OPAL_TRACE(1);
    
    OPAL_THREAD_LOCK(&orted_globals.mutex);
    
    if (orted_globals.debug_daemons) {
        opal_output(0, "[%lu,%lu,%lu] orted_recv: received message from [%ld,%ld,%ld]",
                    ORTE_NAME_ARGS(orte_process_info.my_name),
                    ORTE_NAME_ARGS(sender));
    }
    
    n = 1;
    if (ORTE_SUCCESS != (ret = orte_dss.unpack(buffer, &command, &n, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(ret);
        OPAL_THREAD_UNLOCK(&orted_globals.mutex);
        return;
    }
    
    answer = OBJ_NEW(orte_buffer_t);
    if (NULL == answer) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        goto DONE;
    }
    
    switch(command) {
        /****    EXIT COMMAND    ****/
        case ORTE_DAEMON_EXIT_CMD:
            if (orted_globals.debug_daemons) {
                opal_output(0, "[%lu,%lu,%lu] orted_recv: received exit",
                            ORTE_NAME_ARGS(orte_process_info.my_name));
            }
            
            orted_globals.exit_condition = true;
            opal_condition_signal(&orted_globals.condition);
            break;

        /****    HALT VM COMMAND    ****/
        case ORTE_DAEMON_HALT_VM_CMD:
            if (orted_globals.debug_daemons) {
                opal_output(0, "[%lu,%lu,%lu] orted_recv: received halt vm",
                            ORTE_NAME_ARGS(orte_process_info.my_name));
            }
            halt_vm();
            break;
            
        /****     CONTACT QUERY COMMAND    ****/
        case ORTE_DAEMON_CONTACT_QUERY_CMD:
            /* send back contact info */
            contact_info = orte_rml.get_uri();
            
            if (NULL == contact_info) {
                ORTE_ERROR_LOG(ORTE_ERROR);
                goto CLEANUP;
            }
            
            if (ORTE_SUCCESS != (ret = orte_dss.pack(answer, &contact_info, 1, ORTE_STRING))) {
                ORTE_ERROR_LOG(ret);
                goto CLEANUP;
            }
            
            if (0 > orte_rml.send_buffer(sender, answer, tag, 0)) {
                ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
            }
            break;
        
        /****     HOSTFILE COMMAND    ****/
        case ORTE_DAEMON_HOSTFILE_CMD:
            ORTE_ERROR_LOG(ORTE_ERR_NOT_IMPLEMENTED);
            break;
        
        /****     SCRIPTFILE COMMAND    ****/
        case ORTE_DAEMON_SCRIPTFILE_CMD:
            ORTE_ERROR_LOG(ORTE_ERR_NOT_IMPLEMENTED);
            break;
        
        /****     HEARTBEAT COMMAND    ****/
        case ORTE_DAEMON_HEARTBEAT_CMD:
            ORTE_ERROR_LOG(ORTE_ERR_NOT_IMPLEMENTED);
            break;
            
        default:
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
    }
    
CLEANUP:
    OBJ_RELEASE(answer);
    
DONE:
    OPAL_THREAD_UNLOCK(&orted_globals.mutex);
    
    /* reissue the non-blocking receive */
    ret = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DAEMON, ORTE_RML_NON_PERSISTENT, orte_daemon_recv, NULL);
    if (ret != ORTE_SUCCESS && ret != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(ret);
    }
    
    return;
}
