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
#include "opal/util/cmd_line.h"
#include "opal/util/daemon_init.h"
#include "opal/util/opal_environ.h"
#include "opal/util/os_path.h"
#include "opal/util/output.h"
#include "opal/util/printf.h"
#include "opal/util/show_help.h"
#include "opal/util/trace.h"

#include "orte/dss/dss.h"
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

#include "orte/runtime/runtime.h"

#include "orte/tools/orted/orted.h"

extern char**environ;

orted_globals_t orted_globals;

static struct opal_event term_handler;
static struct opal_event int_handler;

static void signal_callback(int fd, short flags, void *arg);
static void job_state_callback(orte_gpr_notify_data_t *data, void *cbdata);
static void orte_daemon_recv(int status, orte_process_name_t* sender,
                 orte_buffer_t *buffer, orte_rml_tag_t tag,
                 void* cbdata);

/*
 * define the orted context table for obtaining parameters
 */
opal_cmd_line_init_t orte_cmd_line_opts[] = {
    /* Various "obvious" options */
    { NULL, NULL, NULL, 'h', NULL, "help", 0,
      &orted_globals.help, OPAL_CMD_LINE_TYPE_BOOL,
      "This help message" },

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

    /* setup to check common command line options that just report and die */
    memset(&orted_globals, 0, sizeof(orted_globals_t));
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

    /* Okay, now on to serious business
     * First, ensure the process info structure in instantiated and initialized
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

    /* setup the thread lock and condition variable */
    OBJ_CONSTRUCT(&orted_globals.mutex, opal_mutex_t);
    OBJ_CONSTRUCT(&orted_globals.condition, opal_condition_t);

    /* check to see if I'm a bootproxy */
    if (orted_globals.bootproxy) { /* perform bootproxy-specific things */
        if (orted_globals.mpi_call_yield > 0) {
            char *var;
            var = mca_base_param_environ_variable("mpi", NULL, "yield_when_idle");
            opal_setenv(var, "1", true, &environ);
        }

        /* setup callback on jobid */
        ret = orte_rmgr_base_proc_stage_gate_subscribe(orted_globals.bootproxy, job_state_callback, NULL, ORTE_PROC_STATE_TERMINATION);
        if(ORTE_SUCCESS != ret) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }

        if (ORTE_SUCCESS != (ret = orte_rmgr.launch(orted_globals.bootproxy))) {
            /* cleanup session directory */
            orte_session_dir_cleanup(orted_globals.bootproxy);
            /* Finalize the runtime - don't worry about error codes at this point */
            orte_finalize();
            exit(ret);
        }

        /* setup and enter the event monitor */
        OPAL_THREAD_LOCK(&orted_globals.mutex);
        while (false == orted_globals.exit_condition) {
            opal_condition_wait(&orted_globals.condition, &orted_globals.mutex);
        }
        OPAL_THREAD_UNLOCK(&orted_globals.mutex);

        /* cleanup session directory */
        orte_session_dir_cleanup(orted_globals.bootproxy);

        /* Finalize and clean up */
        if (ORTE_SUCCESS != (ret = orte_finalize())) {
            ORTE_ERROR_LOG(ret);
        }
        exit(ret);
    }

    /*
     *  Set my process status to "starting". Note that this must be done
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

    /* register the daemon main callback function */
    ret = orte_rml.recv_buffer_nb(ORTE_RML_NAME_ANY, ORTE_RML_TAG_DAEMON, 0, orte_daemon_recv, NULL);
    if (ret != ORTE_SUCCESS && ret != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(ret);
        return ret;
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
       opal_output(0, "[%lu,%lu,%lu] ompid: mutex cleared - finalizing", ORTE_NAME_ARGS(orte_process_info.my_name));
    }

    /* cleanup */
    if (NULL != log_path) {
        unlink(log_path);
    }

    /* finalize the system */
    orte_finalize();

    if (orted_globals.debug_daemons) {
       opal_output(0, "[%lu,%lu,%lu] ompid: done - exiting", ORTE_NAME_ARGS(orte_process_info.my_name));
    }

    exit(0);
}

static void signal_callback(int fd, short flags, void *arg)
{
    OPAL_TRACE(1);
    orted_globals.exit_condition = true;
    opal_condition_signal(&orted_globals.condition);
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
       opal_output(0, "[%lu,%lu,%lu] ompid: received message", ORTE_NAME_ARGS(orte_process_info.my_name));
    }

    answer = OBJ_NEW(orte_buffer_t);
    if (NULL == answer) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        goto DONE;
    }

    n = 1;
    if (ORTE_SUCCESS != (ret = orte_dss.unpack(buffer, &command, &n, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(ret);
        goto CLEANUP;
    }

    /****    EXIT COMMAND    ****/
    if (ORTE_DAEMON_EXIT_CMD == command) {
        orted_globals.exit_condition = true;
        opal_condition_signal(&orted_globals.condition);

        goto CLEANUP;

    /****     CONTACT QUERY COMMAND    ****/
    } else if (ORTE_DAEMON_CONTACT_QUERY_CMD == command) {
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

        goto CLEANUP;

    /****     HOSTFILE COMMAND    ****/
    } else if (ORTE_DAEMON_HOSTFILE_CMD == command) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_IMPLEMENTED);
        goto CLEANUP;

    /****     SCRIPTFILE COMMAND    ****/
    } else if (ORTE_DAEMON_SCRIPTFILE_CMD == command) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_IMPLEMENTED);
        goto CLEANUP;

    /****     HEARTBEAT COMMAND    ****/
    } else if (ORTE_DAEMON_HEARTBEAT_CMD == command) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_IMPLEMENTED);
        goto CLEANUP;
    }

 CLEANUP:
    OBJ_RELEASE(answer);

 DONE:
    OPAL_THREAD_UNLOCK(&orted_globals.mutex);

    /* reissue the non-blocking receive */
    ret = orte_rml.recv_buffer_nb(ORTE_RML_NAME_ANY, ORTE_RML_TAG_DAEMON, 0, orte_daemon_recv, NULL);
    if (ret != ORTE_SUCCESS && ret != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(ret);
    }

    return;
}

/* Function callback on jobid state changes.
 * This is closely modeled after orte_rmgr_proxy_callback in rmgr_proxy.c
 */
void job_state_callback(orte_gpr_notify_data_t *data, void *cbdata)
{
    orte_gpr_value_t **values, *value;
    orte_gpr_keyval_t** keyvals;
    orte_jobid_t jobid;
    orte_std_cntr_t i, j, k;
    int rc;

    OPAL_TRACE(1);

    /* we made sure in the subscriptions that at least one
     * value is always returned
     * get the jobid from the segment name in the first value
     */
    values = (orte_gpr_value_t**)(data->values)->addr;
    if (ORTE_SUCCESS != (rc =
            orte_schema.extract_jobid_from_segment_name(&jobid,
                        values[0]->segment))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    for (i = 0, k = 0; k < (data->cnt) && i < (data->values)->size; ++i) {

        if (NULL != values[i]) {
            k++;
            value = values[i];

            /* determine the state change */
            keyvals = value->keyvals;
            for (j = 0; j < value->cnt; ++j) {
                orte_gpr_keyval_t* keyval = keyvals[j];

                if(strcmp(keyval->key, ORTE_PROC_NUM_TERMINATED) == 0) {
                    OPAL_THREAD_LOCK(&orted_globals.mutex);

                    if (orted_globals.debug) {
                        opal_output(0, "orted: job_state_callback(jobid = %d, state = ORTE_PROC_STATE_TERMINATED)\n",
                                    jobid);
                    }

                    orted_globals.exit_condition = true;
                    opal_condition_signal(&orted_globals.condition);

                    OPAL_THREAD_UNLOCK(&orted_globals.mutex);
                    continue;
                }

                else if(strcmp(keyval->key, ORTE_PROC_NUM_ABORTED) == 0) {
                    OPAL_THREAD_LOCK(&orted_globals.mutex);

                    if (orted_globals.debug) {
                        opal_output(0, "orted: job_state_callback(jobid = %d, state = ORTE_PROC_STATE_ABORTED)\n",
                                    jobid);
                    }

                    orted_globals.exit_condition = true;
                    opal_condition_signal(&orted_globals.condition);

                    OPAL_THREAD_UNLOCK(&orted_globals.mutex);
                    continue;
                }
                else {
                    if (orted_globals.debug) {
                        opal_output(0, "orted: job_state_callback(jobid = %d, state = %d)\n",
                                    jobid, keyval->key);
                    }
                }
            }
        }
    }

    return;
}

