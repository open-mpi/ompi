/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
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
 *
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <sys/types.h>
#include <stdio.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/class/opal_list.h"
#include "opal/mca/event/event.h"
#include "opal/runtime/opal.h"
#include "opal/runtime/opal_cr.h"

#include "opal/util/if.h"
#include "opal/util/os_path.h"
#include "opal/util/output.h"
#include "opal/util/malloc.h"
#include "opal/util/basename.h"
#include "opal/mca/pstat/base/base.h"
#include "opal/mca/paffinity/base/base.h"
#include "opal/mca/sysinfo/base/base.h"

#include "orte/mca/rml/base/base.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/routed/base/base.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/grpcomm/base/base.h"
#include "orte/mca/iof/base/base.h"
#include "orte/mca/ras/base/base.h"
#include "orte/mca/plm/base/base.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/odls/base/base.h"
#include "orte/mca/notifier/base/base.h"
#include "orte/mca/rmcast/base/base.h"
#include "orte/mca/db/base/base.h"
#include "orte/mca/sensor/base/base.h"
#include "orte/mca/sensor/sensor.h"
#include "orte/mca/debugger/base/base.h"
#include "orte/mca/debugger/debugger.h"
#include "orte/mca/rmaps/base/base.h"
#if OPAL_ENABLE_FT_CR == 1
#include "orte/mca/snapc/base/base.h"
#endif
#include "orte/mca/filem/base/base.h"

#include "orte/util/show_help.h"
#include "orte/util/proc_info.h"
#include "orte/util/session_dir.h"
#include "orte/util/hnp_contact.h"
#include "orte/util/name_fns.h"
#include "orte/util/show_help.h"
#include "orte/util/comm/comm.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_quit.h"
#include "orte/runtime/orte_cr.h"
#include "orte/runtime/orte_locks.h"
#include "orte/runtime/orte_data_server.h"

#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/base/base.h"
#include "orte/mca/ess/hnp/ess_hnp.h"

static int rte_init(void);
static int rte_finalize(void);
static void rte_abort(int status, bool report) __opal_attribute_noreturn__;
static uint8_t proc_get_locality(orte_process_name_t *proc);
static orte_vpid_t proc_get_daemon(orte_process_name_t *proc);
static char* proc_get_hostname(orte_process_name_t *proc);
static orte_local_rank_t proc_get_local_rank(orte_process_name_t *proc);
static orte_node_rank_t proc_get_node_rank(orte_process_name_t *proc);
static int update_pidmap(opal_byte_object_t *bo);
static int update_nidmap(opal_byte_object_t *bo);

orte_ess_base_module_t orte_ess_hnp_module = {
    rte_init,
    rte_finalize,
    rte_abort,
    proc_get_locality,
    proc_get_daemon,
    proc_get_hostname,
    proc_get_local_rank,
    proc_get_node_rank,
    update_pidmap,
    update_nidmap,
    orte_ess_base_query_sys_info,
    NULL /* ft_event */
};

/* local globals */
static bool signals_set=false;
static opal_event_t term_handler;
static opal_event_t int_handler;
static opal_event_t epipe_handler;
#ifndef __WINDOWS__
static opal_event_t sigusr1_handler;
static opal_event_t sigusr2_handler;
static opal_event_t sigtstp_handler;
static opal_event_t sigcont_handler;
#endif  /* __WINDOWS__ */

static void abort_signal_callback(int fd, short flags, void *arg);
static void abort_exit_callback(int fd, short event, void *arg);
static void epipe_signal_callback(int fd, short flags, void *arg);
static void signal_forward_callback(int fd, short event, void *arg);

static int rte_init(void)
{
    int ret;
    char *error = NULL;
    char *contact_path, *jobfam_dir;
    orte_job_t *jdata;
    orte_node_t *node;
    orte_proc_t *proc;
    int value;

    /* run the prolog */
    if (ORTE_SUCCESS != (ret = orte_ess_base_std_prolog())) {
        error = "orte_ess_base_std_prolog";
        goto error;
    }
    
#ifndef __WINDOWS__
    /* setup callback for SIGPIPE */
    opal_event_signal_set(opal_event_base, &epipe_handler, SIGPIPE,
                          epipe_signal_callback, &epipe_handler);
    opal_event_signal_add(&epipe_handler, NULL);
    /** setup callbacks for abort signals - from this point
     * forward, we need to abort in a manner that allows us
     * to cleanup
     */
    opal_event_signal_set(opal_event_base, &term_handler, SIGTERM,
                          abort_signal_callback, &term_handler);
    opal_event_signal_add(&term_handler, NULL);
    opal_event_signal_set(opal_event_base, &int_handler, SIGINT,
                          abort_signal_callback, &int_handler);
    opal_event_signal_add(&int_handler, NULL);

    /** setup callbacks for signals we should foward */
    opal_event_signal_set(opal_event_base, &sigusr1_handler, SIGUSR1,
                          signal_forward_callback, &sigusr1_handler);
    opal_event_signal_add(&sigusr1_handler, NULL);
    opal_event_signal_set(opal_event_base, &sigusr2_handler, SIGUSR2,
                          signal_forward_callback, &sigusr2_handler);
    opal_event_signal_add(&sigusr2_handler, NULL);
    if (orte_forward_job_control) {
        opal_event_signal_set(opal_event_base, &sigtstp_handler, SIGTSTP,
                              signal_forward_callback, &sigtstp_handler);
        opal_event_signal_add(&sigtstp_handler, NULL);
        opal_event_signal_set(opal_event_base, &sigcont_handler, SIGCONT,
                              signal_forward_callback, &sigcont_handler);
        opal_event_signal_add(&sigcont_handler, NULL);
    }
#endif  /* __WINDOWS__ */
    
    signals_set = true;
    
    /* determine the topology info */
    if (0 == orte_default_num_sockets_per_board) {
        /* we weren't given a number, so try to determine it */
        if (OPAL_SUCCESS != opal_paffinity_base_get_socket_info(&value)) {
            /* can't get any info - default to 1 */
            value = 1;
        }
        orte_default_num_sockets_per_board = (uint8_t)value;
    }
    if (0 == orte_default_num_cores_per_socket) {
        /* we weren't given a number, so try to determine it */
        if (OPAL_SUCCESS != (ret = opal_paffinity_base_get_core_info(0, &value))) {
            /* don't have topo info - can we at least get #processors? */
            if (OPAL_SUCCESS != opal_paffinity_base_get_processor_info(&value)) {
                /* can't get any info - default to 1 */
                value = 1;
            }
        }
        orte_default_num_cores_per_socket = (uint8_t)value;
    }
    
    /* if we are using xml for output, put an mpirun start tag */
    if (orte_xml_output) {
        fprintf(orte_xml_fp, "<mpirun>\n");
        fflush(orte_xml_fp);
    }

    /* open and setup the opal_pstat framework so we can provide
     * process stats if requested
     */
    if (ORTE_SUCCESS != (ret = opal_pstat_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "opal_pstat_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = opal_pstat_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_pstat_base_select";
        goto error;
    }
    
    /* open and setup the local resource discovery framework */
    if (ORTE_SUCCESS != (ret = opal_sysinfo_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "opal_sysinfo_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = opal_sysinfo_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "opal_sysinfo_base_select";
        goto error;
    }

    /* Since we are the HNP, then responsibility for
     * defining the name falls to the PLM component for our
     * respective environment - hence, we have to open the PLM
     * first and select that component.
     */
    if (ORTE_SUCCESS != (ret = orte_plm_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_plm_base_open";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_plm_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_plm_base_select";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_plm.set_hnp_name())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_plm_set_hnp_name";
        goto error;
    }
    
    /* Setup the communication infrastructure */
    
    /*
     * Runtime Messaging Layer
     */
    if (ORTE_SUCCESS != (ret = orte_rml_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rml_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_rml_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rml_base_select";
        goto error;
    }
    /*
     * Routed system
     */
    if (ORTE_SUCCESS != (ret = orte_routed_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_routed_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_routed_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_routed_base_select";
        goto error;
    }
    
    /* multicast */
    if (ORTE_SUCCESS != (ret = orte_rmcast_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rmcast_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_rmcast_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rmcast_base_select";
        goto error;
    }
    
    /*
     * Group communications
     */
    if (ORTE_SUCCESS != (ret = orte_grpcomm_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_grpcomm_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_grpcomm_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_grpcomm_base_select";
        goto error;
    }

    /* Now provide a chance for the PLM
     * to perform any module-specific init functions. This
     * needs to occur AFTER the communications are setup
     * as it may involve starting a non-blocking recv
     */
    if (ORTE_SUCCESS != (ret = orte_plm.init())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_plm_init";
        goto error;
    }

    /*
     * Setup the remaining resource
     * management and errmgr frameworks - application procs
     * and daemons do not open these frameworks as they only use
     * the hnp proxy support in the PLM framework.
     */
    if (ORTE_SUCCESS != (ret = orte_ras_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_ras_base_open";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_ras_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_ras_base_find_available";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_rmaps_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rmaps_base_open";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_rmaps_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rmaps_base_find_available";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_errmgr_base_open())) {
        error = "orte_errmgr_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_errmgr_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_errmgr_base_select";
        goto error;
    }
    
    /* Open/select the odls */
    if (ORTE_SUCCESS != (ret = orte_odls_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_odls_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_odls_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_odls_base_select";
        goto error;
    }
    
    /* enable communication with the rml */
    if (ORTE_SUCCESS != (ret = orte_rml.enable_comm())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rml.enable_comm";
        goto error;
    }

    /* set the communication function */
    orte_comm = orte_global_comm;

    /* we are an hnp, so update the contact info field for later use */
    orte_process_info.my_hnp_uri = orte_rml.get_contact_info();
    
    /* we are also officially a daemon, so better update that field too */
    orte_process_info.my_daemon_uri = orte_rml.get_contact_info();
    
#if !ORTE_DISABLE_FULL_SUPPORT
    /* setup the orte_show_help system to recv remote output */
    ret = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_SHOW_HELP,
                                 ORTE_RML_NON_PERSISTENT, orte_show_help_recv, NULL);
    if (ret != ORTE_SUCCESS && OPAL_SOS_GET_ERROR_CODE(ret) != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(ret);
        error = "setup receive for orte_show_help";
        goto error;
    }
#endif

    /* setup my session directory */
    if (orte_create_session_dirs) {
        OPAL_OUTPUT_VERBOSE((2, orte_debug_output,
                             "%s setting up session dir with\n\ttmpdir: %s\n\thost %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (NULL == orte_process_info.tmpdir_base) ? "UNDEF" : orte_process_info.tmpdir_base,
                             orte_process_info.nodename));
        
        if (ORTE_SUCCESS != (ret = orte_session_dir(true,
                                                    orte_process_info.tmpdir_base,
                                                    orte_process_info.nodename, NULL,
                                                    ORTE_PROC_MY_NAME))) {
            ORTE_ERROR_LOG(ret);
            error = "orte_session_dir";
            goto error;
        }
        
        /* Once the session directory location has been established, set
         the opal_output hnp file location to be in the
         proc-specific session directory. */
        opal_output_set_output_file_info(orte_process_info.proc_session_dir,
                                         "output-", NULL, NULL);
        
        /* save my contact info in a file for others to find */
        jobfam_dir = opal_dirname(orte_process_info.job_session_dir);
        contact_path = opal_os_path(false, jobfam_dir, "contact.txt", NULL);
        free(jobfam_dir);
        
        OPAL_OUTPUT_VERBOSE((2, orte_debug_output,
                             "%s writing contact file %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             contact_path));
        
        if (ORTE_SUCCESS != (ret = orte_write_hnp_contact_file(contact_path))) {
            OPAL_OUTPUT_VERBOSE((2, orte_debug_output,
                                 "%s writing contact file failed with error %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_ERROR_NAME(ret)));
        } else {
            OPAL_OUTPUT_VERBOSE((2, orte_debug_output,
                                 "%s wrote contact file",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        }
        free(contact_path);
    }

    /* setup the global job and node arrays */
    orte_job_data = OBJ_NEW(opal_pointer_array_t);
    if (ORTE_SUCCESS != (ret = opal_pointer_array_init(orte_job_data,
                                                      1,
                                                      ORTE_GLOBAL_ARRAY_MAX_SIZE,
                                                      1))) {
        ORTE_ERROR_LOG(ret);
        error = "setup job array";
        goto error;
    }
    
    orte_node_pool = OBJ_NEW(opal_pointer_array_t);
    if (ORTE_SUCCESS != (ret = opal_pointer_array_init(orte_node_pool,
                                                      ORTE_GLOBAL_ARRAY_BLOCK_SIZE,
                                                      ORTE_GLOBAL_ARRAY_MAX_SIZE,
                                                      ORTE_GLOBAL_ARRAY_BLOCK_SIZE))) {
        ORTE_ERROR_LOG(ret);
        error = "setup node array";
        goto error;
    }
    
    /* Setup the job data object for the daemons */        
    /* create and store the job data object */
    jdata = OBJ_NEW(orte_job_t);
    jdata->jobid = ORTE_PROC_MY_NAME->jobid;
    opal_pointer_array_set_item(orte_job_data, 0, jdata);
   
    /* create and store a node object where we are */
    node = OBJ_NEW(orte_node_t);
    node->name = strdup(orte_process_info.nodename);
    node->index = opal_pointer_array_add(orte_node_pool, node);

    /* create and store a proc object for us */
    proc = OBJ_NEW(orte_proc_t);
    proc->name.jobid = ORTE_PROC_MY_NAME->jobid;
    proc->name.vpid = ORTE_PROC_MY_NAME->vpid;
    proc->pid = orte_process_info.pid;
    proc->rml_uri = orte_rml.get_contact_info();
    proc->state = ORTE_PROC_STATE_RUNNING;
    OBJ_RETAIN(node);  /* keep accounting straight */
    proc->node = node;
    proc->nodename = node->name;
    opal_pointer_array_add(jdata->procs, proc);

    /* record that the daemon (i.e., us) is on this node 
     * NOTE: we do not add the proc object to the node's
     * proc array because we are not an application proc.
     * Instead, we record it in the daemon field of the
     * node object
     */
    OBJ_RETAIN(proc);   /* keep accounting straight */
    node->daemon = proc;
    node->daemon_launched = true;
    node->state = ORTE_NODE_STATE_UP;
    
    /* record that the daemon job is running */
    jdata->num_procs = 1;
    jdata->state = ORTE_JOB_STATE_RUNNING;
    
    /* setup the routed info - the selected routed component
     * will know what to do. 
     */
    if (ORTE_SUCCESS != (ret = orte_routed.init_routes(ORTE_PROC_MY_NAME->jobid, NULL))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_routed.init_routes";
        goto error;
    }
    
    /* setup I/O forwarding system - must come after we init routes */
    if (ORTE_SUCCESS != (ret = orte_iof_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_iof_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_iof_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_iof_base_select";
        goto error;
    }
    
    /* setup the FileM */
    if (ORTE_SUCCESS != (ret = orte_filem_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_filem_base_open";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_filem_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_filem_base_select";
        goto error;
    }

#if OPAL_ENABLE_FT_CR == 1
    /*
     * Setup the SnapC
     */
    if (ORTE_SUCCESS != (ret = orte_snapc_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_snapc_base_open";
        goto error;
    }

    if (ORTE_SUCCESS != (ret = orte_snapc_base_select(ORTE_PROC_IS_HNP, !ORTE_PROC_IS_DAEMON))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_snapc_base_select";
        goto error;
    }

    /* For HNP, ORTE doesn't need the OPAL CR stuff */
    opal_cr_set_enabled(false);
#else
    opal_cr_set_enabled(false);
#endif

    /*
     * Initalize the CR setup
     * Note: Always do this, even in non-FT builds.
     * If we don't some user level tools may hang.
     */
    if (ORTE_SUCCESS != (ret = orte_cr_init())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_cr_init";
        goto error;
    }
    
    /* setup the notifier system */
    if (ORTE_SUCCESS != (ret = orte_notifier_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_notifer_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_notifier_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_notifer_select";
        goto error;
    }

    /* setup the db framework */
    if (ORTE_SUCCESS != (ret = orte_db_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_db_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_db_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_db_select";
        goto error;
    }
    
    /* setup the SENSOR framework */
    if (ORTE_SUCCESS != (ret = orte_sensor_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_sensor_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_sensor_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_sensor_select";
        goto error;
    }
    /* start the local sensors */
    orte_sensor.start(ORTE_PROC_MY_NAME->jobid);
    
    /* start the debuggers */
    if (ORTE_SUCCESS != (ret = orte_debugger_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_debugger_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_debugger_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_debugger_select";
        goto error;
    }

    /* if a tool has launched us and is requesting event reports,
     * then set its contact info into the comm system
     */
    if (orte_report_events) {
        if (ORTE_SUCCESS != (ret = orte_util_comm_connect_tool(orte_report_events_uri))) {
            error = "could not connect to tool";
            goto error;
        }
    }

    /* We actually do *not* want an HNP to voluntarily yield() the
     processor more than necessary.  Orterun already blocks when
     it is doing nothing, so it doesn't use any more CPU cycles than
     it should; but when it *is* doing something, we do not want it
     to be unnecessarily delayed because it voluntarily yielded the
     processor in the middle of its work.
     
     For example: when a message arrives at orterun, we want the
     OS to wake us up in a timely fashion (which most OS's
     seem good about doing) and then we want orterun to process
     the message as fast as possible.  If orterun yields and lets
     aggressive MPI applications get the processor back, it may be a
     long time before the OS schedules orterun to run again
     (particularly if there is no IO event to wake it up).  Hence,
     routed OOB messages (for example) may be significantly delayed
     before being delivered to MPI processes, which can be
     problematic in some scenarios (e.g., COMM_SPAWN, BTL's that
     require OOB messages for wireup, etc.). */
    opal_progress_set_yield_when_idle(false);
    
    return ORTE_SUCCESS;

error:
    if (ORTE_ERR_SILENT != OPAL_SOS_GET_ERROR_CODE(ret)) {
        orte_show_help("help-orte-runtime.txt",
                       "orte_init:startup:internal-failure",
                       true, error, ORTE_ERROR_NAME(ret), ret);
    }
    
    return ret;
}

static int rte_finalize(void)
{
    char *contact_path;
    orte_node_t *node;
    orte_job_t *job;
    int i;

    if (signals_set) {
        /* Remove the epipe handler */
        opal_event_signal_del(&epipe_handler);
        /* Remove the TERM and INT signal handlers */
        opal_event_signal_del(&term_handler);
        opal_event_signal_del(&int_handler);
#ifndef __WINDOWS__
        /** Remove the USR signal handlers */
        opal_event_signal_del(&sigusr1_handler);
        opal_event_signal_del(&sigusr2_handler);
        if (orte_forward_job_control) {
            opal_event_signal_del(&sigtstp_handler);
            opal_event_signal_del(&sigcont_handler);
        }
#endif  /* __WINDOWS__ */
        signals_set = false;
    }

    /* stop the debuggers */
    orte_debugger_base_close();

    /* stop the local sensors */
    orte_sensor.stop(ORTE_PROC_MY_NAME->jobid);

    /* remove my contact info file */
    contact_path = opal_os_path(false, orte_process_info.top_session_dir,
                                "contact.txt", NULL);
    unlink(contact_path);
    free(contact_path);
    
    orte_sensor_base_close();
    orte_db_base_close();
    orte_notifier_base_close();
    
    orte_cr_finalize();
    
#if OPAL_ENABLE_FT_CR == 1
    orte_snapc_base_close();
#endif
    orte_filem_base_close();
    
    orte_odls_base_close();
    
    orte_wait_finalize();
    orte_iof_base_close();
    
    /* finalize selected modules so they can de-register
     * any receives
     */
    orte_ras_base_close();
    orte_rmaps_base_close();
    orte_plm_base_close();
    orte_errmgr_base_close();
    orte_grpcomm_base_close();

    /* close the multicast */
    orte_rmcast_base_close();

    /* now can close the rml */
    orte_routed_base_close();
    orte_rml_base_close();

    /* if we were doing timing studies, close the timing file */
    if (orte_timing) {
        if (stdout != orte_timing_output &&
            stderr != orte_timing_output) {
            fclose(orte_timing_output);
        }
    }
    
    /* cleanup the job and node info arrays */
    if (NULL != orte_node_pool) {
        for (i=0; i < orte_node_pool->size; i++) {
            if (NULL != (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool,i))) {
                OBJ_RELEASE(node);
            }
        }
        OBJ_RELEASE(orte_node_pool);
    }
    if (NULL != orte_job_data) {
        for (i=0; i < orte_job_data->size; i++) {
            if (NULL != (job = (orte_job_t*)opal_pointer_array_get_item(orte_job_data,i))) {
                OBJ_RELEASE(job);
            }
        }
        OBJ_RELEASE(orte_job_data);
    }

    /* finalize the session directory tree */
    orte_session_dir_finalize(ORTE_PROC_MY_NAME);
    
    /* clean out the global structures */
    orte_proc_info_finalize();
    if (NULL != orte_job_ident) {
        free(orte_job_ident);
    }
    
    /* close the xml output file, if open */
    if (orte_xml_output) {
        fprintf(orte_xml_fp, "</mpirun>\n");
        fflush(orte_xml_fp);
        if (stdout != orte_xml_fp) {
            fclose(orte_xml_fp);
        }
    }
    
    /* handle the orted-specific OPAL stuff */
    opal_sysinfo_base_close();
    opal_pstat_base_close();

    return ORTE_SUCCESS;    
}

static void rte_abort(int status, bool report)
{
    /* do NOT do a normal finalize as this will very likely
     * hang the process. We are aborting due to an abnormal condition
     * that precludes normal cleanup 
     *
     * We do need to do the following bits to make sure we leave a 
     * clean environment. Taken from orte_finalize():
     * - Assume errmgr cleans up child processes before we exit.
     */
    
    /* CRS cleanup since it may have a named pipe and thread active */
    orte_cr_finalize();
    
    /* - Clean out the global structures 
     * (not really necessary, but good practice)
     */
    orte_proc_info_finalize();
    
    /* just exit */
    exit(status);
}

static uint8_t proc_get_locality(orte_process_name_t *proc)
{
    orte_node_t *node;
    orte_proc_t *myproc;
    int i;
    
    /* the HNP is always on node=0 of the node array */
    node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, 0);
    
    /* cycle through the array of local procs */
    for (i=0; i < node->procs->size; i++) {
        if (NULL == (myproc = (orte_proc_t*)opal_pointer_array_get_item(node->procs, i))) {
            continue;
        }
        if (myproc->name.jobid == proc->jobid &&
            myproc->name.vpid == proc->vpid) {
            OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                                 "%s ess:hnp: proc %s is LOCAL",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(proc)));
            return (OPAL_PROC_ON_NODE | OPAL_PROC_ON_CU | OPAL_PROC_ON_CLUSTER);
        }
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:hnp: proc %s is REMOTE",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc)));
    
    return OPAL_PROC_NON_LOCAL;
    
}

static orte_proc_t* find_proc(orte_process_name_t *proc)
{
    orte_job_t *jdata;
    
    if (NULL == (jdata = orte_get_job_data_object(proc->jobid))) {
        return NULL;
    }

    return (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, proc->vpid);
}


static orte_vpid_t proc_get_daemon(orte_process_name_t *proc)
{
    orte_proc_t *pdata;
    
    if( ORTE_JOBID_IS_DAEMON(proc->jobid) ) {
        return proc->vpid;
    }

    /* get the job data */
     if (NULL == (pdata = find_proc(proc))) {
         return ORTE_VPID_INVALID;
     }
     
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:hnp: proc %s is hosted by daemon %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         ORTE_VPID_PRINT(pdata->node->daemon->name.vpid)));
    
    return pdata->node->daemon->name.vpid;
}

static char* proc_get_hostname(orte_process_name_t *proc)
{
    orte_proc_t *pdata;
    
    if (NULL == (pdata = find_proc(proc))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return NULL;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:hnp: proc %s is on host %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         pdata->node->name));
    
    return pdata->node->name;
}

static orte_local_rank_t proc_get_local_rank(orte_process_name_t *proc)
{
    orte_proc_t *pdata;
    
    if (NULL == (pdata = find_proc(proc))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_LOCAL_RANK_INVALID;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:hnp: proc %s has local rank %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         (int)pdata->local_rank));
    
    return pdata->local_rank;
}

static orte_node_rank_t proc_get_node_rank(orte_process_name_t *proc)
{
    orte_proc_t *pdata;
    
    if (NULL == (pdata = find_proc(proc))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_NODE_RANK_INVALID;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:hnp: proc %s has node rank %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         (int)pdata->node_rank));
    
    return pdata->node_rank;
}

static int update_pidmap(opal_byte_object_t *bo)
{
    /* there is nothing to do here - the HNP can resolve
     * all requests directly from its internal data. However,
     * we do need to free the data in the byte object to
     * be consistent with other modules
     */
    if (NULL != bo && NULL != bo->bytes) {
        free(bo->bytes);
    }
    return ORTE_SUCCESS;
}

static int update_nidmap(opal_byte_object_t *bo)
{
    /* there is nothing to do here - the HNP can resolve
     * all requests directly from its internal data. However,
     * we do need to free the data in the byte object to
     * be consistent with other modules
     */
    if (NULL != bo && NULL != bo->bytes) {
        free(bo->bytes);
    }
    return ORTE_SUCCESS;
}

static bool forcibly_die=false;

static void abort_exit_callback(int fd, short ign, void *arg)
{
    int ret;

    fprintf(stderr, "%s: killing job...\n\n", orte_basename);
    
    /* since we are being terminated by a user's signal, be
     * sure to exit with a non-zero exit code - but don't
     * overwrite any error code from a proc that might have
     * failed, in case that is why the user ordered us
     * to terminate
     */
    ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);

    /* terminate the job - this will also wakeup orterun so
     * it can report to the user and kill all the orteds.
     * Check the jobid, though, just in case the user
     * hit ctrl-c before we had a chance to setup the
     * job in the system - in which case there is nothing
     * to terminate!
     */
    if (!orte_never_launched) {
        /* if the debuggers were run, clean up */
        orte_debugger.finalize();

        /*
         * Turn off the process recovery functionality, if it was enabled.
         * This keeps the errmgr from trying to recover from the shutdown
         * procedure.
         */
        orte_enable_recovery             = false;
        
        /* terminate the orteds - they will automatically kill
         * their local procs
         */
        ret = orte_plm.terminate_orteds();
       
    } else {
        /* if the jobid is invalid or we never launched,
         * there is nothing to do but just clean ourselves
         * up and exit
         */
        orte_quit();
    }
}

/*
 * Attempt to terminate the job and wait for callback indicating
 * the job has been aborted.
 */
static void abort_signal_callback(int fd, short flags, void *arg)
{
    /* if we have already ordered this once, don't keep
     * doing it to avoid race conditions
     */
    if (!opal_atomic_trylock(&orte_abort_inprogress_lock)) { /* returns 1 if already locked */
        if (forcibly_die) {
            /* kill any local procs */
            orte_odls.kill_local_procs(NULL);
            
            /* whack any lingering session directory files from our jobs */
            orte_session_dir_cleanup(ORTE_JOBID_WILDCARD);
            
            /* cleanup our data server */
            orte_data_server_finalize();
            
            /* exit with a non-zero status */
            exit(ORTE_ERROR_DEFAULT_EXIT_CODE);
        }
        fprintf(stderr, "%s: abort is already in progress...hit ctrl-c again to forcibly terminate\n\n", orte_basename);
        forcibly_die = true;
        return;
    }

    /* set the global abnormal exit flag so we know not to
     * use the standard xcast for terminating orteds
     */
    orte_abnormal_term_ordered = true;
    /* ensure that the forwarding of stdin stops */
    orte_job_term_ordered = true;

    /* tell us to be quiet - hey, the user killed us with a ctrl-c,
     * so need to tell them that!
     */
    orte_execute_quiet = true;
    
    /* We are in an event handler; the job completed procedure
       will delete the signal handler that is currently running
       (which is a Bad Thing), so we can't call it directly.
       Instead, we have to exit this handler and setup to call
       job_completed() after this. */
    ORTE_TIMER_EVENT(0, 0, abort_exit_callback);
}

/**
 * Deal with sigpipe errors
 */
static int sigpipe_error_count=0;
static void epipe_signal_callback(int fd, short flags, void *arg)
{
    sigpipe_error_count++;
    if (1 == sigpipe_error_count) {
        /* announce it */
        OPAL_OUTPUT_VERBOSE((1, orte_debug_verbosity,
                             "%s reports a SIGPIPE error on fd %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), fd));
    }

    if (10 < sigpipe_error_count) {
        /* time to abort */
        opal_output(0, "%s: SIGPIPE detected - aborting", orte_basename);
        abort_exit_callback(0, 0, 0);
    }

    return;
}

/**
 * Pass user signals to the remote application processes
 */
static void  signal_forward_callback(int fd, short event, void *arg)
{
    opal_event_t *signal = (opal_event_t*)arg;
    int signum, ret;

    signum = OPAL_EVENT_SIGNAL(signal);
    if (!orte_execute_quiet){
        fprintf(stderr, "%s: Forwarding signal %d to job\n",
                orte_basename, signum);
    }

    /** send the signal out to the processes, including any descendants */
    if (ORTE_SUCCESS != (ret = orte_plm.signal_job(ORTE_JOBID_WILDCARD, signum))) {
        fprintf(stderr, "Signal %d could not be sent to the job (returned %d)",
                signum, ret);
    }
}
