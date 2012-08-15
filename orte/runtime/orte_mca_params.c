/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2011 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009-2010 Oracle and/or its affiliates.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <stdio.h>

#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/installdirs/installdirs.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"

#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_globals.h"

static bool passed_thru = false;

int orte_register_params(void)
{
    int value;
    char *strval, *strval1, *strval2;

    /* only go thru this once - mpirun calls it twice, which causes
     * any error messages to show up twice
     */
    if (passed_thru) {
        return ORTE_SUCCESS;
    }
    passed_thru = true;
    
#if !ORTE_DISABLE_FULL_SUPPORT
    /* get a clean output channel too - need to do this here because
     * we use it below, and orterun and some other tools call this
     * function prior to calling orte_init
     */
    {
        opal_output_stream_t lds;
        OBJ_CONSTRUCT(&lds, opal_output_stream_t);
        lds.lds_want_stdout = true;
        orte_clean_output = opal_output_open(&lds);
        OBJ_DESTRUCT(&lds);
    }
#endif /* !ORTE_DISABLE_FULL_SUPPORT */

    mca_base_param_reg_int_name("orte", "base_help_aggregate",
                                "If orte_base_help_aggregate is true, duplicate help messages will be aggregated rather than displayed individually.  This can be helpful for parallel jobs that experience multiple identical failures; rather than print out the same help/failure message N times, display it once with a count of how many processes sent the same message.",
                                false, false,
                                (int) true, &value);
    orte_help_want_aggregate = OPAL_INT_TO_BOOL(value);
    
    /* LOOK FOR A TMP DIRECTORY BASE */
    /* Several options are provided to cover a range of possibilities:
     *
     * (a) all processes need to use a specified location as the base
     *     for tmp directories
     * (b) daemons on remote nodes need to use a specified location, but
     *     one different from that used by mpirun
     * (c) mpirun needs to use a specified location, but one different
     *     from that used on remote nodes
     */
    mca_base_param_reg_string_name("orte", "tmpdir_base",
                                   "Base of the session directory tree to be used by all processes",
                                   false, false, NULL,  &strval);
   
    mca_base_param_reg_string_name("orte", "local_tmpdir_base",
                                   "Base of the session directory tree to be used by orterun/mpirun",
                                   false, false, NULL,  &strval1);

    mca_base_param_reg_string_name("orte", "remote_tmpdir_base",
                                   "Base of the session directory tree on remote nodes, if required to be different from head node",
                                   false, false, NULL,  &strval2);

    /* if a global tmpdir was specified, then we do not allow specification
     * of the local or remote values to avoid confusion
     */
    if (NULL != strval &&
        (NULL != strval1 || NULL != strval2)) {
            opal_output(orte_clean_output,
                        "------------------------------------------------------------------\n"
                        "The MCA param orte_tmpdir_base was specified, which sets the base\n"
                        "of the temporary directory tree for all procs. However, values for\n"
                        "the local and/or remote tmpdir base were also given. This can lead\n"
                        "to confusion and is therefore not allowed. Please specify either a\n"
                        "global tmpdir base OR a local/remote tmpdir base value\n"
                        "------------------------------------------------------------------");
            exit(1);
    }
     
    if (NULL != strval) {
        if (NULL != orte_process_info.tmpdir_base) {
            free(orte_process_info.tmpdir_base);
        }
        orte_process_info.tmpdir_base = strval;
    } else if (ORTE_PROC_IS_HNP && NULL != strval1) {
        /* orterun will pickup the value for its own use */
        if (NULL != orte_process_info.tmpdir_base) {
            free(orte_process_info.tmpdir_base);
        }
        orte_process_info.tmpdir_base = strval1;
    } else if (ORTE_PROC_IS_DAEMON && NULL != strval2) {
        /* orterun will pickup the value and forward it along, but must not
         * use it in its own work. So only a daemon needs to get it, and the
         * daemon will pass it down to its application procs. Note that orterun
         * will pass -its- value to any procs local to it
         */
        if (NULL != orte_process_info.tmpdir_base) {
            free(orte_process_info.tmpdir_base);
        }
        orte_process_info.tmpdir_base = strval2;
    }

    mca_base_param_reg_string_name("orte", "no_session_dirs",
                                   "Prohibited locations for session directories (multiple locations separated by ',', default=NULL)",
                                   false, false, NULL,  &orte_prohibited_session_dirs);

    mca_base_param_reg_int_name("orte", "create_session_dirs",
                                "Create session directories",
                                false, false, orte_create_session_dirs, &value);
    orte_create_session_dirs = OPAL_INT_TO_BOOL(value);
    
    
    mca_base_param_reg_int_name("orte", "execute_quiet",
                                "Do not output error and help messages",
                                false, false, (int) false, &value);
    orte_execute_quiet = OPAL_INT_TO_BOOL(value);
    
    mca_base_param_reg_int_name("orte", "report_silent_errors",
                                "Report all errors, including silent ones",
                                false, false, (int) false, &value);
    orte_report_silent_errors = OPAL_INT_TO_BOOL(value);

#if !ORTE_DISABLE_FULL_SUPPORT
    
    mca_base_param_reg_int_name("orte", "debug",
                                "Top-level ORTE debug switch (default verbosity: 1)",
                                false, false, (int)false, &value);
    orte_debug_flag = OPAL_INT_TO_BOOL(value);
    
    mca_base_param_reg_int_name("orte", "debug_verbose",
                                "Verbosity level for ORTE debug messages (default: 1)",
                                false, false, -1, &orte_debug_verbosity);
    
    mca_base_param_reg_int_name("orte", "debug_daemons",
                                "Whether to debug the ORTE daemons or not",
                                false, false, (int)false, &value);
    orte_debug_daemons_flag = OPAL_INT_TO_BOOL(value);

    mca_base_param_reg_int_name("orte", "debug_daemons_file",
                                "Whether want stdout/stderr of daemons to go to a file or not",
                                false, false, (int)false, &value);
    orte_debug_daemons_file_flag = OPAL_INT_TO_BOOL(value);
    /* If --debug-daemons-file was specified, that also implies
       --debug-daemons */
    if (orte_debug_daemons_file_flag) {
        orte_debug_daemons_flag = true;
    }

    /* do we want session output left open? */
    mca_base_param_reg_int_name("orte", "leave_session_attached",
                                "Whether applications and/or daemons should leave their sessions "
                                "attached so that any output can be received - this allows X forwarding "
                                "without all the attendant debugging output",
                                false, false, (int)false, &value);
    orte_leave_session_attached = OPAL_INT_TO_BOOL(value);
    
    /* if any debug level is set, ensure we output debug level dumps */
    if (orte_debug_flag || orte_debug_daemons_flag || orte_leave_session_attached) {
        orte_devel_level_output = true;
    }

    /* See comment in orte/tools/orterun/orterun.c about this MCA
       param (this param is internal) */
    mca_base_param_reg_int_name("orte",
                                "in_parallel_debugger",
                                "Whether the application is being debugged "
                                "in a parallel debugger (default: false)",
                                true, false, 0, &value);
    orte_in_parallel_debugger = OPAL_INT_TO_BOOL(value);

    mca_base_param_reg_int_name("orte",
                                "output_debugger_proctable",
                                "Whether or not to output the debugger proctable after launch (default: false)",
                                false, false, 0, &value);
    orte_debugger_dump_proctable = OPAL_INT_TO_BOOL(value);

    mca_base_param_reg_string_name("orte", "debugger_test_daemon",
                                   "Name of the executable to be used to simulate a debugger colaunch (relative or absolute path)",
                                   false, false, NULL, &orte_debugger_test_daemon);

    mca_base_param_reg_int_name("orte",
                                "debugger_test_attach",
                                "Test debugger colaunch after debugger attachment",
                                false, false, 0, &value);
    orte_debugger_test_attach = OPAL_INT_TO_BOOL(value);

    mca_base_param_reg_int_name("orte",
                                "debugger_check_rate",
                                "Set rate (in secs) for auto-detect of debugger attachment (0 => do not check)",
                                false, false, 0, &orte_debugger_check_rate);

    mca_base_param_reg_int_name("orte", "do_not_launch",
                                "Perform all necessary operations to prepare to launch the application, but do not actually launch it",
                                false, false, (int)false, &value);
    orte_do_not_launch = OPAL_INT_TO_BOOL(value);
    
    mca_base_param_reg_int_name("orte", "daemon_spin",
                                "Have any orteds spin until we can connect a debugger to them",
                                false, false, (int)false, &value);
    orted_spin_flag = OPAL_INT_TO_BOOL(value);

    mca_base_param_reg_int_name("orte", "daemon_fail",
                                "Have the specified orted fail after init for debugging purposes",
                                false, false, ORTE_VPID_INVALID, &orted_debug_failure);
    
    mca_base_param_reg_int_name("orte", "daemon_fail_delay",
                                "Have the specified orted fail after specified number of seconds (default: 0 => no delay)",
                                false, false, 0, &orted_debug_failure_delay);

    mca_base_param_reg_int_name("orte", "startup_timeout",
                                "Milliseconds/daemon to wait for startup before declaring failed_to_start (default: 0 => do not check)",
                                false, false, 0, &orte_startup_timeout);
 
    /* check for timing requests */
    mca_base_param_reg_int_name("orte", "timing",
                                "Request that critical timing loops be measured",
                                false, false, (int)false, &value);
    orte_timing = OPAL_INT_TO_BOOL(value);

    mca_base_param_reg_int_name("orte", "timing_details",
                                "Request that detailed timing data by reported",
                                false, false, (int)false, &value);
    orte_timing_details = OPAL_INT_TO_BOOL(value);
    if (orte_timing_details) {
        /* ensure the timing flag is set too */
        orte_timing = true;
    }
    
    if (ORTE_PROC_IS_HNP) {
        char *tmp;
        mca_base_param_reg_string_name("orte", "timing_file",
                                       "Name of the file where timing data is to be written (relative or absolute path)",
                                       false, false, NULL, &tmp);
        if (orte_timing && NULL == tmp) {
            /* send the timing output to stdout */
            orte_timing_output = stdout;
        } else if (NULL != tmp) {
            /* make sure the timing flag is set */
            orte_timing = true;
            /* send the output to the indicated file */
            orte_timing_output = fopen(tmp,  "w");
            if (NULL == orte_timing_output) {
                /* couldn't be opened */
                opal_output(0, "File %s could not be opened", tmp);
                orte_timing_output = stderr;
            }
        }        
    }
    
    /* User-level debugger info string */

    mca_base_param_reg_string_name("orte", "base_user_debugger",
                                   "Sequence of user-level debuggers to search for in orterun",
                                   false, false, "totalview @mpirun@ -a @mpirun_args@ : ddt -n @np@ -start @executable@ @executable_argv@ @single_app@ : fxp @mpirun@ -a @mpirun_args@", NULL);


    mca_base_param_reg_int_name("orte", "abort_timeout",
                                "Max time to wait [in secs] before aborting an ORTE operation (default: 1sec)",
                                false, false, 1, &value);
    orte_max_timeout = 1000000.0 * value;  /* convert to usec */

    mca_base_param_reg_int_name("orte", "timeout_step",
                                "Time to wait [in usecs/proc] before aborting an ORTE operation (default: 1000 usec/proc)",
                                false, false, 1000, &orte_timeout_usec_per_proc);
    
    /* default hostfile */
    mca_base_param_reg_string_name("orte", "default_hostfile",
                                   "Name of the default hostfile (relative or absolute path, \"none\" to ignore environmental or default MCA param setting)",
                                   false, false, NULL, &strval);
    if (NULL == strval) {
        /* nothing was given, so define the default */
        asprintf(&orte_default_hostfile, "%s/openmpi-default-hostfile", opal_install_dirs.sysconfdir);
        /* flag that nothing was given */
        orte_default_hostfile_given = false;
    } else if (0 == strcmp(strval, "none")) {
        orte_default_hostfile = NULL;
        /* flag that it was given */
        orte_default_hostfile_given = true;
    } else {
        orte_default_hostfile = strval;
        /* flag that it was given */
        orte_default_hostfile_given = true;
    }
    
#ifdef __WINDOWS__
    mca_base_param_reg_string_name("orte", "ccp_headnode",
                                   "Name of the cluster head node. (For Windows CCP only.)",
                                   false, false,
                                   NULL, &orte_ccp_headnode);
#endif
    

    /* regex of nodes in system */
    mca_base_param_reg_string_name("orte", "node_regex",
                                   "Regular expression defining nodes in the system",
                                   false, false, NULL, &orte_node_regex);

    /* whether or not to keep FQDN hostnames */
    mca_base_param_reg_int_name("orte", "keep_fqdn_hostnames",
                                "Whether or not to keep FQDN hostnames [default: no]",
                                false, false, (int)false, &value);
    orte_keep_fqdn_hostnames = OPAL_INT_TO_BOOL(value);
    
    /* whether to tag output */
    mca_base_param_reg_int_name("orte", "tag_output",
                                "Tag all output with [job,rank] (default: false)",
                                false, false, (int) false, &value);
    orte_tag_output = OPAL_INT_TO_BOOL(value);
    /* if we requested xml output, be sure to tag the output as well */
    if (orte_xml_output) {
        orte_tag_output = true;
    }
    
    mca_base_param_reg_int_name("orte", "xml_output",
                                "Display all output in XML format (default: false)",
                                false, false, (int) false, &value);
    orte_xml_output = OPAL_INT_TO_BOOL(value);

    mca_base_param_reg_string_name("orte", "xml_file",
                                   "Provide all output in XML format to the specified file",
                                   false, false, NULL, &strval);
    if (NULL != strval) {
        if (ORTE_PROC_IS_HNP && NULL == orte_xml_fp) {
            /* only the HNP opens this file! Make sure it only happens once */
            orte_xml_fp = fopen(strval, "w");
            if (NULL == orte_xml_fp) {
                opal_output(0, "Could not open specified xml output file: %s", strval);
                return ORTE_ERROR;
            }
        }
        /* ensure we set the flags to tag output */
        orte_xml_output = true;
        orte_tag_output = true;
    } else {
        /* default to stdout */
        orte_xml_fp = stdout;
    }
        
    /* whether to timestamp output */
    mca_base_param_reg_int_name("orte", "timestamp_output",
                                "Timestamp all application process output (default: false)",
                                false, false, (int) false, &value);
    orte_timestamp_output = OPAL_INT_TO_BOOL(value);
    
    /* redirect output into files */
    mca_base_param_reg_string_name("orte", "output_filename",
                                   "Redirect output from application processes into filename.rank [default: NULL]",
                                   false, false, NULL, &orte_output_filename);
    
    mca_base_param_reg_int_name("orte", "show_resolved_nodenames",
                                "Display any node names that are resolved to a different name (default: false)",
                                false, false, (int) false, &value);
    orte_show_resolved_nodenames = OPAL_INT_TO_BOOL(value);
    
    mca_base_param_reg_int_name("orte", "hetero_apps",
                                "Indicates that multiple app_contexts are being provided that are a mix of 32/64 bit binaries (default: false)",
                                false, false, (int) false, &value);
    orte_hetero_apps = OPAL_INT_TO_BOOL(value);

#if OPAL_HAVE_HWLOC
    mca_base_param_reg_int_name("orte", "hetero_nodes",
                                "Nodes in cluster may differ in topology, so send the topology back from each node [Default = false]",
                                false, false, (int) false, &value);
    orte_hetero_nodes = OPAL_INT_TO_BOOL(value);
#endif

    /* allow specification of the launch agent */
    mca_base_param_reg_string_name("orte", "launch_agent",
                                   "Command used to start processes on remote nodes (default: orted)",
                                   false, false, "orted", &orte_launch_agent);

    mca_base_param_reg_string_name("orte", "fork_agent",
                                   "Command used to fork processes on remote nodes (default: NULL)",
                                   false, false, NULL, &strval);
    if (NULL != strval) {
        orte_fork_agent = opal_argv_split(strval, ' ');
    }

    /* whether or not to require RM allocation */
    mca_base_param_reg_int_name("orte", "allocation_required",
                                "Whether or not an allocation by a resource manager is required [default: no]",
                                false, false, (int)false, &value);
    orte_allocation_required = OPAL_INT_TO_BOOL(value);

    /* generate new terminal windows to display output from specified ranks */
    mca_base_param_reg_string_name("orte", "xterm",
                                   "Create a new xterm window and display output from the specified ranks there [default: none]",
                                   false, false, NULL, &orte_xterm);
    if (NULL != orte_xterm) {
        /* if an xterm request is given, we have to leave any ssh
         * sessions attached so the xterm window manager can get
         * back to the controlling terminal
         */
        orte_leave_session_attached = true;
        /* also want to redirect stddiag output from opal_output
         * to stderr from the process so those messages show
         * up in the xterm window instead of being forwarded to mpirun
         */
        orte_map_stddiag_to_stderr = true;
    }

    /* whether or not to map stddiag to stderr */
    mca_base_param_reg_int_name("orte", "map_stddiag_to_stderr",
                                "Map output from opal_output to stderr of the local process [default: no]",
                                false, false,
                                (int) false, &value);
    orte_map_stddiag_to_stderr = OPAL_INT_TO_BOOL(value);

    /* whether or not to forward SIGTSTP and SIGCONT signals */
    mca_base_param_reg_int_name("orte", "forward_job_control",
                                "Forward SIGTSTP (after converting to SIGSTOP) and SIGCONT signals to the application procs [default: no]",
                                false, false,
                                (int) false, &value);
    orte_forward_job_control = OPAL_INT_TO_BOOL(value);
    
    /* whether or not to report launch progress */
    mca_base_param_reg_int_name("orte", "report_launch_progress",
                                "Output a brief periodic report on launch progress [default: no]",
                                false, false,
                                (int) false, &value);
    orte_report_launch_progress = OPAL_INT_TO_BOOL(value);
    if (orte_report_launch_progress) {
        /* ensure the startup timeout is set to something reasonable */
        if (0 == orte_startup_timeout) {
            orte_startup_timeout = 2000;  /* default to 2 seconds */
        }
    }
    
    /* cluster hardware info detected by orte only */
    mca_base_param_reg_string_name("orte", "cpu_type",
                                   "cpu type detected in node",
                                   true, false, NULL, &orte_local_cpu_type);

    mca_base_param_reg_string_name("orte", "cpu_model",
                                   "cpu model detected in node",
                                   true, false, NULL, &orte_local_cpu_model);

    /* tool communication controls */
    mca_base_param_reg_string_name("orte", "report_events",
                                   "URI to which events are to be reported (default: NULL)",
                                   false, false, NULL, &orte_report_events_uri);
    if (NULL != orte_report_events_uri) {
        orte_report_events = true;
    }
    
    /* barrier control */
    mca_base_param_reg_int_name("orte", "do_not_barrier",
                                "Do not barrier in orte_init",
                                true, false,
                                (int) false, &value);
    orte_do_not_barrier = OPAL_INT_TO_BOOL(value);

    mca_base_param_reg_int_name("orte", "enable_recovery",
                                "Enable recovery from process failure [Default = disabled]",
                                false, false,
                                (int)false, &value);
    orte_enable_recovery = OPAL_INT_TO_BOOL(value);

    mca_base_param_reg_int_name("orte", "max_restarts",
                                "Max number of times to restart a failed process",
                                false, false,
                                0, &orte_max_restarts);
    
    if (!orte_enable_recovery && orte_max_restarts != 0) {
        if (ORTE_PROC_IS_HNP) {
            opal_output(orte_clean_output,
                        "------------------------------------------------------------------\n"
                        "The MCA param orte_enable_recovery was not set to true, but\n"
                        "a value was provided for the number of restarts:\n\n"
                        "Max restarts: %d\n"
                        "We are enabling process recovery and continuing execution. To avoid\n"
                        "this warning in the future, please set the orte_enable_recovery\n"
                        "param to non-zero.\n"
                        "------------------------------------------------------------------",
                        orte_max_restarts);            
        }
        orte_enable_recovery = true;
    }
    
    mca_base_param_reg_int_name("orte", "abort_on_non_zero_status",
                                "Abort the job if any process returns a non-zero exit status - no restart in such cases",
                                false, false, (int)true, &value);
    orte_abort_non_zero_exit = OPAL_INT_TO_BOOL(value);

    mca_base_param_reg_int_name("orte", "report_child_jobs_separately",
                                "Return the exit status of the primary job only",
                                false, false,
                                (int)false, &value);
    orte_report_child_jobs_separately = OPAL_INT_TO_BOOL(value);
    
    mca_base_param_reg_int_name("orte", "child_time_to_exit",
                                "Max time a spawned child job is allowed to run after the primary job has terminated (seconds)",
                                false, false,
                                INT_MAX, &value);
    orte_child_time_to_exit.tv_sec = value;
    orte_child_time_to_exit.tv_usec = 0;
 
    mca_base_param_reg_int_name("orte", "stat_history_size",
                                "Number of stat samples to keep",
                                false, false, 1, &orte_stat_history_size);

    mca_base_param_reg_string_name("orte", "forward_envars",
                                   "Comma-delimited environmental variables to forward, can include value to set",
                                   false, false, NULL, &orte_forward_envars);


    mca_base_param_reg_int_name("orte", "preload_binaries",
                                "Preload the binaries on remote machines before starting remote proceses",
                                false, false, (int)false, &value);
    orte_preload_binaries = OPAL_INT_TO_BOOL(value);

    mca_base_param_reg_int_name("orte", "max_vm_size",
                                "Maximum size of virtual machine - used to subdivide allocation",
                                false, false, -1, &orte_max_vm_size);

#if ORTE_ENABLE_STATIC_PORTS
    mca_base_param_reg_int_name("orte", "use_common_port",
                                "Daemons use same port as HNP",
                                false, false, (int)false, &value);
    orte_use_common_port = OPAL_INT_TO_BOOL(value);
#else
    orte_use_common_port = false;
#endif

#endif /* ORTE_DISABLE_FULL_SUPPORT */
    
    return ORTE_SUCCESS;
}
