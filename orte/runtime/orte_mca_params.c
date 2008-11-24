/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
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

#include "opal/mca/base/mca_base_param.h"

#include "orte/util/proc_info.h"
#include "orte/util/show_help.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_globals.h"

int orte_register_params(void)
{
    int value;
    
    mca_base_param_reg_int_name("orte", "base_help_aggregate",
                                "If orte_base_help_aggregate is true, duplicate help messages will be aggregated rather than displayed individually.  This can be helpful for parallel jobs that experience multiple identical failures; rather than print out the same help/failure message N times, display it once with a count of how many processes sent the same message.",
                                false, false,
                                (int) true, &value);
    orte_help_want_aggregate = OPAL_INT_TO_BOOL(value);
    
    mca_base_param_reg_string_name("orte", "tmpdir_base",
                                   "Base of the session directory tree",
                                   false, false, NULL,  &(orte_process_info.tmpdir_base));
   
    mca_base_param_reg_string_name("orte", "no_session_dirs",
                                   "Prohibited locations for session directories (multiple locations separated by ',', default=NULL)",
                                   false, false, NULL,  &orte_prohibited_session_dirs);

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
    
    /* See comment in orte/tools/orterun/debuggers.c about this MCA
       param (this param is internal) */
    mca_base_param_reg_int_name("orte",
                                "in_parallel_debugger",
                                "Whether the application is being debugged "
                                "in a parallel debugger (default: false)",
                                true, false, 0, &value);
    orte_in_parallel_debugger = OPAL_INT_TO_BOOL(value);

    mca_base_param_reg_int_name("orte",
                                "enable_debug_cospawn_while_running",
                                "Whether a debugger can attach to the job "
                                "while it is running and request it co-locate debugger daemons (default: false)",
                                false, false, (int)false, &value);
    orte_enable_debug_cospawn_while_running = OPAL_INT_TO_BOOL(value);
    
    mca_base_param_reg_int_name("orte",
                                "debugger_check_rate",
                                "How often (in seconds) to check if a debugger "
                                "has attached to a running job and requested cospawn support (default: 2 sec)",
                                false, false, 2, &orte_debugger_check_rate);
    
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

    mca_base_param_reg_int_name("orte", "heartbeat_rate",
                                "Seconds between checks for daemon state-of-health (default: 0 => do not check)",
                                false, false, 0, &orte_heartbeat_rate);
    
    mca_base_param_reg_int_name("orte", "startup_timeout",
                                "Milliseconds/daemon to wait for startup before declaring failed_to_start (default: 0 => do not check)",
                                false, false, 0, &orte_startup_timeout);
 
    /* check for timing requests */
    mca_base_param_reg_int_name("orte", "timing",
                                "Request that critical timing loops be measured",
                                false, false, (int)false, &value);
    orte_timing = OPAL_INT_TO_BOOL(value);
    
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
                                   "Name of the default hostfile (relative or absolute path)",
                                   false, false, NULL, &orte_default_hostfile);
    
    
    /* whether or not to keep FQDN hostnames */
    mca_base_param_reg_int_name("orte", "keep_fqdn_hostnames",
                                "Whether or not to keep FQDN hostnames [default: no]",
                                false, false, (int)false, &value);
    orte_keep_fqdn_hostnames = OPAL_INT_TO_BOOL(value);
    
    /* whether or not contiguous nodenames are in use */
    mca_base_param_reg_int_name("orte", "contiguous_nodes",
                                "Number of nodes after which contiguous nodename encoding will automatically be used [default: INT_MAX]",
                                false, false, INT32_MAX, &orte_contiguous_nodes);
    
    mca_base_param_reg_int_name("orte", "tag_output",
                                "Tag all output with [job,rank] (default: false)",
                                false, false, (int) false, &value);
    orte_tag_output = OPAL_INT_TO_BOOL(value);

    mca_base_param_reg_int_name("orte", "xml_output",
                                "Display all output in XML format (default: false)",
                                false, false, (int) false, &value);
    orte_xml_output = OPAL_INT_TO_BOOL(value);

    mca_base_param_reg_int_name("orte", "show_resolved_nodenames",
                                "Display any node names that are resolved to a different name (default: false)",
                                false, false, (int) false, &value);
    orte_show_resolved_nodenames = OPAL_INT_TO_BOOL(value);
    
    mca_base_param_reg_int_name("orte", "hetero_apps",
                                "Indicates that multiple app_contexts are being provided that are a mix of 32/64 bit binaries (default: false)",
                                false, false, (int) false, &value);
    orte_hetero_apps = OPAL_INT_TO_BOOL(value);
    
    /* allow specification of the launch agent */
    mca_base_param_reg_string_name("orte", "launch_agent",
                                   "Command used to start processes on remote nodes (default: orted)",
                                   false, false, "orted", &orte_launch_agent);

    /* whether or not to require RM allocation */
    mca_base_param_reg_int_name("orte", "allocation_required",
                                "Whether or not an allocation by a resource manager is required [default: no]",
                                false, false, (int)false, &value);
    orte_allocation_required = OPAL_INT_TO_BOOL(value);
        
#endif /* ORTE_DISABLE_FULL_SUPPORT */
    
    return ORTE_SUCCESS;
}
