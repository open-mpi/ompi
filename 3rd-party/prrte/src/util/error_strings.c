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
 * Copyright (c) 2010-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file **/

#include "prte_config.h"
#include "constants.h"

#include <stdio.h>
#ifdef HAVE_SYS_SIGNAL_H
#    include <sys/signal.h>
#else
#    include <signal.h>
#endif

#include "src/mca/plm/plm_types.h"
#include "src/runtime/prte_globals.h"
#include "src/util/error_strings.h"

const char *prte_job_state_to_str(prte_job_state_t state)
{
    switch (state) {
    case PRTE_JOB_STATE_UNDEF:
        return "UNDEFINED";
    case PRTE_JOB_STATE_INIT:
        return "PENDING INIT";
    case PRTE_JOB_STATE_INIT_COMPLETE:
        return "INIT_COMPLETE";
    case PRTE_JOB_STATE_ALLOCATE:
        return "PENDING ALLOCATION";
    case PRTE_JOB_STATE_ALLOCATION_COMPLETE:
        return "ALLOCATION COMPLETE";
    case PRTE_JOB_STATE_MAP:
        return "PENDING MAPPING";
    case PRTE_JOB_STATE_MAP_COMPLETE:
        return "MAP COMPLETE";
    case PRTE_JOB_STATE_SYSTEM_PREP:
        return "PENDING FINAL SYSTEM PREP";
    case PRTE_JOB_STATE_LAUNCH_DAEMONS:
        return "PENDING DAEMON LAUNCH";
    case PRTE_JOB_STATE_DAEMONS_LAUNCHED:
        return "DAEMONS LAUNCHED";
    case PRTE_JOB_STATE_DAEMONS_REPORTED:
        return "ALL DAEMONS REPORTED";
    case PRTE_JOB_STATE_VM_READY:
        return "VM READY";
    case PRTE_JOB_STATE_LAUNCH_APPS:
        return "PENDING APP LAUNCH";
    case PRTE_JOB_STATE_SEND_LAUNCH_MSG:
        return "SENDING LAUNCH MSG";
    case PRTE_JOB_STATE_RUNNING:
        return "RUNNING";
    case PRTE_JOB_STATE_SUSPENDED:
        return "SUSPENDED";
    case PRTE_JOB_STATE_REGISTERED:
        return "SYNC REGISTERED";
    case PRTE_JOB_STATE_LOCAL_LAUNCH_COMPLETE:
        return "LOCAL LAUNCH COMPLETE";
    case PRTE_JOB_STATE_READY_FOR_DEBUG:
        return "READY FOR DEBUG";
    case PRTE_JOB_STATE_STARTED:
        return "JOB STARTED";
    case PRTE_JOB_STATE_UNTERMINATED:
        return "UNTERMINATED";
    case PRTE_JOB_STATE_TERMINATED:
        return "NORMALLY TERMINATED";
    case PRTE_JOB_STATE_NOTIFY_COMPLETED:
        return "NOTIFY COMPLETED";
    case PRTE_JOB_STATE_NOTIFIED:
        return "NOTIFIED";
    case PRTE_JOB_STATE_ALL_JOBS_COMPLETE:
        return "ALL JOBS COMPLETE";
    case PRTE_JOB_STATE_ERROR:
        return "ARTIFICIAL BOUNDARY - ERROR";
    case PRTE_JOB_STATE_KILLED_BY_CMD:
        return "KILLED BY INTERNAL COMMAND";
    case PRTE_JOB_STATE_ABORTED:
        return "ABORTED";
    case PRTE_JOB_STATE_FAILED_TO_START:
        return "FAILED TO START";
    case PRTE_JOB_STATE_ABORTED_BY_SIG:
        return "ABORTED BY SIGNAL";
    case PRTE_JOB_STATE_ABORTED_WO_SYNC:
        return "TERMINATED WITHOUT SYNC";
    case PRTE_JOB_STATE_COMM_FAILED:
        return "COMMUNICATION FAILURE";
    case PRTE_JOB_STATE_SENSOR_BOUND_EXCEEDED:
        return "SENSOR BOUND EXCEEDED";
    case PRTE_JOB_STATE_CALLED_ABORT:
        return "PROC CALLED ABORT";
    case PRTE_JOB_STATE_HEARTBEAT_FAILED:
        return "HEARTBEAT FAILED";
    case PRTE_JOB_STATE_NEVER_LAUNCHED:
        return "NEVER LAUNCHED";
    case PRTE_JOB_STATE_ABORT_ORDERED:
        return "ABORT IN PROGRESS";
    case PRTE_JOB_STATE_NON_ZERO_TERM:
        return "AT LEAST ONE PROCESS EXITED WITH NON-ZERO STATUS";
    case PRTE_JOB_STATE_FAILED_TO_LAUNCH:
        return "FAILED TO LAUNCH";
    case PRTE_JOB_STATE_FORCED_EXIT:
        return "FORCED EXIT";
    case PRTE_JOB_STATE_DAEMONS_TERMINATED:
        return "DAEMONS TERMINATED";
    case PRTE_JOB_STATE_SILENT_ABORT:
        return "ERROR REPORTED ELSEWHERE";
    case PRTE_JOB_STATE_REPORT_PROGRESS:
        return "REPORT PROGRESS";
    case PRTE_JOB_STATE_ALLOC_FAILED:
        return "ALLOCATION FAILED";
    case PRTE_JOB_STATE_MAP_FAILED:
        return "MAP FAILED";
    case PRTE_JOB_STATE_CANNOT_LAUNCH:
        return "CANNOT LAUNCH";
    case PRTE_JOB_STATE_FILES_POSN_FAILED:
        return "FILE PREPOSITION FAILED";
    case PRTE_JOB_STATE_FT_CHECKPOINT:
        return "FAULT TOLERANCE CHECKPOINT";
    case PRTE_JOB_STATE_FT_CONTINUE:
        return "FAULT TOLERANCE CONTINUE";
    case PRTE_JOB_STATE_FT_RESTART:
        return "FAULT TOLERANCE RESTART";
    case PRTE_JOB_STATE_ANY:
        return "ANY";
    default:
        return "UNKNOWN STATE!";
    }
}

const char *prte_app_ctx_state_to_str(prte_app_state_t state)
{
    switch (state) {
    case PRTE_APP_STATE_UNDEF:
        return "UNDEFINED";
    case PRTE_APP_STATE_INIT:
        return "PENDING INIT";
    case PRTE_APP_STATE_ALL_MAPPED:
        return "ALL MAPPED";
    case PRTE_APP_STATE_RUNNING:
        return "RUNNING";
    case PRTE_APP_STATE_COMPLETED:
        return "COMPLETED";
    default:
        return "UNKNOWN STATE!";
    }
}

const char *prte_proc_state_to_str(prte_proc_state_t state)
{
    switch (state) {
    case PRTE_PROC_STATE_UNDEF:
        return "UNDEFINED";
    case PRTE_PROC_STATE_INIT:
        return "INITIALIZED";
    case PRTE_PROC_STATE_RESTART:
        return "RESTARTING";
    case PRTE_PROC_STATE_TERMINATE:
        return "MARKED FOR TERMINATION";
    case PRTE_PROC_STATE_RUNNING:
        return "RUNNING";
    case PRTE_PROC_STATE_REGISTERED:
        return "SYNC REGISTERED";
    case PRTE_PROC_STATE_IOF_COMPLETE:
        return "IOF COMPLETE";
    case PRTE_PROC_STATE_WAITPID_FIRED:
        return "WAITPID FIRED";
    case PRTE_PROC_STATE_MODEX_READY:
        return "MODEX READY";
    case PRTE_PROC_STATE_READY_FOR_DEBUG:
        return "READY FOR DEBUG";
    case PRTE_PROC_STATE_UNTERMINATED:
        return "UNTERMINATED";
    case PRTE_PROC_STATE_TERMINATED:
        return "NORMALLY TERMINATED";
    case PRTE_PROC_STATE_ERROR:
        return "ARTIFICIAL BOUNDARY - ERROR";
    case PRTE_PROC_STATE_KILLED_BY_CMD:
        return "KILLED BY INTERNAL COMMAND";
    case PRTE_PROC_STATE_ABORTED:
        return "ABORTED";
    case PRTE_PROC_STATE_FAILED_TO_START:
        return "FAILED TO START";
    case PRTE_PROC_STATE_ABORTED_BY_SIG:
        return "ABORTED BY SIGNAL";
    case PRTE_PROC_STATE_TERM_WO_SYNC:
        return "TERMINATED WITHOUT SYNC";
    case PRTE_PROC_STATE_COMM_FAILED:
        return "COMMUNICATION FAILURE";
    case PRTE_PROC_STATE_SENSOR_BOUND_EXCEEDED:
        return "SENSOR BOUND EXCEEDED";
    case PRTE_PROC_STATE_CALLED_ABORT:
        return "CALLED ABORT";
    case PRTE_PROC_STATE_HEARTBEAT_FAILED:
        return "HEARTBEAT FAILED";
    case PRTE_PROC_STATE_MIGRATING:
        return "MIGRATING";
    case PRTE_PROC_STATE_CANNOT_RESTART:
        return "CANNOT BE RESTARTED";
    case PRTE_PROC_STATE_TERM_NON_ZERO:
        return "EXITED WITH NON-ZERO STATUS";
    case PRTE_PROC_STATE_FAILED_TO_LAUNCH:
        return "FAILED TO LAUNCH";
    case PRTE_PROC_STATE_UNABLE_TO_SEND_MSG:
        return "UNABLE TO SEND MSG";
    case PRTE_PROC_STATE_LIFELINE_LOST:
        return "LIFELINE LOST";
    case PRTE_PROC_STATE_NO_PATH_TO_TARGET:
        return "NO PATH TO TARGET";
    case PRTE_PROC_STATE_FAILED_TO_CONNECT:
        return "FAILED TO CONNECT";
    case PRTE_PROC_STATE_PEER_UNKNOWN:
        return "PEER UNKNOWN";
    case PRTE_PROC_STATE_ANY:
        return "ANY";
    default:
        return "UNKNOWN STATE!";
    }
}

const char *prte_node_state_to_str(prte_node_state_t state)
{
    switch (state) {
    case PRTE_NODE_STATE_UNDEF:
        return "UNDEF";
    case PRTE_NODE_STATE_UNKNOWN:
        return "UNKNOWN";
    case PRTE_NODE_STATE_DOWN:
        return "DOWN";
    case PRTE_NODE_STATE_UP:
        return "UP";
    case PRTE_NODE_STATE_REBOOT:
        return "REBOOT";
    case PRTE_NODE_STATE_DO_NOT_USE:
        return "DO_NOT_USE";
    case PRTE_NODE_STATE_NOT_INCLUDED:
        return "NOT_INCLUDED";
    case PRTE_NODE_STATE_ADDED:
        return "ADDED";
    default:
        return "UNKNOWN STATE!";
    }
}
