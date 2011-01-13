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
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file **/

#include "orte_config.h"
#include "orte/constants.h"

#include <stdio.h>

#include "opal/util/opal_sos.h"
#include "orte/util/error_strings.h"
#include "orte/runtime/orte_globals.h"

const char *orte_err2str(int errnum)
{
    const char *retval;
    switch (OPAL_SOS_GET_ERROR_CODE(errnum)) {
    case ORTE_ERR_RECV_LESS_THAN_POSTED:
        retval = "Receive was less than posted size";
        break;
    case ORTE_ERR_RECV_MORE_THAN_POSTED:
        retval = "Receive was greater than posted size";
        break;
    case ORTE_ERR_NO_MATCH_YET:
        retval = "No match for receive posted";
        break;
    case ORTE_ERR_REQUEST:
        retval = "Request error";
        break;
    case ORTE_ERR_NO_CONNECTION_ALLOWED:
        retval = "No connection allowed";
        break;
    case ORTE_ERR_CONNECTION_REFUSED:
        retval = "Connection refused";
        break;
    case ORTE_ERR_CONNECTION_FAILED:
        retval = "Connection failed";
        break;
    case ORTE_ERR_COMM_FAILURE:
        retval = "Communication failure";
        break;
    case ORTE_ERR_TYPE_MISMATCH:
        retval = "Type mismatch";
        break;
    case ORTE_ERR_COMPARE_FAILURE:
        retval = "Data comparison failure";
        break;
    case ORTE_ERR_COPY_FAILURE:
        retval = "Data copy failure";
        break;
    case ORTE_ERR_PROC_STATE_MISSING:
        retval = "The process state information is missing on the registry";
        break;
    case ORTE_ERR_PROC_EXIT_STATUS_MISSING:
        retval = "The process exit status is missing on the registry";
        break;
    case ORTE_ERR_INDETERMINATE_STATE_INFO:
        retval = "Request for state returned multiple responses";
        break;
    case ORTE_ERR_NODE_FULLY_USED:
        retval = "All the slots on a given node have been used";
        break;
    case ORTE_ERR_INVALID_NUM_PROCS:
        retval = "Multiple applications were specified, but at least one failed to specify the number of processes to run";
        break;
    case ORTE_ERR_SILENT:
        if (orte_report_silent_errors) {
            retval = "Silent error";
        } else {
            retval = NULL;
        }
        break;
    case ORTE_ERR_ADDRESSEE_UNKNOWN:
        retval = "A message is attempting to be sent to a process whose contact information is unknown";
        break;
    case ORTE_ERR_SYS_LIMITS_PIPES:
        retval = "The system limit on number of pipes a process can open was reached";
        break;
    case ORTE_ERR_PIPE_SETUP_FAILURE:
        retval = "A pipe could not be setup between a daemon and one of its local processes";
        break;
    case ORTE_ERR_SYS_LIMITS_CHILDREN:
        retval = "The system limit on number of children a process can have was reached";
        break;
    case ORTE_ERR_FAILED_GET_TERM_ATTRS:
        retval = "The I/O forwarding system was unable to get the attributes of your terminal";
        break;
    case ORTE_ERR_WDIR_NOT_FOUND:
        retval = "The specified working directory could not be found";
        break;
    case ORTE_ERR_EXE_NOT_FOUND:
        retval = "The specified executable could not be found";
        break;
    case ORTE_ERR_PIPE_READ_FAILURE:
        retval = "A pipe could not be read";
        break;
    case ORTE_ERR_EXE_NOT_ACCESSIBLE:
        retval = "The specified executable could not be executed";
        break;
    case ORTE_ERR_FAILED_TO_START:
        retval = "The specified application failed to start";
        break;
    case ORTE_ERR_FILE_NOT_EXECUTABLE:
        retval = "A system-required executable either could not be found or was not executable by this user";
        break;
    case ORTE_ERR_HNP_COULD_NOT_START:
        retval = "Unable to start a daemon on the local node";
        break;
    case ORTE_ERR_SYS_LIMITS_SOCKETS:
        retval = "The system limit on number of network connections a process can open was reached";
        break;
    case ORTE_ERR_SOCKET_NOT_AVAILABLE:
        retval = "Unable to open a TCP socket for out-of-band communications";
        break;
    case ORTE_ERR_SYSTEM_WILL_BOOTSTRAP:
        retval = "System will determine resources during bootstrap of daemons";
        break;
    case ORTE_ERR_RELOCATE_LIMIT_EXCEEDED:
        retval = "Limit on number of process relocations was exceeded";
        break;
    case ORTE_ERR_UNRECOVERABLE:
        retval = "Unrecoverable error";
        break;
    case ORTE_ERR_NO_APP_SPECIFIED:
        retval = "No application specified";
        break;
    case ORTE_ERR_NO_EXE_SPECIFIED:
        retval = "No executable specified";
        break;
    case ORTE_ERR_COMM_DISABLED:
        retval = "Communications have been disabled";
        break;
            
    default:
        if (orte_report_silent_errors) {
            retval = "Unknown error";
        } else {
            retval = NULL;
        }
    }

    return retval;
}

const char *orte_job_state_to_str(orte_job_state_t state)
{
    switch(state) {
        case ORTE_JOB_STATE_UNDEF:
            return "UNDEFINED";
        case ORTE_JOB_STATE_INIT:
            return "INITIALIZED";
        case ORTE_JOB_STATE_RESTART:
            return "RESTARTING";
        case ORTE_JOB_STATE_LAUNCHED:
            return "LAUNCHED";
        case ORTE_JOB_STATE_RUNNING:
            return "RUNNING";
        case ORTE_JOB_STATE_SUSPENDED:
            return "SUSPENDED";
        case ORTE_JOB_STATE_REGISTERED:
            return "SYNC REGISTERED";
        case ORTE_JOB_STATE_UNTERMINATED:
            return "UNTERMINATED";
        case ORTE_JOB_STATE_TERMINATED:
            return "NORMALLY TERMINATED";
        case ORTE_JOB_STATE_ABORTED:
            return "ABORTED";
        case ORTE_JOB_STATE_FAILED_TO_START:
            return "FAILED TO START";
        case ORTE_JOB_STATE_ABORTED_BY_SIG:
            return "ABORTED BY SIGNAL";
        case ORTE_JOB_STATE_ABORTED_WO_SYNC:
            return "TERMINATED WITHOUT SYNC";
        case ORTE_JOB_STATE_KILLED_BY_CMD:
            return "KILLED BY INTERNAL COMMAND";
        case ORTE_JOB_STATE_COMM_FAILED:
            return "COMMUNICATION FAILURE";
        case ORTE_JOB_STATE_SENSOR_BOUND_EXCEEDED:
            return "SENSOR BOUND EXCEEDED";
            break;
        case ORTE_JOB_STATE_NEVER_LAUNCHED:
            return "NEVER LAUNCHED";
        case ORTE_JOB_STATE_ABORT_ORDERED:
            return "ABORT IN PROGRESS";
        case ORTE_JOB_STATE_HEARTBEAT_FAILED:
            return "HEARTBEAT FAILED";
        default:
            return "UNKNOWN STATE!";
    }
}

const char *orte_proc_state_to_str(orte_proc_state_t state)
{
    switch(state) {
        case ORTE_PROC_STATE_UNDEF:
            return "UNDEFINED";
        case ORTE_PROC_STATE_INIT:
            return "INITIALIZED";
        case ORTE_PROC_STATE_RESTART:
            return "RESTARTING";
        case ORTE_PROC_STATE_LAUNCHED:
            return "LAUNCHED";
        case ORTE_PROC_STATE_RUNNING:
            return "RUNNING";
        case ORTE_PROC_STATE_REGISTERED:
            return "SYNC REGISTERED";
        case ORTE_PROC_STATE_UNTERMINATED:
            return "UNTERMINATED";
        case ORTE_PROC_STATE_TERMINATED:
            return "NORMALLY TERMINATED";
        case ORTE_PROC_STATE_ABORTED:
            return "ABORTED";
        case ORTE_PROC_STATE_FAILED_TO_START:
            return "FAILED TO START";
        case ORTE_PROC_STATE_ABORTED_BY_SIG:
            return "ABORTED BY SIGNAL";
        case ORTE_PROC_STATE_TERM_WO_SYNC:
            return "TERMINATED WITHOUT SYNC";
        case ORTE_PROC_STATE_KILLED_BY_CMD:
            return "KILLED BY INTERNAL COMMAND";
        case ORTE_PROC_STATE_COMM_FAILED:
            return "COMMUNICATION FAILURE";
        case ORTE_PROC_STATE_SENSOR_BOUND_EXCEEDED:
            return "SENSOR BOUND EXCEEDED";
            break;
        case ORTE_PROC_STATE_HEARTBEAT_FAILED:
            return "HEARTBEAT FAILED";
            break;
        default:
            return "UNKNOWN STATE!";
    }
}

