/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2007-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      FUJITSU LIMITED.  All rights reserved.
 * Copyright (c) 2017      IBM Corporation. All rights reserved.
 * Copyright (c) 2018      Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "constants.h"
#include "src/runtime/prte_globals.h"
#include "src/util/error.h"
#include "src/util/pmix_printf.h"
#include "src/util/proc_info.h"
#include "src/util/pmix_string_copy.h"

const char *prte_strerror(int errnum)
{
    const char *retval;

    switch (errnum) {
    case PRTE_SUCCESS:
        retval = "Success";
        break;
    case PRTE_ERROR:
        retval = "Error";
        break;
    case PRTE_ERR_OUT_OF_RESOURCE:
        retval = "Out of resource";
        break;
    case PRTE_ERR_TEMP_OUT_OF_RESOURCE:
        retval = "Temporarily out of resource";
        break;
    case PRTE_ERR_RESOURCE_BUSY:
        retval = "Resource busy";
        break;
    case PRTE_ERR_BAD_PARAM:
        retval = "Bad parameter";
        break;
    case PRTE_ERR_FATAL:
        retval = "Fatal";
        break;
    case PRTE_ERR_NOT_IMPLEMENTED:
        retval = "Not implemented";
        break;
    case PRTE_ERR_NOT_SUPPORTED:
        retval = "Not supported";
        break;
    case PRTE_ERR_INTERRUPTED:
        retval = "Interrupted";
        break;
    case PRTE_ERR_WOULD_BLOCK:
        retval = "Would block";
        break;
    case PRTE_ERR_IN_ERRNO:
        retval = "In errno";
        break;
    case PRTE_ERR_UNREACH:
        retval = "Unreachable";
        break;
    case PRTE_ERR_NOT_FOUND:
        retval = "Not found";
        break;
    case PRTE_EXISTS:
        retval = "Exists";
        break;
    case PRTE_ERR_TIMEOUT:
        retval = "Timeout";
        break;
    case PRTE_ERR_NOT_AVAILABLE:
        retval = "Not available";
        break;
    case PRTE_ERR_PERM:
        retval = "No permission";
        break;
    case PRTE_ERR_VALUE_OUT_OF_BOUNDS:
        retval = "Value out of bounds";
        break;
    case PRTE_ERR_FILE_READ_FAILURE:
        retval = "File read failure";
        break;
    case PRTE_ERR_FILE_WRITE_FAILURE:
        retval = "File write failure";
        break;
    case PRTE_ERR_FILE_OPEN_FAILURE:
        retval = "File open failure";
        break;
    case PRTE_ERR_PACK_MISMATCH:
        retval = "Pack data mismatch";
        break;
    case PRTE_ERR_PACK_FAILURE:
        retval = "Data pack failed";
        break;
    case PRTE_ERR_UNPACK_FAILURE:
        retval = "Data unpack failed";
        break;
    case PRTE_ERR_UNPACK_INADEQUATE_SPACE:
        retval = "Data unpack had inadequate space";
        break;
    case PRTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER:
        retval = "Data unpack would read past end of buffer";
        break;
    case PRTE_ERR_OPERATION_UNSUPPORTED:
        retval = "Requested operation is not supported on referenced data type";
        break;
    case PRTE_ERR_UNKNOWN_DATA_TYPE:
        retval = "Unknown data type";
        break;
    case PRTE_ERR_BUFFER:
        retval = "Buffer type (described vs non-described) mismatch - operation not allowed";
        break;
    case PRTE_ERR_DATA_TYPE_REDEF:
        retval = "Attempt to redefine an existing data type";
        break;
    case PRTE_ERR_DATA_OVERWRITE_ATTEMPT:
        retval = "Attempt to overwrite a data value";
        break;
    case PRTE_ERR_MODULE_NOT_FOUND:
        retval = "Framework requires at least one active module, but none found";
        break;
    case PRTE_ERR_TOPO_SLOT_LIST_NOT_SUPPORTED:
        retval = "OS topology does not support slot_list process affinity";
        break;
    case PRTE_ERR_TOPO_SOCKET_NOT_SUPPORTED:
        retval = "Could not obtain socket topology information";
        break;
    case PRTE_ERR_TOPO_CORE_NOT_SUPPORTED:
        retval = "Could not obtain core topology information";
        break;
    case PRTE_ERR_NOT_ENOUGH_SOCKETS:
        retval = "Not enough sockets to meet request";
        break;
    case PRTE_ERR_NOT_ENOUGH_CORES:
        retval = "Not enough cores to meet request";
        break;
    case PRTE_ERR_INVALID_PHYS_CPU:
        retval = "Invalid physical cpu number returned";
        break;
    case PRTE_ERR_MULTIPLE_AFFINITIES:
        retval = "Multiple methods for assigning process affinity were specified";
        break;
    case PRTE_ERR_SLOT_LIST_RANGE:
        retval = "Provided slot_list range is invalid";
        break;
    case PRTE_ERR_NETWORK_NOT_PARSEABLE:
        retval = "Provided network specification is not parseable";
        break;
    case PRTE_ERR_NOT_INITIALIZED:
        retval = "Not initialized";
        break;
    case PRTE_ERR_NOT_BOUND:
        retval = "Not bound";
        break;
    case PRTE_ERR_PROC_ENTRY_NOT_FOUND:
        retval = "Database entry not found";
        break;
    case PRTE_ERR_DATA_VALUE_NOT_FOUND:
        retval = "Data for specified key not found";
        break;
    case PRTE_ERR_CONNECTION_FAILED:
        retval = "Connection failed";
        break;
    case PRTE_ERR_AUTHENTICATION_FAILED:
        retval = "Authentication failed";
        break;
    case PRTE_ERR_COMM_FAILURE:
        retval = "Comm failure";
        break;
    case PRTE_ERR_SERVER_NOT_AVAIL:
        retval = "Server not available";
        break;
    case PRTE_ERR_IN_PROCESS:
        retval = "Operation in process";
        break;
    case PRTE_ERR_DEBUGGER_RELEASE:
        retval = "Release debugger";
        break;
    case PRTE_ERR_HANDLERS_COMPLETE:
        retval = "Event handlers complete";
        break;
    case PRTE_ERR_PARTIAL_SUCCESS:
        retval = "Partial success";
        break;
    case PRTE_ERR_PROC_ABORTED:
        retval = "Process abnormally terminated";
        break;
    case PRTE_ERR_PROC_REQUESTED_ABORT:
        retval = "Process requested abort";
        break;
    case PRTE_ERR_PROC_ABORTING:
        retval = "Process is aborting";
        break;
    case PRTE_ERR_NODE_DOWN:
        retval = "Node has gone down";
        break;
    case PRTE_ERR_NODE_OFFLINE:
        retval = "Node has gone offline";
        break;
    case PRTE_ERR_JOB_TERMINATED:
        retval = "Job terminated";
        break;
    case PRTE_ERR_PROC_RESTART:
        retval = "Process restarted";
        break;
    case PRTE_ERR_PROC_CHECKPOINT:
        retval = "Process checkpoint";
        break;
    case PRTE_ERR_PROC_MIGRATE:
        retval = "Process migrate";
        break;
    case PRTE_ERR_EVENT_REGISTRATION:
        retval = "Event registration";
        break;
    case PRTE_ERR_HEARTBEAT_ALERT:
        retval = "Heartbeat not received";
        break;
    case PRTE_ERR_FILE_ALERT:
        retval = "File alert - proc may have stalled";
        break;
    case PRTE_ERR_RECV_LESS_THAN_POSTED:
        retval = "Receive was less than posted size";
        break;
    case PRTE_ERR_RECV_MORE_THAN_POSTED:
        retval = "Receive was greater than posted size";
        break;
    case PRTE_ERR_NO_MATCH_YET:
        retval = "No match for receive posted";
        break;
    case PRTE_ERR_REQUEST:
        retval = "Request error";
        break;
    case PRTE_ERR_NO_CONNECTION_ALLOWED:
        retval = "No connection allowed";
        break;
    case PRTE_ERR_CONNECTION_REFUSED:
        retval = "Connection refused";
        break;
    case PRTE_ERR_TYPE_MISMATCH:
        retval = "Type mismatch";
        break;
    case PRTE_ERR_COMPARE_FAILURE:
        retval = "Data comparison failure";
        break;
    case PRTE_ERR_COPY_FAILURE:
        retval = "Data copy failure";
        break;
    case PRTE_ERR_PROC_STATE_MISSING:
        retval = "The process state information is missing on the registry";
        break;
    case PRTE_ERR_PROC_EXIT_STATUS_MISSING:
        retval = "The process exit status is missing on the registry";
        break;
    case PRTE_ERR_INDETERMINATE_STATE_INFO:
        retval = "Request for state returned multiple responses";
        break;
    case PRTE_ERR_NODE_FULLY_USED:
        retval = "All the slots on a given node have been used";
        break;
    case PRTE_ERR_INVALID_NUM_PROCS:
        retval = "Multiple applications were specified, but at least one failed to specify the "
                 "number of processes to run";
        break;
    case PRTE_ERR_SILENT:
        if (prte_report_silent_errors) {
            retval = "Silent error";
        } else {
            retval = "";
        }
        break;
    case PRTE_ERR_ADDRESSEE_UNKNOWN:
        retval = "A message is attempting to be sent to a process whose contact information is "
                 "unknown";
        break;
    case PRTE_ERR_SYS_LIMITS_PIPES:
        retval = "The system limit on number of pipes a process can open was reached";
        break;
    case PRTE_ERR_PIPE_SETUP_FAILURE:
        retval = "A pipe could not be setup between a daemon and one of its local processes";
        break;
    case PRTE_ERR_SYS_LIMITS_CHILDREN:
        retval = "The system limit on number of children a process can have was reached";
        break;
    case PRTE_ERR_FAILED_GET_TERM_ATTRS:
        retval = "The I/O forwarding system was unable to get the attributes of your terminal";
        break;
    case PRTE_ERR_WDIR_NOT_FOUND:
        retval = "The specified working directory could not be found";
        break;
    case PRTE_ERR_EXE_NOT_FOUND:
        retval = "The specified executable could not be found";
        break;
    case PRTE_ERR_PIPE_READ_FAILURE:
        retval = "A pipe could not be read";
        break;
    case PRTE_ERR_EXE_NOT_ACCESSIBLE:
        retval = "The specified executable could not be executed";
        break;
    case PRTE_ERR_FAILED_TO_START:
        retval = "The specified application failed to start";
        break;
    case PRTE_ERR_FILE_NOT_EXECUTABLE:
        retval = "A system-required executable either could not be found or was not executable by "
                 "this user";
        break;
    case PRTE_ERR_HNP_COULD_NOT_START:
        retval = "Unable to start a daemon on the local node";
        break;
    case PRTE_ERR_SYS_LIMITS_SOCKETS:
        retval = "The system limit on number of network connections a process can open was reached";
        break;
    case PRTE_ERR_SOCKET_NOT_AVAILABLE:
        retval = "Unable to open a TCP socket for out-of-band communications";
        break;
    case PRTE_ERR_SYSTEM_WILL_BOOTSTRAP:
        retval = "System will determine resources during bootstrap of daemons";
        break;
    case PRTE_ERR_RESTART_LIMIT_EXCEEDED:
        retval = "Limit on number of process restarts was exceeded";
        break;
    case PRTE_ERR_INVALID_NODE_RANK:
        retval = "Invalid node rank";
        break;
    case PRTE_ERR_INVALID_LOCAL_RANK:
        retval = "Invalid local rank";
        break;
    case PRTE_ERR_UNRECOVERABLE:
        retval = "Unrecoverable error";
        break;
    case PRTE_ERR_MEM_LIMIT_EXCEEDED:
        retval = "Memory limit exceeded";
        break;
    case PRTE_ERR_HEARTBEAT_LOST:
        retval = "Heartbeat lost";
        break;
    case PRTE_ERR_PROC_STALLED:
        retval = "Proc appears to be stalled";
        break;
    case PRTE_ERR_NO_APP_SPECIFIED:
        retval = "No application specified";
        break;
    case PRTE_ERR_NO_EXE_SPECIFIED:
        retval = "No executable specified";
        break;
    case PRTE_ERR_COMM_DISABLED:
        retval = "Communications have been disabled";
        break;
    case PRTE_ERR_FAILED_TO_MAP:
        retval = "Unable to map job";
        break;
    case PRTE_ERR_TAKE_NEXT_OPTION:
        if (prte_report_silent_errors) {
            retval = "Next option";
        } else {
            retval = "";
        }
        break;
    case PRTE_ERR_SENSOR_LIMIT_EXCEEDED:
        retval = "Sensor limit exceeded";
        break;
    case PRTE_ERR_ALLOCATION_PENDING:
        retval = "Allocation pending";
        break;
    case PRTE_ERR_NO_PATH_TO_TARGET:
        retval = "No OOB path to target";
        break;
    case PRTE_ERR_OP_IN_PROGRESS:
        retval = "Operation in progress";
        break;
    case PRTE_ERR_OPEN_CONDUIT_FAIL:
        retval = "Open messaging conduit failed";
        break;
    case PRTE_ERR_OUT_OF_ORDER_MSG:
        retval = "Out of order message";
        break;
    case PRTE_ERR_FORCE_SELECT:
        retval = "Force select";
        break;
    case PRTE_ERR_JOB_CANCELLED:
        retval = "Job cancelled";
        break;
    case PRTE_ERR_CONDUIT_SEND_FAIL:
        retval = " Transport Conduit returned send error";
        break;
    default:
        retval = "Unknown error";
    }
    return retval;
}
