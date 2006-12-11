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

/** @file **/

#include "orte_config.h"

#include "orte/orte_constants.h"
#include <stdio.h>

const char *
orte_err2str(int errnum)
{
    const char *retval;
    switch (errnum) {
    case ORTE_ERR_RECV_LESS_THAN_POSTED:
        retval = "Receive was less than posted size";
        break;
    case ORTE_ERR_RECV_MORE_THAN_POSTED:
        retval = "Receive was greater than posted size";
        break;
    case ORTE_ERR_NO_MATCH_YET:
        retval = "No match for receive posted";
        break;
    case ORTE_ERR_BUFFER:
        retval = "Buffer error";
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
    case ORTE_ERR_PACK_MISMATCH:
        retval = "Pack data mismatch";
        break;
    case ORTE_ERR_PACK_FAILURE:
        retval = "Data pack failed";
        break;
    case ORTE_ERR_UNPACK_FAILURE:
        retval = "Data unpack failed";
        break;
    case ORTE_ERR_COMM_FAILURE:
        retval = "Communication failure";
        break;
    case ORTE_ERR_UNPACK_INADEQUATE_SPACE:
        retval = "Data unpack had inadequate space";
        break;
    case ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER:
        retval = "Data unpack would read past end of buffer";
        break;
    case ORTE_ERR_GPR_DATA_CORRUPT:
        retval = "GPR data corruption";
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
    case ORTE_ERR_UNKNOWN_DATA_TYPE:
        retval = "Unknown data type";
        break;
    case ORTE_ERR_DATA_TYPE_REDEF:
        retval = "Attempt to redefine an existing data type";
        break;
    case ORTE_ERR_DATA_OVERWRITE_ATTEMPT:
        retval = "Attempt to overwrite a data value";
        break;
    case ORTE_ERR_OPERATION_UNSUPPORTED:
        retval = "Requested operation is not supported on referenced data type";
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
        retval = NULL;
        break;
    default:
        retval = NULL;
    }

    return retval;
}
