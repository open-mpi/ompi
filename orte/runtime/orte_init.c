/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "include/orte_constants.h"
#include "mca/errmgr/errmgr.h"

#include "runtime/opal.h"
#include "runtime/runtime.h"

/**
 * Initialze and setup a process in the ORTE.
 *
 * @retval ORTE_SUCCESS Upon success.
 * @retval ORTE_ERROR Upon failure.
 */

/* globals used by RTE */
int orte_debug_flag=(int)false;

static const char * orte_err2str(int errnum);

int orte_init(void)
{
    int rc;

    if (ORTE_SUCCESS != (rc = opal_init())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* register handler for errnum -> string converstion */
    opal_error_register(orte_err2str);
    
    if (ORTE_SUCCESS != (rc = orte_system_init())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ORTE_SUCCESS;
}

static const char *
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
    case ORTE_STARTUP_DETECTED:
        retval = "Startup detected";
        break;
    case ORTE_SHUTDOWN_DETECTED:
        retval = "Shutdown detected";
        break;
    case ORTE_PROC_STARTING:
        retval = "Proccess starting";
        break;
    case ORTE_PROC_STOPPED:
        retval = "Proccess stopped";
        break;
    case ORTE_PROC_TERMINATING:
        retval = "Proccess terminating";
        break;
    case ORTE_PROC_ALIVE:
        retval = "Proccess alive";
        break;
    case ORTE_PROC_RUNNING:
        retval = "Process running";
        break;
    case ORTE_PROC_KILLED:
        retval = "Process killed";
        break;
    case ORTE_PROC_EXITED:
        retval = "Process exited";
        break;
    case ORTE_NODE_UP:
        retval = "Node is up";
        break;
    case ORTE_NODE_DOWN:
        retval = "Node is down";
        break;
    case ORTE_NODE_BOOTING:
        retval = "Node is booting";
        break;
    case ORTE_NODE_ERROR:
        retval = "Node is in error condition";
        break;
    case ORTE_PACK_MISMATCH:
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
    case ORTE_UNPACK_INADEQUATE_SPACE:
        retval = "Data unpack had inadequate space";
        break;
    case ORTE_UNPACK_READ_PAST_END_OF_BUFFER:
        retval = "Data unpack would read past end of buffer";
        break;
    case ORTE_ERR_GPR_DATA_CORRUPT:
        retval = "GPR data corruption";
        break;
    case ORTE_ERR_TYPE_MISMATCH:
        retval = "Type mismatch";
        break;
    default: 
        retval = NULL;
    }

    return retval;
}

