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

#include "runtime/runtime.h"

/**
 * Initialze and setup a process in the ORTE.
 *
 * @retval ORTE_SUCCESS Upon success.
 * @retval ORTE_ERROR Upon failure.
 */

/* globals used by RTE */
int orte_debug_flag=(int)false;

int orte_init(void)
{
    int rc;

    if (ORTE_SUCCESS != (rc = orte_init_stage1())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_init_stage2())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ORTE_SUCCESS;
}

/* This array is provided so that users can output an intelligible name for a data
 * type during debugging. It is called by the ORTE_DATA_NAME macro defined in
 * include/orte_names.h. For example, you can call it as:
 * ompi_output(0, "data type: %s", ORTE_DATA_NAME(keyval.type));
 * 
 * THE CORRESPONDING MACRO IS DEFINED IN include/orte_schema.h
 * AS IS THE EXTERN STATEMENT FOR ACCESSING THIS ARRAY
 */

char *orte_data_strings[] = {
    "DATA_TYPE_NOT_DEFINED",
    "ORTE_BYTE",
    "ORTE_BOOL",
    "ORTE_STRING",
    "ORTE_SIZE",
    "ORTE_PID",
    /* all the integer flavors */
    "ORTE_INT",
    "ORTE_INT8",
    "ORTE_INT16",
    "ORTE_INT32",
    "ORTE_INT64",
    /* all the unsigned integer flavors */
    "ORTE_UINT",
    "ORTE_UINT8",
    "ORTE_UINT16",
    "ORTE_UINT32",
    "ORTE_UINT64",
    /* all the floating point flavors */
    "ORTE_FLOAT",
    "ORTE_FLOAT4",
    "ORTE_DOUBLE",
    "ORTE_FLOAT8",
    "ORTE_LONG_DOUBLE",
    "ORTE_FLOAT12",
    "ORTE_FLOAT16",
    /* orte-specific typedefs */
    "ORTE_NAME",
    "ORTE_VPID",
    "ORTE_JOBID",
    "ORTE_CELLID",
    "ORTE_NODE_STATE",
    "ORTE_PROC_STATE",
    "ORTE_EXIT_CODE",
    "ORTE_BYTE_OBJECT",
    "ORTE_KEYVAL",
    "ORTE_NOTIFY_ACTION",
    "ORTE_GPR_CMD",
    "ORTE_GPR_NOTIFY_ID",
    "ORTE_GPR_VALUE",
    "ORTE_DATA_TYPE",
    "ORTE_APP_CONTEXT",
    "ORTE_APP_CONTEXT_MAP",
    "ORTE_GPR_ADDR_MODE",
    "ORTE_GPR_SUBSCRIPTION",
    "ORTE_GPR_NOTIFY_DATA",
    "ORTE_NULL"
};


/*
 * Similar to the above, this array is used to output intelligible error
 * messages. It is disturbing to think that we are still outputing error numbers and
 * expecting users to look them up in the "big book" to find out what they represent.
 * This array allows the user to output an actual string representation of the error.
 *
 * THE CORRESPONDING MACRO IS DEFINED IN include/orte_schema.h
 * AS IS THE EXTERN STATEMENT FOR ACCESSING THIS ARRAY
 */

char *orte_error_strings[] = {
    "ORTE_SUCCESS",
    "ORTE_ERROR",
    "ORTE_ERR_OUT_OF_RESOURCE", /* fatal error */
    "ORTE_ERR_TEMP_OUT_OF_RESOURCE", /* try again later */
    "ORTE_ERR_RESOURCE_BUSY",
    "ORTE_ERR_BAD_PARAM",     /* equivalent to MPI_ERR_ARG error code */
    "ORTE_ERR_RECV_LESS_THAN_POSTED",
    "ORTE_ERR_RECV_MORE_THAN_POSTED",
    "ORTE_ERR_NO_MATCH_YET",
    "ORTE_ERR_FATAL",
    "ORTE_ERR_NOT_IMPLEMENTED",
    "ORTE_ERR_NOT_SUPPORTED",
    "ORTE_ERR_INTERUPTED",
    "ORTE_ERR_WOULD_BLOCK",
    "ORTE_ERR_IN_ERRNO",
    "ORTE_ERR_UNREACH",
    "ORTE_ERR_NOT_FOUND",
    "ORTE_ERR_BUFFER", /* equivalent to MPI_ERR_BUFFER */
    "ORTE_ERR_REQUEST", /* equivalent to MPI_ERR_REQUEST */
    "ORTE_EXISTS",  /* indicates that the specified object already exists */
    "ORTE_ERR_NO_CONNECTION_ALLOWED", /* indicates that the receiving process does not allow connections */
    "ORTE_ERR_CONNECTION_REFUSED", /* contact made with process, but it refuses any further communication */
    "ORTE_ERR_CONNECTION_FAILED",  /* message sent, but delivery failed */
    "ORTE_ERR_TIMEOUT",
    "ORTE_STARTUP_DETECTED",
    "ORTE_SHUTDOWN_DETECTED",
    "ORTE_PROC_STARTING",
    "ORTE_PROC_STOPPED",
    "ORTE_PROC_TERMINATING",
    "ORTE_PROC_ALIVE",
    "ORTE_PROC_RUNNING",
    "ORTE_PROC_KILLED",
    "ORTE_PROC_EXITED",
    "ORTE_NODE_UP",
    "ORTE_NODE_DOWN",
    "ORTE_NODE_BOOTING",
    "ORTE_NODE_ERROR",
    "ORTE_PACK_MISMATCH",
    "ORTE_ERR_PACK_FAILURE",
    "ORTE_ERR_UNPACK_FAILURE",
    "ORTE_ERR_COMM_FAILURE",
    "ORTE_UNPACK_INADEQUATE_SPACE",
    "ORTE_UNPACK_READ_PAST_END_OF_BUFFER",
    "ORTE_ERR_NOT_AVAILABLE",
    "ORTE_ERR_GPR_DATA_CORRUPT",
    "ORTE_ERR_PERM",
    "ORTE_ERR_TYPE_MISMATCH"
};

