/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef ORTE_CONSTANTS_H
#define ORTE_CONSTANTS_H

/* error codes
 *
 * ANY CHANGES TO THESE DEFINITIONS MUST BE REFLECTED IN THE TEXT ARRAY
 * orte_error_strings DEFINED IN src/runtime/orte_init.c.
 * 
 */
enum {
    ORTE_SUCCESS = 0,
    ORTE_ERROR = -1,
    ORTE_ERR_OUT_OF_RESOURCE = -2, /* fatal error */
    ORTE_ERR_TEMP_OUT_OF_RESOURCE = -3, /* try again later */
    ORTE_ERR_RESOURCE_BUSY = -4,
    ORTE_ERR_BAD_PARAM = -5,     /* equivalent to MPI_ERR_ARG error code */
    ORTE_ERR_RECV_LESS_THAN_POSTED = -6,
    ORTE_ERR_RECV_MORE_THAN_POSTED = -7,
    ORTE_ERR_NO_MATCH_YET = -8,
    ORTE_ERR_FATAL = -9,
    ORTE_ERR_NOT_IMPLEMENTED = -10,
    ORTE_ERR_NOT_SUPPORTED = -11,
    ORTE_ERR_INTERUPTED = -12,
    ORTE_ERR_WOULD_BLOCK = -13,
    ORTE_ERR_IN_ERRNO = -14,
    ORTE_ERR_UNREACH = -15,
    ORTE_ERR_NOT_FOUND = -16,
    ORTE_ERR_BUFFER = -17, /* equivalent to MPI_ERR_BUFFER */
    ORTE_ERR_REQUEST = -18, /* equivalent to MPI_ERR_REQUEST */
    ORTE_EXISTS = -19,  /* indicates that the specified object already exists */
    ORTE_ERR_NO_CONNECTION_ALLOWED = -20, /* indicates that the receiving process does not allow connections */
    ORTE_ERR_CONNECTION_REFUSED = -21, /* contact made with process, but it refuses any further communication */
    ORTE_ERR_CONNECTION_FAILED = -22,  /* message sent, but delivery failed */
    ORTE_ERR_TIMEOUT = -23,
    ORTE_STARTUP_DETECTED = -24,
    ORTE_SHUTDOWN_DETECTED = -25,
    ORTE_PROC_STARTING = -26,
    ORTE_PROC_STOPPED = -27,
    ORTE_PROC_TERMINATING = -28,
    ORTE_PROC_ALIVE = -29,
    ORTE_PROC_RUNNING = -30,
    ORTE_PROC_KILLED = -31,
    ORTE_PROC_EXITED = -32,
    ORTE_NODE_UP = -33,
    ORTE_NODE_DOWN = -34,
    ORTE_NODE_BOOTING = -35,
    ORTE_NODE_ERROR = -36,
    ORTE_PACK_MISMATCH = -37,
    ORTE_ERR_PACK_FAILURE = -38,
    ORTE_ERR_UNPACK_FAILURE = -39,
    ORTE_ERR_COMM_FAILURE = -40,
    ORTE_UNPACK_INADEQUATE_SPACE = -41,
    ORTE_UNPACK_READ_PAST_END_OF_BUFFER = -42,
    ORTE_ERR_NOT_AVAILABLE = -43,
    ORTE_ERR_GPR_DATA_CORRUPT = -44,
    ORTE_ERR_PERM /* no permission */
};

#endif /* ORTE_CONSTANTS_H */

