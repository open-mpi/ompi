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

#ifndef OMPI_CONSTANTS_H
#define OMPI_CONSTANTS_H

/* error codes */
enum {
    OMPI_SUCCESS = 0,
    OMPI_ERROR = -1,
    OMPI_ERR_OUT_OF_RESOURCE = -2, /* fatal error */
    OMPI_ERR_TEMP_OUT_OF_RESOURCE = -3, /* try again later */
    OMPI_ERR_RESOURCE_BUSY = -4,
    OMPI_ERR_BAD_PARAM = -5,     /* equivalent to MPI_ERR_ARG error code */
    OMPI_ERR_RECV_LESS_THAN_POSTED = -6,
    OMPI_ERR_RECV_MORE_THAN_POSTED = -7,
    OMPI_ERR_NO_MATCH_YET = -8,
    OMPI_ERR_FATAL = -9,
    OMPI_ERR_NOT_IMPLEMENTED = -10,
    OMPI_ERR_NOT_SUPPORTED = -11,
    OMPI_ERR_INTERUPTED = -12,
    OMPI_ERR_WOULD_BLOCK = -13,
    OMPI_ERR_IN_ERRNO = -14,
    OMPI_ERR_UNREACH = -15,
    OMPI_ERR_NOT_FOUND = -16,
    OMPI_ERR_BUFFER = -17, /* equivalent to MPI_ERR_BUFFER */
    OMPI_ERR_REQUEST = -18, /* equivalent to MPI_ERR_REQUEST */
    OMPI_EXISTS = -19,  /* indicates that the specified object already exists */
    OMPI_ERR_NO_CONNECTION_ALLOWED = -20, /* indicates that the receiving process does not allow connections */
    OMPI_ERR_CONNECTION_REFUSED = -21, /* contact made with process, but it refuses any further communication */
    OMPI_ERR_CONNECTION_FAILED = -22,  /* message sent, but delivery failed */
    OMPI_ERR_TIMEOUT = -23,
    OMPI_STARTUP_DETECTED = -24,
    OMPI_SHUTDOWN_DETECTED = -25,
    OMPI_PROC_STARTING = -26,
    OMPI_PROC_STOPPED = -27,
    OMPI_PROC_TERMINATING = -28,
    OMPI_PROC_ALIVE = -29,
    OMPI_PROC_RUNNING = -30,
    OMPI_PROC_KILLED = -31,
    	OMPI_PROC_EXITED = -32,
    OMPI_NODE_UP = -33,
    OMPI_NODE_DOWN = -34,
    OMPI_NODE_BOOTING = -35,
    OMPI_NODE_ERROR = -36,
    OMPI_PACK_MISMATCH = -37,
    OMPI_ERR_PACK_FAILURE = -38,
    OMPI_ERR_UNPACK_FAILURE = -39,
    OMPI_ERR_COMM_FAILURE = -40,
    OMPI_UNPACK_INADEQUATE_SPACE = -41,
    OMPI_UNPACK_READ_PAST_END_OF_BUFFER = -42,
    OMPI_ERR_NOT_AVAILABLE = -43
};

#define OMPI_NAMESPACE_SEGMENT        "ompi-namespace"

#endif /* OMPI_CONSTANTS_H */

