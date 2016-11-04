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
 * Copyright (c) 2007-2012 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014-2016 Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <src/include/pmix_config.h>


#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <errno.h>
#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include <pmix_common.h>

#include "src/util/error.h"

const char* PMIx_Error_string(pmix_status_t errnum)
{
    switch(errnum) {
    case PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER:
        return "UNPACK-PAST-END";
    case PMIX_ERR_LOST_CONNECTION_TO_SERVER:
        return "LOST_CONNECTION_TO_SERVER";
    case PMIX_ERR_NOT_SUPPORTED:
        return "NOT-SUPPORTED";
    case PMIX_ERR_NOT_FOUND:
        return "NOT-FOUND";
    case PMIX_ERR_SERVER_NOT_AVAIL:
        return "SERVER-NOT-AVAIL";
    case PMIX_ERR_INVALID_NAMESPACE:
        return "INVALID-NAMESPACE";
    case PMIX_ERR_INVALID_SIZE:
        return "INVALID-SIZE";
    case PMIX_ERR_INVALID_KEYVALP:
        return "INVALID-KEYVAL";
    case PMIX_ERR_INVALID_NUM_PARSED:
        return "INVALID-NUM-PARSED";

    case PMIX_ERR_INVALID_ARGS:
        return "INVALID-ARGS";
    case PMIX_ERR_INVALID_NUM_ARGS:
        return "INVALID-NUM-ARGS";
    case PMIX_ERR_INVALID_LENGTH:
        return "INVALID-LENGTH";
    case PMIX_ERR_INVALID_VAL_LENGTH:
        return "INVALID-VAL-LENGTH";
    case PMIX_ERR_INVALID_VAL:
        return "INVALID-VAL";
    case PMIX_ERR_INVALID_KEY_LENGTH:
        return "INVALID-KEY-LENGTH";
    case PMIX_ERR_INVALID_KEY:
        return "INVALID-KEY";
    case PMIX_ERR_INVALID_ARG:
        return "INVALID-ARG";
    case PMIX_ERR_NOMEM:
        return "NO-MEM";
    case PMIX_ERR_INIT:
        return "INIT";

    case PMIX_ERR_DATA_VALUE_NOT_FOUND:
        return "DATA-VALUE-NOT-FOUND";
    case PMIX_ERR_OUT_OF_RESOURCE:
        return "OUT-OF-RESOURCE";
    case PMIX_ERR_BAD_PARAM:
        return "BAD-PARAM";
    case PMIX_ERR_IN_ERRNO:
        return "ERR-IN-ERRNO";
    case PMIX_ERR_UNREACH:
        return "UNREACHABLE";
    case PMIX_ERR_TIMEOUT:
        return "TIMEOUT";
    case PMIX_ERR_NO_PERMISSIONS:
        return "NO-PERMISSIONS";
    case PMIX_ERR_PACK_MISMATCH:
        return "PACK-MISMATCH";
    case PMIX_ERR_PACK_FAILURE:
        return "PACK-FAILURE";

    case PMIX_ERR_UNPACK_FAILURE:
        return "UNPACK-FAILURE";
    case PMIX_ERR_UNPACK_INADEQUATE_SPACE:
        return "UNPACK-INADEQUATE-SPACE";
    case PMIX_ERR_TYPE_MISMATCH:
        return "TYPE-MISMATCH";
    case PMIX_ERR_PROC_ENTRY_NOT_FOUND:
        return "PROC-ENTRY-NOT-FOUND";
    case PMIX_ERR_UNKNOWN_DATA_TYPE:
        return "UNKNOWN-DATA-TYPE";
    case PMIX_ERR_WOULD_BLOCK:
        return "WOULD-BLOCK";
    case PMIX_ERR_READY_FOR_HANDSHAKE:
        return "READY-FOR-HANDSHAKE";
    case PMIX_ERR_HANDSHAKE_FAILED:
        return "HANDSHAKE-FAILED";
    case PMIX_ERR_INVALID_CRED:
        return "INVALID-CREDENTIAL";
    case PMIX_EXISTS:
        return "EXISTS";
    case PMIX_ERR_SERVER_FAILED_REQUEST:
        return "SERVER FAILED REQUEST";
    case PMIX_ERR_PROC_MIGRATE:
        return "PROC-MIGRATE";
    case PMIX_ERR_PROC_CHECKPOINT:
        return "PROC-CHECKPOINT-ERROR";
    case PMIX_ERR_PROC_RESTART:
        return "PROC_RESTART";
    case PMIX_ERR_PROC_ABORTING:
        return "PROC-ABORTING";
    case PMIX_ERR_PROC_REQUESTED_ABORT:
        return "PROC-ABORT-REQUESTED";
    case PMIX_ERR_PROC_ABORTED:
        return "PROC-ABORTED";
    case PMIX_ERR_DEBUGGER_RELEASE:
        return "DEBUGGER-RELEASE";
    case PMIX_ERR_SILENT:
        return "SILENT_ERROR";
    case PMIX_ERROR:
        return "ERROR";
    case PMIX_ERR_NOT_AVAILABLE:
        return "PMIX_ERR_NOT_AVAILABLE";
    case PMIX_ERR_FATAL:
        return "PMIX_ERR_FATAL";
    case PMIX_ERR_VALUE_OUT_OF_BOUNDS:
        return "PMIX_ERR_VALUE_OUT_OF_BOUNDS";
    case PMIX_ERR_NETWORK_NOT_PARSEABLE:
        return "PMIX_ERR_NETWORK_NOT_PARSEABLE";
    case PMIX_ERR_PERM:
        return "PMIX_ERR_PERM";
    case PMIX_SUCCESS:
        return "SUCCESS";
    default:
        return "ERROR STRING NOT FOUND";
    }
}
