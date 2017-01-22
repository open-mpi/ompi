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

#include <pmix/pmix_common.h>

#include "src/util/error.h"
#include "src/include/pmix_globals.h"
#include "src/buffer_ops/buffer_ops.h"

const char* PMIx_Error_string(pmix_status_t errnum)
{
    switch(errnum) {
    case PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER:
        return "UNPACK-PAST-END";
    case PMIX_ERR_COMM_FAILURE:
        return "COMM-FAILURE";
    case PMIX_ERR_NOT_IMPLEMENTED:
        return "NOT-IMPLEMENTED";
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
    case PMIX_ERR_RESOURCE_BUSY:
        return "RESOURCE-BUSY";
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
    case PMIX_SUCCESS:
        return "SUCCESS";

    }
    return "ERROR STRING NOT FOUND";
}

void pmix_errhandler_invoke(pmix_status_t status,
                            pmix_proc_t procs[], size_t nprocs,
                            pmix_info_t info[], size_t ninfo)
{
    return;
}

pmix_status_t pmix_lookup_errhandler(pmix_notification_fn_t err,
                                     int *index)
{
    return PMIX_ERR_NOT_FOUND;
}

pmix_status_t pmix_add_errhandler(pmix_notification_fn_t err,
                                  pmix_info_t *info, int ninfo,
                                  int *index)
{
    return PMIX_ERR_NOT_SUPPORTED;
}

pmix_status_t pmix_remove_errhandler(int errhandler_ref)
{
    return PMIX_ERR_NOT_SUPPORTED;
}

void pmix_get_errorgroup(pmix_status_t status, char *pmix_error_group)
{
    return;
}
