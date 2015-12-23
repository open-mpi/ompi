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
 * Copyright (c) 2014-2015 Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <private/autogen/config.h>
#include <pmix/rename.h>

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
    /* We need to parse thru each registered handler and determine
       which one to call for the specific error */
    int matched_errregs[PMIX_MAX_ERROR_REGISTRATIONS];
    int i, nmatched = 0;
    unsigned int j,k;
    bool exact_match = false;
    int allerrhandler_ind = -1;
    pmix_error_reg_info_t *errreg;
    for (i = 0; i < pmix_pointer_array_get_size(&pmix_globals.errregs) &&
            !exact_match; i++) {
        errreg = (pmix_error_reg_info_t*) pmix_pointer_array_get_item (&pmix_globals.errregs, i);
        if(0 == errreg->ninfo) {
            // this is a general err handler we will call it if there is no better match
            allerrhandler_ind = i;
        } else {
            /* match error name key first */
            for(j = 0; j < errreg->ninfo; j++) {
                if ((0 == strcmp(errreg->info[j].key, PMIX_ERROR_NAME)) &&
                        (status == errreg->info[j].value.data.int32)) {
                     matched_errregs[0] = i;
                     nmatched = 1;
                     exact_match = true;
                     break;
                } else {
                    for (k = 0; k < errreg->ninfo; k++) {
                        if ((0 == strcmp(errreg->info[j].key, info[k].key)) &&
                            (pmix_value_cmp(&errreg->info[j].value, &info[k].value))) {
                            matched_errregs[nmatched++] = 1;
                            break;
                        }
                    }
                }
            }
        }
    }
    for ( i =0 ;i < nmatched; i++) {
        errreg = (pmix_error_reg_info_t*) pmix_pointer_array_get_item (&pmix_globals.errregs,
                                                                        matched_errregs[i]);
        errreg->errhandler (status, procs, nprocs, info, ninfo);
    }
    if ( 0 == nmatched && 0 <= allerrhandler_ind) {
        errreg = (pmix_error_reg_info_t*) pmix_pointer_array_get_item (&pmix_globals.errregs,
                  allerrhandler_ind);
        errreg->errhandler (status, procs, nprocs, info, ninfo);
    }
}

pmix_status_t pmix_lookup_errhandler(pmix_notification_fn_t err,
                                     int *index)
{
    int i;
    pmix_status_t rc = PMIX_ERR_NOT_FOUND;
    pmix_error_reg_info_t *errreg = NULL;
    for (i = 0; i < pmix_pointer_array_get_size(&pmix_globals.errregs) ; i++) {
        errreg = (pmix_error_reg_info_t*) pmix_pointer_array_get_item (&pmix_globals.errregs, i);
        if((NULL != errreg) && (err == errreg->errhandler)) {
            *index = i;
            rc = PMIX_SUCCESS;
            break;
        }
    }
    return rc;
}

pmix_status_t pmix_add_errhandler(pmix_notification_fn_t err,
                                  pmix_info_t *info, int ninfo,
                                  int *index)
{
    int i;
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_error_reg_info_t *errreg = PMIX_NEW(pmix_error_reg_info_t);
    errreg->errhandler = err;
    errreg->ninfo = ninfo;
    PMIX_INFO_CREATE(errreg->info, ninfo);
    for (i=0; i < ninfo; i++) {
        memcpy(errreg->info[i].key, info[i].key, PMIX_MAX_KEYLEN);
        pmix_value_xfer(&errreg->info[i].value, &info[i].value);
    }
    *index = pmix_pointer_array_add (&pmix_globals.errregs, errreg);
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix_add_errhandler index =%d", *index);
    if (-1 == *index)
        rc = PMIX_ERROR;
    return rc;
}

pmix_status_t pmix_remove_errhandler(int errhandler_ref)
{
    int rc = PMIX_SUCCESS;
    pmix_error_reg_info_t *errreg;
    errreg = (pmix_error_reg_info_t*) pmix_pointer_array_get_item (&pmix_globals.errregs,
                                                                   errhandler_ref);
    if (NULL != errreg)
        PMIX_RELEASE(errreg);
    else
        rc = PMIX_ERR_NOT_FOUND;
    return rc;
}

void pmix_get_errorgroup ( pmix_status_t status, char *pmix_error_group)
{
    switch(status) {
        case PMIX_ERR_UNREACH:
        case PMIX_ERR_COMM_FAILURE:
        case PMIX_ERR_SERVER_NOT_AVAIL:
        case PMIX_ERR_TIMEOUT:
        case PMIX_ERR_PACK_FAILURE:
        case PMIX_ERR_UNPACK_FAILURE:
            strcpy(pmix_error_group, PMIX_ERROR_GROUP_COMM);
            break;
        case PMIX_ERR_OUT_OF_RESOURCE:
        case PMIX_ERR_RESOURCE_BUSY:
        case PMIX_ERR_NOMEM:
            strcpy(pmix_error_group, PMIX_ERROR_GROUP_RESOURCE);
            break;
        case PMIX_ERR_PROC_MIGRATE:
        case PMIX_ERR_PROC_CHECKPOINT:
        case PMIX_ERR_PROC_RESTART:
            strcpy(pmix_error_group, PMIX_ERROR_GROUP_MIGRATE);
            break;
        case PMIX_ERR_PROC_ABORTING:
        case PMIX_ERR_PROC_REQUESTED_ABORT:
        case PMIX_ERR_PROC_ABORTED:
            strcpy(pmix_error_group, PMIX_ERROR_GROUP_ABORT);
            break;
        default:
            strcpy(pmix_error_group, PMIX_ERROR_GROUP_GENERAL);
    }
}
