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
    case PMIX_ERR_DEBUGGER_RELEASE:
        return "DEBUGGER-RELEASE";
    case PMIX_ERR_SILENT:
        return "SILENT_ERROR";
    case PMIX_ERROR:
        return "ERROR";
    case PMIX_ERR_GRP_FOUND:
        return "GROUP-FOUND";
    case PMIX_ERR_DFLT_FOUND:
        return "DEFAULT-FOUND";
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
     * which one to call for the specific error */
    int i, idflt;
    size_t j, k;
    bool fired = false;
    bool exact_match;
    int allerrhandler_ind = -1;
    pmix_error_reg_info_t *errreg, *errdflt=NULL;
    pmix_info_t *iptr;

    /* we will need to provide the errhandler reference id when
     * we provide the callback. Since the callback function doesn't
     * provide a param for that purpose, we have to add it to any
     * info array that came from the RM, so extend the array by 1 */
    PMIX_INFO_CREATE(iptr, ninfo+1);
    /* put the reference id in the first location */
    (void)strncpy(iptr[0].key, PMIX_ERROR_HANDLER_ID, PMIX_MAX_KEYLEN);
    iptr[0].value.type = PMIX_INT;
    /* we don't know the reference id yet, but we'll fill that in
     * later - for now, just copy the incoming info array across */
    if (NULL != info) {
        for (j=0; j < ninfo; j++) {
            PMIX_INFO_LOAD(&iptr[j+1], info[j].key, &info[j].value.data, info[j].value.type);
        }
    }

    /* search our array of errhandlers for a match. We take any specific
     * error status first, then take the group of the incoming status next.
     * If neither of those have been registered, then use any default
     * errhandler - otherwise, ignore it */
    for (i = 0; i < pmix_globals.errregs.size; i++) {
        if (NULL == (errreg = (pmix_error_reg_info_t*) pmix_pointer_array_get_item(&pmix_globals.errregs, i))) {
            continue;
        }
        if (NULL == errreg->info || 0 == errreg->ninfo) {
            // this is a general err handler - we will call it if there is no better match
            errdflt = errreg;
            idflt = i;
            continue;
        }
        iptr[0].value.data.integer = i;
        /* match error name key first */
        exact_match = false;
        for (j = 0; j < errreg->ninfo; j++) {
            if ((0 == strcmp(errreg->info[j].key, PMIX_ERROR_NAME)) &&
                (status == errreg->info[j].value.data.int32)) {
                    iptr[0].value.data.integer = i;
                    errreg->errhandler(status, procs, nprocs, iptr, ninfo+1);
                    fired = true;
                    exact_match = true;
                    break;
            }
        }
        if (!exact_match && NULL != info) {
            /* if no exact match was found, then we will fire the errhandler
             * for any matching info key. This may be too lax and need to be adjusted
             * later */
            for (k = 0; k < errreg->ninfo; k++) {
                if ((0 == strcmp(errreg->info[j].key, info[k].key)) &&
                    (pmix_value_cmp(&errreg->info[j].value, &info[k].value))) {
                    errreg->errhandler(status, procs, nprocs, iptr, ninfo+1);
                    fired = true;
                }
            }
        }
    }

    /* if nothing fired and we found a general err handler, then fire it */
    if (!fired && NULL != errdflt) {
        iptr[0].value.data.integer = idflt;
        errdflt->errhandler(status, procs, nprocs, iptr, ninfo+1);
    }
    /* cleanup */
    PMIX_INFO_FREE(iptr, ninfo+1);
}

/* lookup an errhandler during registration */
pmix_status_t pmix_lookup_errhandler(pmix_info_t info[], size_t ninfo,
                                     int *index)
{
    int i, idflt=-1, igrp=-1;
    pmix_error_reg_info_t *errreg;
    size_t sz, n;
    char errgrp[PMIX_MAX_KEYLEN];
    bool exact_given = false;
    int given = -1;
    pmix_status_t status;
    char *grp;

    /* scan the incoming specification to see if it is a general errhandler,
     * a group errhandler, or an error handler for a specific status. Only
     * one of these options can be specified! */
    if (NULL == info) {
        /* this is the general error handler */
        given = 0;
    } else {
        for (n=0; n < ninfo; n++) {
            if (0 == strncmp(info[n].key, PMIX_ERROR_NAME, PMIX_MAX_KEYLEN)) {
                /* this is a specific errhandler */
                given = 1;
                status = info[n].value.data.integer;
                break;
            } else if (0 == strcmp(info[n].key, "pmix.errgroup")) {
                /* this is a group errhandler */
                given = 2;
                grp = info[n].value.data.string;
                break;
            }
        }
    }

    /* search our array of errhandlers for a match */
    for (i = 0; i < pmix_globals.errregs.size ; i++) {
        errreg = (pmix_error_reg_info_t*)pmix_pointer_array_get_item(&pmix_globals.errregs, i);
        if (NULL == errreg) {
            continue;
        }
        if (NULL == errreg->info) {
            /* this is the general errhandler - if they gave us
             * another general errhandler, then we should
             * replace it */
            if (0 == given) {
                *index = i;
                return PMIX_ERR_DFLT_FOUND;
            }
            /* save this spot as we will default to it if nothing else is found */
            idflt = i;
            continue;
        }
        if (0 == given) {
            /* they are looking for the general errhandler */
            continue;
        }
        /* if this registration is for a single specific errhandler, then
         * see if the incoming one matches */
        if (1 == given && errreg->sglhdlr) {
            for (sz=0; sz < errreg->ninfo; sz++) {
                if (0 == strncmp(errreg->info[sz].key, PMIX_ERROR_NAME, PMIX_MAX_KEYLEN)) {
                    if (status == errreg->info[sz].value.data.integer) {
                        /* we have an exact match - return this errhandler and
                         * let the caller know it was an exact match */
                        *index = i;
                        return PMIX_EXISTS;
                    }
                }
            }
        } else if (2 == given && !errreg->sglhdlr) {
            /* this registration is for a group, so check that case */

        }
    }

    /* if we get here, then no match was found. If they
     * gave us a specific error, then we have to return not_found */
    if (exact_given) {
        return PMIX_ERR_NOT_FOUND;
    }

    /* If we have a group match, then that takes precedence */
    if (0 <= igrp) {
        *index = igrp;
        return PMIX_ERR_GRP_FOUND;
    }

    /* if we found a default errhandler, then use it */
    if (0 <= idflt) {
        *index = idflt;
        return PMIX_ERR_DFLT_FOUND;
    }

    /* otherwise, it wasn't found */
    return PMIX_ERR_NOT_FOUND;
}

pmix_status_t pmix_add_errhandler(pmix_notification_fn_t err,
                                  pmix_info_t *info, int ninfo,
                                  int *index)
{
    int i;
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_error_reg_info_t *errreg;
    bool sglhdlr = false;

    if (0 != *index) {
        /* overwrite an existing entry */
        errreg = (pmix_error_reg_info_t*)pmix_pointer_array_get_item(&pmix_globals.errregs, *index);
        if (NULL == errreg) {
            return PMIX_ERR_NOT_FOUND;
        }
        errreg->errhandler = err;
        PMIX_INFO_FREE(errreg->info, errreg->ninfo);
        errreg->ninfo = ninfo;
    } else {
        errreg = PMIX_NEW(pmix_error_reg_info_t);
        errreg->errhandler = err;
        errreg->ninfo = ninfo;
        *index = pmix_pointer_array_add(&pmix_globals.errregs, errreg);
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "pmix_add_errhandler index =%d", *index);
        if (*index < 0) {
            PMIX_RELEASE(errreg);
            return PMIX_ERROR;
        }
    }
    /* sadly, we have to copy the info objects as we cannot
     * rely on them to remain in-memory */
    if (NULL != info && 0 < ninfo) {
        PMIX_INFO_CREATE(errreg->info, ninfo);
        for (i=0; i < ninfo; i++) {
            /* if this is a specific, single errhandler, then
             * mark it accordingly */
            if (0 == strncmp(info[i].key, PMIX_ERROR_NAME, PMIX_MAX_KEYLEN)) {
                errreg->sglhdlr = true;
            }
            (void)strncpy(errreg->info[i].key, info[i].key, PMIX_MAX_KEYLEN);
            pmix_value_xfer(&errreg->info[i].value, &info[i].value);
        }
    }

    return PMIX_SUCCESS;
}

pmix_status_t pmix_remove_errhandler(int errhandler_ref)
{
    int rc = PMIX_SUCCESS;
    pmix_error_reg_info_t *errreg;

    errreg = (pmix_error_reg_info_t*)pmix_pointer_array_get_item(&pmix_globals.errregs,
                                                                 errhandler_ref);
    if (NULL != errreg) {
        PMIX_RELEASE(errreg);
        pmix_pointer_array_set_item(&pmix_globals.errregs, errhandler_ref, NULL);
    } else {
        rc = PMIX_ERR_NOT_FOUND;
    }
    return rc;
}

void pmix_get_errorgroup(pmix_status_t status, char *pmix_error_group)
{
    switch(status) {
        case PMIX_ERR_UNREACH:
        case PMIX_ERR_COMM_FAILURE:
        case PMIX_ERR_SERVER_NOT_AVAIL:
        case PMIX_ERR_TIMEOUT:
        case PMIX_ERR_PACK_FAILURE:
        case PMIX_ERR_UNPACK_FAILURE:
            (void)strncpy(pmix_error_group, PMIX_ERROR_GROUP_COMM, PMIX_MAX_KEYLEN);
            break;
        case PMIX_ERR_OUT_OF_RESOURCE:
        case PMIX_ERR_RESOURCE_BUSY:
        case PMIX_ERR_NOMEM:
            (void)strncpy(pmix_error_group, PMIX_ERROR_GROUP_RESOURCE, PMIX_MAX_KEYLEN);
            break;
        case PMIX_ERR_PROC_MIGRATE:
        case PMIX_ERR_PROC_CHECKPOINT:
        case PMIX_ERR_PROC_RESTART:
            (void)strncpy(pmix_error_group, PMIX_ERROR_GROUP_MIGRATE, PMIX_MAX_KEYLEN);
            break;
        case PMIX_ERR_PROC_ABORTING:
        case PMIX_ERR_PROC_REQUESTED_ABORT:
        case PMIX_ERR_PROC_ABORTED:
            (void)strncpy(pmix_error_group, PMIX_ERROR_GROUP_ABORT, PMIX_MAX_KEYLEN);
            break;
        default:
            (void)strncpy(pmix_error_group, PMIX_ERROR_GROUP_GENERAL, PMIX_MAX_KEYLEN);
    }
}
