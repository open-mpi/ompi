/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2015      Intel, Inc. All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_UTIL_ERROR_H
#define PMIX_UTIL_ERROR_H

#include <private/autogen/config.h>
#include <pmix/rename.h>

#include <pmix/pmix_common.h>
#include "src/util/output.h"

BEGIN_C_DECLS

#define PMIX_ERROR_LOG(r)                                               \
     do {                                                               \
        if (PMIX_ERR_SILENT != (r)) {                                   \
            pmix_output(0, "PMIX ERROR: %s in file %s at line %d",      \
                        PMIx_Error_string((r)), __FILE__, __LINE__);    \
        }                                                               \
    }while(0);

#define PMIX_REPORT_ERROR(e)                            \
    do {                                                \
        pmix_globals.connected = false;                 \
        pmix_errhandler_invoke(e, NULL, 0, NULL, 0);    \
    } while(0);

/* invoke the error handler that is registered against the given
 * status, passing it the provided info on the procs that were
 * affected, plus any additional info provided by the server */
PMIX_DECLSPEC void pmix_errhandler_invoke(pmix_status_t status,
                                          pmix_proc_t procs[], size_t nprocs,
                                          pmix_info_t info[], size_t ninfo);

/* lookup the errhandler registered against the given status. If there
 * is none, but an errhandler has been registered against the group
 * that this status belongs to, then return that errhandler. If neither
 * of those is true, but a general errhandler has been registered, then
 * return that errhandler. Otherwise, return NOT_FOUND */
PMIX_DECLSPEC pmix_status_t pmix_lookup_errhandler(pmix_info_t info[], size_t ninfo,
                                                   int *index);

PMIX_DECLSPEC pmix_status_t pmix_add_errhandler(pmix_notification_fn_t err,
                                                pmix_info_t *info, int ninfo,
                                                int *index);

PMIX_DECLSPEC pmix_status_t pmix_remove_errhandler(int errhandler_ref);

PMIX_DECLSPEC void pmix_get_errorgroup ( pmix_status_t status, char *pmix_error_group);

END_C_DECLS

#endif /* PMIX_UTIL_ERROR_H */
