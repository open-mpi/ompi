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
 * Copyright (c) 2015-2016 Intel, Inc. All rights reserved
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
    do {                                                                \
        if (PMIX_ERR_SILENT != (r)) {                                   \
            pmix_output(0, "PMIX ERROR: %s in file %s at line %d",      \
                        PMIx_Error_string((r)), __FILE__, __LINE__);    \
        }                                                               \
    }while(0);

#define PMIX_REPORT_ERROR(e)  \
    pmix_errhandler_invoke(e, NULL, 0, NULL, 0)

PMIX_DECLSPEC void pmix_errhandler_invoke(pmix_status_t status,
                                          pmix_proc_t procs[], size_t nprocs,
                                          pmix_info_t info[], size_t ninfo);

END_C_DECLS

#endif /* PMIX_UTIL_ERROR_H */
