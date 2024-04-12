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
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_UTIL_ERROR_H
#define PMIX_UTIL_ERROR_H

#include "src/include/pmix_config.h"

#include "pmix.h"
#include "src/util/pmix_output.h"

BEGIN_C_DECLS

/* define a starting point for PMIx internal error codes
 * that are never exposed outside the library */
#define PMIX_INTERNAL_ERR_BASE -1330

/****    PMIX ERROR CONSTANTS    ****/

/* internal error codes - never exposed outside of the library */
#define PMIX_ERR_FABRIC_NOT_PARSEABLE -1363
#define PMIX_ERR_TAKE_NEXT_OPTION     -1366
#define PMIX_ERR_TEMP_UNAVAILABLE     -1367

#define PMIX_INTERNAL_ERR_DONE   -2000

#define PMIX_ERROR_LOG(r)                                                                  \
    do {                                                                                   \
        if (PMIX_ERR_SILENT != (r)) {                                                      \
            pmix_output(0, "PMIX ERROR: %s in file %s at line %d", PMIx_Error_string((r)), \
                        __FILE__, __LINE__);                                               \
        }                                                                                  \
    } while (0)

END_C_DECLS

#endif /* PMIX_UTIL_ERROR_H */
