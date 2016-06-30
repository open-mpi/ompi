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

#include <src/include/pmix_config.h>


#include <pmix/pmix_common.h>
#include "src/util/output.h"

 BEGIN_C_DECLS

/* internal error codes - never exposed outside of the library */
#define PMIX_ERR_INVALID_CRED                           (PMIX_INTERNAL_ERR_BASE -  1)
#define PMIX_ERR_HANDSHAKE_FAILED                       (PMIX_INTERNAL_ERR_BASE -  2)
#define PMIX_ERR_READY_FOR_HANDSHAKE                    (PMIX_INTERNAL_ERR_BASE -  3)
#define PMIX_ERR_UNKNOWN_DATA_TYPE                      (PMIX_INTERNAL_ERR_BASE -  4)
#define PMIX_ERR_TYPE_MISMATCH                          (PMIX_INTERNAL_ERR_BASE -  5)
#define PMIX_ERR_UNPACK_INADEQUATE_SPACE                (PMIX_INTERNAL_ERR_BASE -  6)
#define PMIX_ERR_UNPACK_FAILURE                         (PMIX_INTERNAL_ERR_BASE -  7)
#define PMIX_ERR_PACK_FAILURE                           (PMIX_INTERNAL_ERR_BASE -  8)
#define PMIX_ERR_PACK_MISMATCH                          (PMIX_INTERNAL_ERR_BASE -  9)
#define PMIX_ERR_PROC_ENTRY_NOT_FOUND                   (PMIX_INTERNAL_ERR_BASE - 10)
#define PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER         (PMIX_INTERNAL_ERR_BASE - 11)
#define PMIX_ERR_SERVER_NOT_AVAIL                       (PMIX_INTERNAL_ERR_BASE - 12)
#define PMIX_ERR_INVALID_KEYVALP                        (PMIX_INTERNAL_ERR_BASE - 13)
#define PMIX_ERR_INVALID_NUM_PARSED                     (PMIX_INTERNAL_ERR_BASE - 14)
#define PMIX_ERR_INVALID_ARGS                           (PMIX_INTERNAL_ERR_BASE - 15)
#define PMIX_ERR_INVALID_NUM_ARGS                       (PMIX_INTERNAL_ERR_BASE - 16)
#define PMIX_ERR_INVALID_LENGTH                         (PMIX_INTERNAL_ERR_BASE - 17)
#define PMIX_ERR_INVALID_VAL_LENGTH                     (PMIX_INTERNAL_ERR_BASE - 18)
#define PMIX_ERR_INVALID_VAL                            (PMIX_INTERNAL_ERR_BASE - 19)
#define PMIX_ERR_INVALID_KEY_LENGTH                     (PMIX_INTERNAL_ERR_BASE - 20)
#define PMIX_ERR_INVALID_KEY                            (PMIX_INTERNAL_ERR_BASE - 21)
#define PMIX_ERR_INVALID_ARG                            (PMIX_INTERNAL_ERR_BASE - 22)
#define PMIX_ERR_NOMEM                                  (PMIX_INTERNAL_ERR_BASE - 23)
#define PMIX_ERR_IN_ERRNO                               (PMIX_INTERNAL_ERR_BASE - 24)
#define PMIX_ERR_SILENT                                 (PMIX_INTERNAL_ERR_BASE - 25)
#define PMIX_ERR_UNKNOWN_DATATYPE                       (PMIX_INTERNAL_ERR_BASE - 26)
#define PMIX_ERR_RESOURCE_BUSY                          (PMIX_INTERNAL_ERR_BASE - 27)
#define PMIX_ERR_OPERATION_IN_PROGRESS                  (PMIX_INTERNAL_ERR_BASE - 28)

#define PMIX_ERROR_LOG(r)                                           \
 do {                                                               \
    if (PMIX_ERR_SILENT != (r)) {                                   \
        pmix_output(0, "PMIX ERROR: %s in file %s at line %d",      \
                    PMIx_Error_string((r)), __FILE__, __LINE__);    \
    }                                                               \
} while (0)

 END_C_DECLS

#endif /* PMIX_UTIL_ERROR_H */
