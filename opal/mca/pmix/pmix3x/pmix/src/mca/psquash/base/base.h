/* -*- C -*-
 *
 * Copyright (c) 2019      IBM Corporation.  All rights reserved.
 * Copyright (c) 2019      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2020      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2020      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */
#ifndef PMIX_PSQUASH_BASE_H_
#define PMIX_PSQUASH_BASE_H_

#include "src/include/pmix_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "src/class/pmix_pointer_array.h"
#include "src/mca/mca.h"
#include "src/mca/base/pmix_mca_base_framework.h"

#include "src/mca/psquash/psquash.h"


BEGIN_C_DECLS

/**
 * Sizeof by PMIx type integer values.
 *
 * r - return status code
 * t - type (pmix_data_type_t) of integer value
 * s - size of type in bytes
 * (see a comment to `pmix_bfrops_pack_flex` for additional details)
 */
#define PMIX_SQUASH_TYPE_SIZEOF(r, t, s)                    \
do {                                                        \
    (r) = PMIX_SUCCESS;                                     \
    switch (t) {                                            \
        case PMIX_INT16:                                    \
        case PMIX_UINT16:                                   \
            (s) = SIZEOF_SHORT;                             \
            break;                                          \
        case PMIX_INT:                                      \
        case PMIX_INT32:                                    \
        case PMIX_UINT:                                     \
        case PMIX_UINT32:                                   \
            (s) = SIZEOF_INT;                               \
            break;                                          \
        case PMIX_INT64:                                    \
        case PMIX_UINT64:                                   \
            (s) = SIZEOF_LONG;                              \
            break;                                          \
        case PMIX_SIZE:                                     \
            (s) = SIZEOF_SIZE_T;                            \
            break;                                          \
        default:                                            \
            (r) = PMIX_ERR_BAD_PARAM;                       \
    }                                                       \
} while (0)

struct pmix_psquash_globals_t {
  bool initialized;
  bool selected;
};

typedef struct pmix_psquash_globals_t pmix_psquash_globals_t;

PMIX_EXPORT extern pmix_mca_base_framework_t pmix_psquash_base_framework;

PMIX_EXPORT pmix_status_t pmix_psquash_base_select(void);

PMIX_EXPORT extern pmix_psquash_globals_t pmix_psquash_globals;

END_C_DECLS

#endif
