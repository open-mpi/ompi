/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, Inc. All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 *
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * @file:
 *
 */

#ifndef PMIX_PSTRG_H_
#define PMIX_PSTRG_H_

#include "src/include/pmix_config.h"

#include "src/class/pmix_list.h"
#include "src/include/pmix_globals.h"
#include "src/mca/mca.h"

BEGIN_C_DECLS

/* initialization */
typedef pmix_status_t (*pmix_pstrg_base_module_init_fn_t)(void);

/* finalization */
typedef void (*pmix_pstrg_base_module_fini_fn_t)(void);

/* query support
 *
 * If the operation can be performed immediately, then the module should just
 * add the results (as pmix_kval_t's) to the provided list and
 * return PMIX_SUCCESS.
 *
 * If the module needs to perform some non-atomic operation
 * (e.g., query a storage manager), then it should shift to its own internal
 * thread, return PMIX_OPERATION_IN_PROGRESS, and execute the provided
 * callback function when the operation is completed.
 *
 * If there is no support for the given keys, then just return PMIX_SUCCESS.
 *
 * If the module should be providing a response but encounters an error,
 * then immediately return an error code if the error is immediately detected,
 * or execute the callback function with an error code if it is detected later.
 */
typedef pmix_status_t (*pmix_pstrg_base_module_query_fn_t)(pmix_query_t queries[], size_t nqueries,
                                                           pmix_list_t *results,
                                                           pmix_pstrg_query_cbfunc_t cbfunc,
                                                           void *cbdata);

/*
 * Ver 1.0
 */
typedef struct pmix_pstrg_base_module_1_0_0_t {
    char *name;
    pmix_pstrg_base_module_init_fn_t init;
    pmix_pstrg_base_module_fini_fn_t finalize;
    pmix_pstrg_base_module_query_fn_t query;
} pmix_pstrg_base_module_t;

/*
 * the standard component data structure
 */
typedef pmix_mca_base_component_t pmix_pstrg_base_component_t;

typedef struct pmix_pstrg_API_module_1_0_0_t {
    pmix_pstrg_base_module_query_fn_t query;
} pmix_pstrg_API_module_t;

/*
 * Macro for use in components that are of type storage v1.0.0
 */
#define PMIX_PSTRG_BASE_VERSION_1_0_0 PMIX_MCA_BASE_VERSION_1_0_0("pstrg", 1, 0, 0)

/* Global structure for accessing storage functions
 */
PMIX_EXPORT extern pmix_pstrg_API_module_t pmix_pstrg; /* holds API function pointers */

END_C_DECLS

#endif /* MCA_PSTRG_H */
