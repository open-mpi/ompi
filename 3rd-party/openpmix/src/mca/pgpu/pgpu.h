/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2007-2008 Cisco Systems, Inc.  All rights reserved.
 *
 * Copyright (c) 2015-2018 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 * This interface is for use by PMIx servers to obtain GPU-related info
 * and to setup GPU support for applications prior to launch
 *
 * Available plugins may be defined at runtime via the typical MCA parameter
 * syntax.
 */

#ifndef PMIX_PGPU_H
#define PMIX_PGPU_H

#include "src/include/pmix_config.h"
#include "include/pmix.h"

#include "src/class/pmix_list.h"
#include "src/include/pmix_globals.h"
#include "src/mca/base/pmix_mca_base_framework.h"
#include "src/mca/base/pmix_mca_base_var.h"
#include "src/mca/mca.h"
#include "src/server/pmix_server_ops.h"

BEGIN_C_DECLS

/******    MODULE DEFINITION    ******/

/**
 * Initialize the module. Returns an error if the module cannot
 * run, success if it can and wants to be used.
 */
typedef pmix_status_t (*pmix_pgpu_base_module_init_fn_t)(void);

/**
 * Finalize the module. Tear down any allocated storage, disconnect
 * from any system support (e.g., LDAP server)
 */
typedef void (*pmix_pgpu_base_module_fini_fn_t)(void);

/**
 * Provide an entry point for the PMIx_server_setup_application function
 */
typedef pmix_status_t (*pmix_pgpu_base_module_allocate_fn_t)(pmix_namespace_t *nptr,
                                                             pmix_info_t info[], size_t ninfo,
                                                             pmix_list_t *ilist);

/**
 * Provide an entry point for the PMIx_server_setup_local_support function
 */
typedef pmix_status_t (*pmix_pgpu_base_module_setup_local_fn_t)(pmix_nspace_env_cache_t *ns,
                                                                pmix_info_t info[],
                                                                size_t ninfo);

/**
 * Provide an opportunity to cleanup when a
 * local application process terminates
 */
typedef void (*pmix_pgpu_base_module_child_finalized_fn_t)(pmix_proc_t *peer);

/**
 * Provide  an opportunity to cleanup after
 * all local clients for a given application have terminated
 */
typedef void (*pmix_pgpu_base_module_local_app_finalized_fn_t)(pmix_namespace_t *nptr);

/**
 * Provide an opportunity for the components to cleanup any
 * resource allocations they may have assigned
 */
typedef void (*pmix_pgpu_base_module_dregister_nspace_fn_t)(pmix_namespace_t *nptr);

/**
 * Request that the module report local inventory
 *
 * If the operation can be performed immediately, then the module should just
 * add the inventory (as pmix_kval_t's) to the provided object's list and
 * return PMIX_SUCCESS.
 *
 * If the module needs to perform some non-atomic operation
 * then it should shift to its own internal
 * thread, return PMIX_OPERATION_IN_PROGRESS, and execute the provided
 * callback function when the operation is completed.
 *
 * If there is no inventory to report, then just return PMIX_SUCCESS.
 *
 * If the module should be providing inventory but encounters an error,
 * then immediately return an error code if the error is immediately detected,
 * or execute the callback function with an error code if it is detected later.
 */
typedef pmix_status_t (*pmix_pgpu_base_module_collect_inventory_fn_t)(pmix_info_t directives[], size_t ndirs,
                                                                      pmix_list_t *inventory);

/**
 * Deliver inventory for archiving by corresponding modules
 *
 * Modules are to search the provided inventory to identify
 * entries provided by their remote peers, and then store that
 * information in a manner that can be queried/retrieved by
 * the host RM and/or scheduler. If the operation can be
 * performed immediately (e.g., storing the information in
 * the local hash table), then the module should just perform
 * that operation and return the appropriate status.
 *
 * If the module needs to perform some non-atomic operation
 * (e.g., storing the information in a non-local DHT), then
 * it should shift to its own internal thread, return
 * PMIX_OPERATION_IN_PROGRESS, and execute the provided
 * callback function when the operation is completed.
 *
 * If there is no relevant inventory to archive, then the module
 * should just return PMIX_SUCCESS;
 */
typedef pmix_status_t (*pmix_pgpu_base_module_deliver_inventory_fn_t)(pmix_info_t info[], size_t ninfo,
                                                                      pmix_info_t directives[], size_t ndirs);

/**
 * Base structure for a pgpu module. Each component should malloc a
 * copy of the module structure for each fabric plane they support.
 */
typedef struct {
    char *name;
    /* provide a pointer to plane-specific metadata */
    void *plane;
    /* init/finalize */
    pmix_pgpu_base_module_init_fn_t init;
    pmix_pgpu_base_module_fini_fn_t finalize;
    pmix_pgpu_base_module_allocate_fn_t allocate;
    pmix_pgpu_base_module_setup_local_fn_t setup_local;
    pmix_pgpu_base_module_child_finalized_fn_t child_finalized;
    pmix_pgpu_base_module_local_app_finalized_fn_t local_app_finalized;
    pmix_pgpu_base_module_dregister_nspace_fn_t deregister_nspace;
    pmix_pgpu_base_module_collect_inventory_fn_t collect_inventory;
    pmix_pgpu_base_module_deliver_inventory_fn_t deliver_inventory;
} pmix_pgpu_module_t;

/* define a few API versions of the functions - main difference is the
 * string nspace parameter instead of a pointer to pmix_namespace_t. This
 * is done as an optimization to avoid having every component look for
 * that pointer */
typedef pmix_status_t (*pmix_pgpu_base_API_allocate_fn_t)(char *nspace, pmix_info_t info[],
                                                          size_t ninfo, pmix_list_t *ilist);
typedef pmix_status_t (*pmix_pgpu_base_API_setup_local_fn_t)(char *nspace, pmix_info_t info[],
                                                             size_t ninfo);
typedef pmix_status_t (*pmix_pgpu_base_API_setup_fork_fn_t)(const pmix_proc_t *peer, char ***env);

typedef void (*pmix_pgpu_base_API_deregister_nspace_fn_t)(char *nspace);

/**
 * Base structure for a pgpu API
 */
typedef struct {
    char *name;
    /* init/finalize */
    pmix_pgpu_base_module_init_fn_t init;
    pmix_pgpu_base_module_fini_fn_t finalize;
    pmix_pgpu_base_API_allocate_fn_t allocate;
    pmix_pgpu_base_API_setup_local_fn_t setup_local;
    pmix_pgpu_base_API_setup_fork_fn_t setup_fork;
    pmix_pgpu_base_module_child_finalized_fn_t child_finalized;
    pmix_pgpu_base_module_local_app_finalized_fn_t local_app_finalized;
    pmix_pgpu_base_API_deregister_nspace_fn_t deregister_nspace;
    pmix_pgpu_base_module_collect_inventory_fn_t collect_inventory;
    pmix_pgpu_base_module_deliver_inventory_fn_t deliver_inventory;
} pmix_pgpu_API_module_t;

/* declare the global APIs */
PMIX_EXPORT extern pmix_pgpu_API_module_t pmix_pgpu;

/*
 * the standard component data structure
 */
typedef pmix_mca_base_component_t pmix_pgpu_base_component_t;

/*
 * Macro for use in components that are of type pgpu
 */
#define PMIX_PGPU_BASE_VERSION_1_0_0 PMIX_MCA_BASE_VERSION_1_0_0("pgpu", 1, 0, 0)

END_C_DECLS

#endif
