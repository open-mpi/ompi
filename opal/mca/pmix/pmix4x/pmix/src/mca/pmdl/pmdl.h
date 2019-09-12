/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2007-2008 Cisco Systems, Inc.  All rights reserved.
 *
 * Copyright (c) 2015-2018 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018-2019 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 * This interface is for use by PMIx servers to obtain network-related info
 * such as security keys that need to be shared across applications, and to
 * setup network support for applications prior to launch
 *
 * Available plugins may be defined at runtime via the typical MCA parameter
 * syntax.
 */

#ifndef PMIX_PMDL_H
#define PMIX_PMDL_H

#include <src/include/pmix_config.h>
#include <pmix_sched.h>

#include "src/class/pmix_list.h"
#include "src/mca/mca.h"
#include "src/mca/base/pmix_mca_base_var.h"
#include "src/mca/base/pmix_mca_base_framework.h"
#include "src/include/pmix_globals.h"
#include "src/server/pmix_server_ops.h"

BEGIN_C_DECLS

/******    MODULE DEFINITION    ******/

/**
 * Initialize the module. Returns an error if the module cannot
 * run, success if it can and wants to be used.
 */
typedef pmix_status_t (*pmix_pmdl_base_module_init_fn_t)(void);

/**
 * Finalize the module. Tear down any allocated storage, disconnect
 * from any system support (e.g., LDAP server)
 */
typedef void (*pmix_pmdl_base_module_fini_fn_t)(void);

/* Harvest envars for this programming model so they can be forwarded
 * to backend processes */
typedef pmix_status_t (*pmix_pmdl_base_module_harvest_envars_fn_t)(pmix_namespace_t *nptr,
                                                                   pmix_info_t info[], size_t ninfo,
                                                                   pmix_list_t *ilist);
/**
 * Setup any programming model specific support for the given nspace
 */
typedef pmix_status_t (*pmix_pmdl_base_module_setup_ns_fn_t)(pmix_namespace_t *nptr,
                                                             uint32_t appnum,
                                                             pmix_info_t *info);
typedef pmix_status_t (*pmix_pmdl_base_module_setup_ns_kv_fn_t)(pmix_namespace_t *nptr,
                                                                uint32_t appnum,
                                                                pmix_kval_t *kv);

/**
 * Setup any programming model specific support for the given client */
typedef pmix_status_t (*pmix_pmdl_base_module_setup_client_fn_t)(pmix_namespace_t *nptr,
                                                                 pmix_rank_t rank,
                                                                 uint32_t apppnum);

/**
 * Give the plugins an opportunity to add any envars to the
 * environment of a local application process prior to fork/exec
 */
typedef pmix_status_t (*pmix_pmdl_base_module_setup_fork_fn_t)(const pmix_proc_t *peer, char ***env);

/**
 * Provide an opportunity for the fabric components to cleanup any
 * resources they may have created to track the nspace
 */
typedef void (*pmix_pmdl_base_module_dregister_nspace_fn_t)(pmix_namespace_t *nptr);

/**
 * Base structure for a PMDL module. Each component should malloc a
 * copy of the module structure for each fabric plane they support.
 */
typedef struct {
    char *name;
    pmix_pmdl_base_module_init_fn_t                 init;
    pmix_pmdl_base_module_fini_fn_t                 finalize;
    pmix_pmdl_base_module_harvest_envars_fn_t       harvest_envars;
    pmix_pmdl_base_module_setup_ns_fn_t             setup_nspace;
    pmix_pmdl_base_module_setup_ns_kv_fn_t          setup_nspace_kv;
    pmix_pmdl_base_module_setup_client_fn_t         setup_client;
    pmix_pmdl_base_module_setup_fork_fn_t           setup_fork;
    pmix_pmdl_base_module_dregister_nspace_fn_t     deregister_nspace;
} pmix_pmdl_module_t;

/* define a public API */

typedef pmix_status_t (*pmix_pmdl_base_API_harvest_envars_fn_t)(char *nspace,
                                                                pmix_info_t info[], size_t ninfo,
                                                                pmix_list_t *ilist);
typedef void (*pmix_pmdl_base_API_dregister_nspace_fn_t)(const char *nptr);
typedef struct {
    char *name;
    pmix_pmdl_base_module_init_fn_t                 init;
    pmix_pmdl_base_module_fini_fn_t                 finalize;
    pmix_pmdl_base_API_harvest_envars_fn_t          harvest_envars;
    pmix_pmdl_base_module_setup_ns_fn_t             setup_nspace;
    pmix_pmdl_base_module_setup_ns_kv_fn_t          setup_nspace_kv;
    pmix_pmdl_base_module_setup_client_fn_t         setup_client;
    pmix_pmdl_base_module_setup_fork_fn_t           setup_fork;
    pmix_pmdl_base_API_dregister_nspace_fn_t        deregister_nspace;
} pmix_pmdl_API_module_t;


/* declare the global APIs */
PMIX_EXPORT extern pmix_pmdl_API_module_t pmix_pmdl;

/*
 * the standard component data structure
 */
struct pmix_pmdl_base_component_t {
    pmix_mca_base_component_t                        base;
    pmix_mca_base_component_data_t                   data;
};
typedef struct pmix_pmdl_base_component_t pmix_pmdl_base_component_t;

/*
 * Macro for use in components that are of type pmdl
 */
#define PMIX_PMDL_BASE_VERSION_1_0_0 \
    PMIX_MCA_BASE_VERSION_1_0_0("pmdl", 1, 0, 0)

END_C_DECLS

#endif
