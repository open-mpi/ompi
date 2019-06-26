/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2007-2008 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015-2019 Intel, Inc.  All rights reserved.
 *
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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

#ifndef PMIX_PNET_H
#define PMIX_PNET_H

#include <src/include/pmix_config.h>

#include "src/class/pmix_list.h"
#include "src/mca/mca.h"
#include "src/mca/base/pmix_mca_base_var.h"
#include "src/mca/base/pmix_mca_base_framework.h"
#include "src/include/pmix_globals.h"

BEGIN_C_DECLS

/******    MODULE DEFINITION    ******/

/**
 * Initialize the module. Returns an error if the module cannot
 * run, success if it can and wants to be used.
 */
typedef pmix_status_t (*pmix_pnet_base_module_init_fn_t)(void);

/**
 * Finalize the module. Tear down any allocated storage, disconnect
 * from any system support (e.g., LDAP server)
 */
typedef void (*pmix_pnet_base_module_fini_fn_t)(void);

/**
 * Provide an opportunity for the network to define values that
 * are to be passed to an application. This can include security
 * tokens required for application processes to communicate with
 * each other
 */
typedef pmix_status_t (*pmix_pnet_base_module_setup_app_fn_t)(char *nspace, pmix_list_t *ilist);

/**
 * Give the local network library an opportunity to setup address information
 * for the application by passing in the layout type and a regex describing
 * the layout */
typedef pmix_status_t (*pmix_pnet_base_module_setup_local_net_fn_t)(char *nspace,
                                                                    pmix_info_t info[],
                                                                    size_t ninfo);

/**
 * Give the local network library an opportunity to add any envars to the
 * environment of a local application process prior to fork/exec
 */
typedef pmix_status_t (*pmix_pnet_base_module_setup_fork_fn_t)(const pmix_proc_t *peer, char ***env);

/**
 * Provide an opportunity for the local network library to cleanup when a
 * local application process terminates
 */
typedef void (*pmix_pnet_base_module_child_finalized_fn_t)(pmix_peer_t *peer);

/**
 * Provide  an opportunity for the local network library to cleanup after
 * all local clients for a given application have terminated
 */
typedef void (*pmix_pnet_base_module_local_app_finalized_fn_t)(char *nspace);

/**
 * Base structure for a PNET module
 */
typedef struct {
    char *name;
    /* init/finalize */
    pmix_pnet_base_module_init_fn_t                 init;
    pmix_pnet_base_module_fini_fn_t                 finalize;
    pmix_pnet_base_module_setup_app_fn_t            setup_app;
    pmix_pnet_base_module_setup_local_net_fn_t      setup_local_network;
    pmix_pnet_base_module_setup_fork_fn_t           setup_fork;
    pmix_pnet_base_module_child_finalized_fn_t      child_finalized;
    pmix_pnet_base_module_local_app_finalized_fn_t  local_app_finalized;
} pmix_pnet_module_t;

/* declare the global APIs */
PMIX_EXPORT extern pmix_pnet_module_t pmix_pnet;

/*
 * the standard component data structure
 */
struct pmix_pnet_base_component_t {
    pmix_mca_base_component_t                        base;
    pmix_mca_base_component_data_t                   data;
};
typedef struct pmix_pnet_base_component_t pmix_pnet_base_component_t;

/*
 * Macro for use in components that are of type pnet
 */
#define PMIX_PNET_BASE_VERSION_1_0_0 \
    PMIX_MCA_BASE_VERSION_1_0_0("pnet", 1, 0, 0)

END_C_DECLS

#endif
