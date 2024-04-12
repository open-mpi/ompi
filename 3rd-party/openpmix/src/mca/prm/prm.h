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
 * This interface is for use by PMIx servers to interpret/translate/interact
 * from/to/with their host environment
 *
 * Available plugins may be defined at runtime via the typical MCA parameter
 * syntax.
 */

#ifndef PMIX_PRM_H
#define PMIX_PRM_H

#include "src/include/pmix_config.h"
#include "pmix_common.h"

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
typedef pmix_status_t (*pmix_prm_base_module_init_fn_t)(void);

/**
 * Finalize the module. Tear down any allocated storage, disconnect
 * from any system support (e.g., LDAP server)
 */
typedef void (*pmix_prm_base_module_fini_fn_t)(void);

/**
 * Allocate, deallocate, modify allocation request
 */
typedef pmix_status_t (*pmix_prm_base_module_alloc_fn_t)(pmix_alloc_directive_t directive,
                                                         pmix_info_t *info, size_t ninfo,
                                                         pmix_info_t **results, size_t *nresults);


/**
 * Pass an event to the host system for transport. If the host system
 * has called PMIx_server_init and provided an entry for the event
 * notification upcall, then the default plugin will execute that
 * pathway. However, some systems have chosen to utilize a "backdoor"
 * channel for transporting the event - e.g., by calling some RM-provided
 * API to inject the event into their transport.
 */
typedef pmix_status_t (*pmix_prm_base_module_notify_fn_t)(pmix_status_t status,
                                                          const pmix_proc_t *source,
                                                          pmix_data_range_t range,
                                                          const pmix_info_t info[], size_t ninfo,
                                                          pmix_op_cbfunc_t cbfunc, void *cbdata);

/* request time remaining in this allocation */
typedef pmix_status_t (*pmix_prm_base_module_get_rem_time_fn_t)(uint32_t *timeleft);

/**
 * Base structure for a PRM module. Each component should malloc a
 * copy of the module structure for each fabric plane they support.
 */
typedef struct {
    char *name;
    /* init/finalize */
    pmix_prm_base_module_init_fn_t                 init;
    pmix_prm_base_module_fini_fn_t                 finalize;
    pmix_prm_base_module_alloc_fn_t                allocate;
    pmix_prm_base_module_notify_fn_t               notify;
    pmix_prm_base_module_get_rem_time_fn_t         get_remaining_time;
} pmix_prm_module_t;


/* declare the global APIs */
PMIX_EXPORT extern pmix_prm_module_t pmix_prm;

/*
 * the standard component data structure
 */
typedef pmix_mca_base_component_t pmix_prm_base_component_t;

/*
 * Macro for use in components that are of type prm
 */
#define PMIX_PRM_BASE_VERSION_1_0_0 PMIX_MCA_BASE_VERSION_1_0_0("prm", 1, 0, 0)

END_C_DECLS

#endif
