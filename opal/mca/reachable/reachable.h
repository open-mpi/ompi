/*
 * Copyright (c) 2014-2015 Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_REACHABLE_H
#define OPAL_REACHABLE_H

#include "opal_config.h"
#include "opal/types.h"

#include "opal/mca/mca.h"
#include "opal/mca/if/if.h"

BEGIN_C_DECLS


/* Init */
typedef int (*opal_reachable_base_module_init_fn_t)(void);

/* Finalize */
typedef int (*opal_reachable_base_module_fini_fn_t)(void);

/* Given a list of local interfaces and a list of remote
 * interfaces, return the interface that is the "best"
 * for connecting to the remote process.
 *
 * local_if: list of local opal_if_t interfaces
 * remote_if: list of opal_if_t interfaces for the remote
 *            process
 *
 * return value: pointer to opal_if_t on local_if that is
 *               the "best" option for connecting. NULL
 *               indicates that the remote process cannot
 *               be reached on any interface
 */
typedef opal_if_t*
(*opal_reachable_base_module_reachable_fn_t)(opal_list_t *local_if,
                                             opal_list_t *remote_if);


/*
 * the standard public API data structure
 */
typedef struct {
    /* currently used APIs */
    opal_reachable_base_module_init_fn_t                   init;
    opal_reachable_base_module_fini_fn_t                   finalize;
    opal_reachable_base_module_reachable_fn_t              reachable;
} opal_reachable_base_module_t;

typedef struct {
    mca_base_component_t                      base_version;
    mca_base_component_data_t                 base_data;
    int priority;
} opal_reachable_base_component_t;

/*
 * Macro for use in components that are of type reachable
 */
#define OPAL_REACHABLE_BASE_VERSION_2_0_0 \
  MCA_BASE_VERSION_2_0_0, \
  "reachable", 2, 0, 0

/* Global structure for accessing reachability functions */
OPAL_DECLSPEC extern opal_reachable_base_module_t opal_reachable;


END_C_DECLS

#endif
