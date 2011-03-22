/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All Rights Reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_DEBUGGER_H
#define MCA_DEBUGGER_H

/*
 * includes
 */

#include "orte_config.h"

#include "opal/mca/mca.h"

#include "orte/runtime/orte_globals.h"

BEGIN_C_DECLS

/*
 * Component functions - all MUST be provided!
 */

/* initialize the selected module */
typedef int (*orte_debugger_base_module_init_fn_t)(void);
    
/* finalize the selected module */
typedef void (*orte_debugger_base_module_finalize_fn_t)(void);

/* init debuggers before spawn */
typedef void (*orte_debugger_base_module_init_before_spawn_fn_t)(orte_job_t *jdata);

/* init debuggers after spawn */
typedef void (*orte_debugger_base_module_init_after_spawn_fn_t)(orte_job_t *jdata);

/*
 * Ver 1.0
 */
struct orte_debugger_base_module_1_0_0_t {
    orte_debugger_base_module_init_fn_t                 init;
    orte_debugger_base_module_finalize_fn_t             finalize;
    orte_debugger_base_module_init_before_spawn_fn_t    init_before_spawn;
    orte_debugger_base_module_init_after_spawn_fn_t     init_after_spawn;
};

typedef struct orte_debugger_base_module_1_0_0_t orte_debugger_base_module_1_0_0_t;
typedef orte_debugger_base_module_1_0_0_t orte_debugger_base_module_t;

ORTE_DECLSPEC extern orte_debugger_base_module_t orte_debugger;

/*
 * the standard component data structure
 */
struct orte_debugger_base_component_1_0_0_t {
    mca_base_component_t base_version;
    mca_base_component_data_t base_data;
};
typedef struct orte_debugger_base_component_1_0_0_t orte_debugger_base_component_1_0_0_t;
typedef orte_debugger_base_component_1_0_0_t orte_debugger_base_component_t;

/*
 * Macro for use in components that are of type debugger v1.0.0
 */
#define ORTE_DEBUGGER_BASE_VERSION_1_0_0 \
  /* debugger v1.0 is chained to MCA v2.0 */ \
  MCA_BASE_VERSION_2_0_0, \
  /* debugger v1.0 */ \
  "debugger", 1, 0, 0

END_C_DECLS

#endif /* MCA_DEBUGGER_H */
