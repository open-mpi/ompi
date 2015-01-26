/*
 * Copyright (c) 2015      Intel, Inc. All rights reserved
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 *
 * The Open RTE Personality Framework (schizo)
 *
 * Multi-select framework so that multiple personalities can be
 * simultaneously supported
 * 
 */

#ifndef ORTE_MCA_SCHIZO_H
#define ORTE_MCA_SCHIZO_H

#include "orte_config.h"
#include "orte/types.h"

#include "opal/mca/mca.h"

#include "orte/runtime/orte_globals.h"


BEGIN_C_DECLS

/*
 * schizo module functions
 */

/**
* SCHIZO module functions - the modules are accessed via
* the base stub functions
*/
typedef int (*orte_schizo_base_module_parse_cli_fn_t)();

typedef int (*orte_schizo_base_module_parse_env_fn_t)(char ***env);

typedef int (*orte_schizo_base_module_prep_spawn_fn_t)(orte_job_t *jdata,
                                                       orte_proc_t *proc);

/*
 * schizo module version 1.3.0
 */
struct orte_schizo_base_module_1_3_0_t {
    /** Mapping function pointer */
    orte_schizo_base_module_map_fn_t         	map_job;
};
/** Convenience typedef */
typedef struct orte_schizo_base_module_1_3_0_t orte_schizo_base_module_1_3_0_t;
/** Convenience typedef */
typedef orte_schizo_base_module_1_3_0_t orte_schizo_base_module_t;


/*
 * schizo component
 */
 
/**
 * schizo component version 1.3.0
 */
struct orte_schizo_base_component_2_0_0_t {
    /** Base MCA structure */
    mca_base_component_t base_version;
    /** Base MCA data */
    mca_base_component_data_t base_data;
};
/** Convenience typedef */
typedef struct orte_schizo_base_component_2_0_0_t orte_schizo_base_component_2_0_0_t;
/** Convenience typedef */
typedef orte_schizo_base_component_2_0_0_t orte_schizo_base_component_t;


END_C_DECLS

#endif
