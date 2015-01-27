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
typedef int (*orte_schizo_base_module_parse_cli_fn_t)(char *personality,
                                                      int argc, int start,
                                                      char **argv);

typedef int (*orte_schizo_base_module_parse_env_fn_t)(char *personality,
                                                      char *path,
                                                      opal_cmd_line_t *cmd_line,
                                                      char *server,
                                                      char **srcenv,
                                                      char ***dstenv);

typedef int (*orte_schizo_base_module_setup_fork_fn_t)(orte_job_t *jdata,
                                                       orte_app_context_t *context);

typedef int (*orte_schizo_base_module_setup_child_fn_t)(orte_job_t *jdata,
                                                        orte_proc_t *child,
                                                        orte_app_context_t *app);

/*
 * schizo module version 1.3.0
 */
typedef struct {
    orte_schizo_base_module_parse_cli_fn_t     parse_cli;
    orte_schizo_base_module_parse_env_fn_t     parse_env;
    orte_schizo_base_module_setup_fork_fn_t    setup_fork;
    orte_schizo_base_module_setup_child_fn_t   setup_child;
} orte_schizo_base_module_t;

ORTE_DECLSPEC extern orte_schizo_base_module_t orte_schizo;

/*
 * schizo component
 */
 
/**
 * schizo component version 1.3.0
 */
typedef struct {
    /** Base MCA structure */
    mca_base_component_t base_version;
    /** Base MCA data */
    mca_base_component_data_t base_data;
} orte_schizo_base_component_t;

/**
 * Macro for use in components that are of type schizo
 */
#define MCA_SCHIZO_BASE_VERSION_1_0_0 \
  MCA_BASE_VERSION_2_0_0, \
  "schizo", 1, 0, 0


END_C_DECLS

#endif
