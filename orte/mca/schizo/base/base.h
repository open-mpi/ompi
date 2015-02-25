/*
 * Copyright (c) 2015      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 * schizo framework base functionality.
 */

#ifndef ORTE_MCA_SCHIZO_BASE_H
#define ORTE_MCA_SCHIZO_BASE_H

/*
 * includes
 */
#include "orte_config.h"
#include "orte/types.h"

#include "opal/class/opal_list.h"
#include "opal/mca/mca.h"

#include "orte/runtime/orte_globals.h"

#include "orte/mca/schizo/schizo.h"

BEGIN_C_DECLS

/*
 * MCA Framework
 */
ORTE_DECLSPEC extern mca_base_framework_t orte_schizo_base_framework;
/* select all components */
ORTE_DECLSPEC    int orte_schizo_base_select(void);

/**
 * Struct to hold data global to the schizo framework
 */
typedef struct {
    /* list of active modules */
    opal_list_t active_modules;
} orte_schizo_base_t;

/**
 * Global instance of schizo-wide framework data
 */
ORTE_DECLSPEC extern orte_schizo_base_t orte_schizo_base;

/**
 * Active schizo component / module
 */
typedef struct {
    opal_list_item_t super;
    int pri;
    orte_schizo_base_module_t *module;
    mca_base_component_t *component;
} orte_schizo_base_active_module_t;
OBJ_CLASS_DECLARATION(orte_schizo_base_active_module_t);

/* the base stub functions */
ORTE_DECLSPEC int orte_schizo_base_parse_cli(char *personality,
                                             int argc, int start, char **argv);
ORTE_DECLSPEC int orte_schizo_base_parse_env(char *personality,
                                             char *path,
                                             opal_cmd_line_t *cmd_line,
                                             char *server,
                                             char **srcenv,
                                             char ***dstenv);
ORTE_DECLSPEC int orte_schizo_base_setup_fork(orte_job_t *jdata,
                                              orte_app_context_t *context);
ORTE_DECLSPEC int orte_schizo_base_setup_child(orte_job_t *jobdat,
                                               orte_proc_t *child,
                                               orte_app_context_t *app);

END_C_DECLS

#endif
