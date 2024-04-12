/*
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      IBM Corporation.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 * schizo framework base functionality.
 */

#ifndef PRTE_MCA_SCHIZO_BASE_H
#define PRTE_MCA_SCHIZO_BASE_H

/*
 * includes
 */
#include "prte_config.h"
#include "types.h"

#include "src/class/pmix_list.h"
#include "src/mca/base/pmix_mca_base_framework.h"
#include "src/mca/mca.h"
#include "src/util/pmix_cmd_line.h"
#include "src/util/pmix_printf.h"

#include "src/runtime/prte_globals.h"

#include "src/mca/schizo/schizo.h"

BEGIN_C_DECLS

/*
 * MCA Framework
 */
PRTE_EXPORT extern pmix_mca_base_framework_t prte_schizo_base_framework;
/* select all components */
PRTE_EXPORT int prte_schizo_base_select(void);

/**
 * Struct to hold data global to the schizo framework
 */
typedef struct {
    /* list of active modules */
    pmix_list_t active_modules;
    bool test_proxy_launch;
} prte_schizo_base_t;

/**
 * Global instance of schizo-wide framework data
 */
PRTE_EXPORT extern prte_schizo_base_t prte_schizo_base;

/**
 * Active schizo component / module
 */
typedef struct {
    pmix_list_item_t super;
    int pri;
    prte_schizo_base_module_t *module;
    pmix_mca_base_component_t *component;
} prte_schizo_base_active_module_t;
PMIX_CLASS_DECLARATION(prte_schizo_base_active_module_t);

typedef struct {
    char *name;
    char **conflicts;
} prte_schizo_conflicts_t;

/* base support functions */
PRTE_EXPORT int prte_schizo_base_convert(pmix_cli_result_t *results,
                                         char *deprecated,
                                         char *key, char *option,
                                         prte_schizo_conflicts_t *conflicts,
                                         bool report);

/* the base stub functions */

PRTE_EXPORT prte_schizo_base_module_t *prte_schizo_base_detect_proxy(char *cmdpath);

PRTE_EXPORT void prte_schizo_base_root_error_msg(void);
PRTE_EXPORT char *prte_schizo_base_getline(FILE *fp);
PRTE_EXPORT char *prte_schizo_base_strip_quotes(char *p);
PRTE_EXPORT int prte_schizo_base_parse_prte(int argc, int start, char **argv, char ***target);
PRTE_EXPORT int prte_schizo_base_parse_pmix(int argc, int start, char **argv, char ***target);
PRTE_EXPORT int prte_schizo_base_sanity(pmix_cli_result_t *cmd_line);
PRTE_EXPORT bool prte_schizo_base_check_directives(char *directive,
                                                   char **valid,
                                                   char **quals,
                                                   char *dir);
PRTE_EXPORT bool prte_schizo_base_check_qualifiers(char *directive,
                                                   char **valid,
                                                   char *qual);
PRTE_EXPORT bool prte_schizo_base_check_prte_param(char *param);
PRTE_EXPORT bool prte_schizo_base_check_pmix_param(char *param);
PRTE_EXPORT void prte_schizo_base_expose(char *param, char *prefix);
PRTE_EXPORT int prte_schizo_base_add_directive(pmix_cli_result_t *results,
                                               const char *deprecated, const char *target,
                                               char *directive, bool report);
PRTE_EXPORT int prte_schizo_base_add_qualifier(pmix_cli_result_t *results,
                                               char *deprecated, char *target,
                                               char *qualifier, bool report);
PRTE_EXPORT int prte_schizo_base_parse_output(pmix_cli_item_t *opt, void *jinfo);
PRTE_EXPORT int prte_schizo_base_parse_display(pmix_cli_item_t *opt, void *jinfo);

PRTE_EXPORT int prte_schizo_base_setup_fork(prte_job_t *jdata, prte_app_context_t *app);


END_C_DECLS

#endif
