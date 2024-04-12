/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2020      IBM Corporation.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2022      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 *
 * The PRTE Personality Framework (schizo)
 *
 * Multi-select framework so that multiple personalities can be
 * simultaneously supported
 *
 */

#ifndef PRTE_MCA_SCHIZO_H
#define PRTE_MCA_SCHIZO_H

#include "prte_config.h"
#include "types.h"

#include "src/class/pmix_list.h"
#include "src/hwloc/hwloc-internal.h"
#include "src/mca/rmaps/rmaps_types.h"
#include "src/pmix/pmix-internal.h"
#include "src/util/pmix_cmd_line.h"

#include "src/mca/mca.h"

#include "src/runtime/prte_globals.h"

BEGIN_C_DECLS

typedef int (*prte_schizo_convertor_fn_t)(char *option, char ***argv, int idx);

/*
 * schizo module functions
 */

/**
 * SCHIZO module functions - the modules are accessed via
 * the base stub functions
 */

/* initialize the module - allow it to do whatever one-time
 * things it requires */
typedef int (*prte_schizo_base_module_init_fn_t)(void);

/* parse a tool command line */
typedef int (*prte_schizo_base_module_parse_cli_fn_t)(char **argv,
                                                      pmix_cli_result_t *results,
                                                      bool silent);

/* detect if we are running as a proxy
 * Check the environment to determine what, if any, host we are running
 * under. Check the argv to see if we are running as a proxy for some
 * other command and to see which environment we are proxying. Return
 * a priority indicating the level of confidence this component has
 * that it is the proxy, with 100 being a definitive "yes". Highest
 * confidence wins.
 */
typedef int (*prte_schizo_base_detect_proxy_fn_t)(char *cmdpath);

/* parse the environment of the
 * tool to extract any personality-specific envars that need to be
 * forward to the app's environment upon execution */
typedef int (*prte_schizo_base_module_parse_env_fn_t)(char **srcenv,
                                                      char ***dstenv,
                                                      pmix_cli_result_t *cli);

/* check if running as root is allowed in this environment */
typedef void (*prte_schizo_base_module_allow_run_as_root_fn_t)(pmix_cli_result_t *results);

/* Set the default mapping policy for a job */
typedef int (*prte_schizo_base_module_set_default_mapping_fn_t)(prte_job_t *jdata,
                                                                prte_rmaps_options_t *options);

typedef int (*prte_schizo_base_module_set_default_ranking_fn_t)(prte_job_t *jdata,
                                                                prte_rmaps_options_t *options);

typedef int (*prte_schizo_base_module_set_default_binding_fn_t)(prte_job_t *jdata,
                                                                prte_rmaps_options_t *options);

typedef int (*prte_schizo_base_module_set_default_rto_fn_t)(prte_job_t *jdata,
                                                            prte_rmaps_options_t *options);

/* do whatever preparation work
 * is required to setup the app for execution. This is intended to be
 * used by prun and other launcher tools to, for example, change
 * an executable's relative-path to an absolute-path, or add a command
 * required for starting a particular kind of application (e.g., adding
 * "java" to start a Java application) */
typedef int (*prte_schizo_base_module_setup_app_fn_t)(prte_pmix_app_t *app);

/* add any personality-specific envars required at the job level prior
 * to beginning to execute local procs */
typedef int (*prte_schizo_base_module_setup_fork_fn_t)(prte_job_t *jdata,
                                                       prte_app_context_t *context);

/* give the component a chance to cleanup */
typedef void (*prte_schizo_base_module_finalize_fn_t)(void);

/* give the components a chance to add job info */
typedef void (*prte_schizo_base_module_job_info_fn_t)(pmix_cli_result_t *results,
                                                      void *jobinfo);

/* give the component a chance to validate directives and their values */
typedef int (*prte_schizo_base_module_check_sanity_fn_t)(pmix_cli_result_t *cmd_line);

/*
 * schizo module version 1.3.0
 */
typedef struct {
    char *name;
    prte_schizo_base_module_init_fn_t                   init;
    prte_schizo_base_module_parse_cli_fn_t              parse_cli;
    prte_schizo_base_module_parse_env_fn_t              parse_env;
    prte_schizo_base_detect_proxy_fn_t                  detect_proxy;
    prte_schizo_base_module_allow_run_as_root_fn_t      allow_run_as_root;
    prte_schizo_base_module_set_default_mapping_fn_t    set_default_mapping;
    prte_schizo_base_module_set_default_ranking_fn_t    set_default_ranking;
    prte_schizo_base_module_set_default_binding_fn_t    set_default_binding;
    prte_schizo_base_module_set_default_rto_fn_t        set_default_rto;
    prte_schizo_base_module_setup_app_fn_t              setup_app;
    prte_schizo_base_module_setup_fork_fn_t             setup_fork;
    prte_schizo_base_module_job_info_fn_t               job_info;
    prte_schizo_base_module_check_sanity_fn_t           check_sanity;
    prte_schizo_base_module_finalize_fn_t               finalize;
} prte_schizo_base_module_t;

/*
 * schizo component
 */

/**
 * schizo component version 1.3.0
 */
typedef pmix_mca_base_component_t prte_schizo_base_component_t;

/**
 * Macro for use in components that are of type schizo
 */
#define PRTE_MCA_SCHIZO_BASE_VERSION_1_0_0 PRTE_MCA_BASE_VERSION_3_0_0("schizo", 1, 0, 0)

END_C_DECLS

#endif
