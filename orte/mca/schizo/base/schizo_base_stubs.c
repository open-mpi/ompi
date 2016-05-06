/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2015-2016 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/constants.h"

#include "opal/class/opal_list.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"
#include "orte/mca/schizo/base/base.h"

 const char* orte_schizo_base_print_env(orte_schizo_launch_environ_t env)
 {
    switch(env) {
        case ORTE_SCHIZO_UNDETERMINED:
            return "UNDETERMINED";
        case ORTE_SCHIZO_NATIVE_LAUNCHED:
            return "NATIVE_LAUNCHED";
        case ORTE_SCHIZO_UNMANAGED_SINGLETON:
            return "UNMANAGED_SINGLETON";
        case ORTE_SCHIZO_DIRECT_LAUNCHED:
            return "DIRECT_LAUNCHED";
        case ORTE_SCHIZO_MANAGED_SINGLETON:
            return "MANAGED_SINGLETON";
        default:
            return "INVALID_CODE";
    }
}

int orte_schizo_base_parse_cli(char **personality,
                               int argc, int start, char **argv)
{
    int rc;
    orte_schizo_base_active_module_t *mod;

    if (NULL == personality) {
        return ORTE_ERR_NOT_SUPPORTED;
    }

    OPAL_LIST_FOREACH(mod, &orte_schizo_base.active_modules, orte_schizo_base_active_module_t) {
        if (NULL != mod->module->parse_cli) {
            rc = mod->module->parse_cli(personality, argc, start, argv);
            if (ORTE_SUCCESS != rc && ORTE_ERR_TAKE_NEXT_OPTION != rc) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }
    return ORTE_SUCCESS;
}

int orte_schizo_base_parse_env(char **personality,
                               char *path,
                               opal_cmd_line_t *cmd_line,
                               char **srcenv,
                               char ***dstenv)
{
    int rc;
    orte_schizo_base_active_module_t *mod;

    OPAL_LIST_FOREACH(mod, &orte_schizo_base.active_modules, orte_schizo_base_active_module_t) {
        if (NULL != mod->module->parse_env) {
            rc = mod->module->parse_env(personality, path, cmd_line, srcenv, dstenv);
            if (ORTE_SUCCESS != rc && ORTE_ERR_TAKE_NEXT_OPTION != rc) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }
    return ORTE_SUCCESS;
}

int orte_schizo_base_setup_app(char **personality,
                               orte_app_context_t *app)
{
    int rc;
    orte_schizo_base_active_module_t *mod;

    OPAL_LIST_FOREACH(mod, &orte_schizo_base.active_modules, orte_schizo_base_active_module_t) {
        if (NULL != mod->module->setup_app) {
            rc = mod->module->setup_app(personality, app);
            if (ORTE_SUCCESS != rc && ORTE_ERR_TAKE_NEXT_OPTION != rc) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }
    return ORTE_SUCCESS;
}

int orte_schizo_base_setup_fork(orte_job_t *jdata,
                                orte_app_context_t *context)
{
    int rc;
    orte_schizo_base_active_module_t *mod;

    OPAL_LIST_FOREACH(mod, &orte_schizo_base.active_modules, orte_schizo_base_active_module_t) {
        if (NULL != mod->module->setup_fork) {
            rc = mod->module->setup_fork(jdata, context);
            if (ORTE_SUCCESS != rc && ORTE_ERR_TAKE_NEXT_OPTION != rc) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }
    return ORTE_SUCCESS;
}

int orte_schizo_base_setup_child(orte_job_t *jdata,
                                 orte_proc_t *child,
                                 orte_app_context_t *app)
{
    int rc;
    orte_schizo_base_active_module_t *mod;

    OPAL_LIST_FOREACH(mod, &orte_schizo_base.active_modules, orte_schizo_base_active_module_t) {
        if (NULL != mod->module->setup_child) {
            rc = mod->module->setup_child(jdata, child, app);
            if (ORTE_SUCCESS != rc && ORTE_ERR_TAKE_NEXT_OPTION != rc) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }
    return ORTE_SUCCESS;
}

orte_schizo_launch_environ_t orte_schizo_base_check_launch_environment(void)
{
    orte_schizo_launch_environ_t rc;
    orte_schizo_base_active_module_t *mod;

    OPAL_LIST_FOREACH(mod, &orte_schizo_base.active_modules, orte_schizo_base_active_module_t) {
        if (NULL != mod->module->check_launch_environment) {
            rc = mod->module->check_launch_environment();
            if (ORTE_SCHIZO_UNDETERMINED != rc) {
                return rc;
            }
        }
    }
    return ORTE_SCHIZO_UNDETERMINED;
}

void orte_schizo_base_finalize(void)
{
    orte_schizo_base_active_module_t *mod;

    OPAL_LIST_FOREACH(mod, &orte_schizo_base.active_modules, orte_schizo_base_active_module_t) {
        if (NULL != mod->module->finalize) {
            mod->module->finalize();
        }
    }
}
