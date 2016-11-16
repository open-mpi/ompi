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
#include "orte/mca/schizo/base/base.h"

int orte_schizo_base_parse_cli(char *personality,
                               int argc, int start, char **argv)
{
    int rc;
    orte_schizo_base_active_module_t *mod;

    if (NULL == personality) {
        return ORTE_ERR_NOT_SUPPORTED;
    }

    OPAL_LIST_FOREACH(mod, &orte_schizo_base.active_modules, orte_schizo_base_active_module_t) {
        if (0 == strcmp(personality, mod->component->mca_component_name)) {
            if (NULL != mod->module->parse_cli) {
                rc = mod->module->parse_cli(personality, argc, start, argv);
                return rc;
            }
        }
    }
    return ORTE_ERR_NOT_SUPPORTED;
}

int orte_schizo_base_parse_env(char *personality,
                               char *path,
                               opal_cmd_line_t *cmd_line,
                               char **srcenv,
                               char ***dstenv)
{
    int rc;
    orte_schizo_base_active_module_t *mod;

    OPAL_LIST_FOREACH(mod, &orte_schizo_base.active_modules, orte_schizo_base_active_module_t) {
        if (0 == strcmp(personality, mod->component->mca_component_name)) {
            if (NULL != mod->module->parse_env) {
                rc = mod->module->parse_env(personality, path, cmd_line, srcenv, dstenv);
                return rc;
            }
        }
    }
    return ORTE_ERR_NOT_SUPPORTED;
}

int orte_schizo_base_setup_fork(orte_job_t *jdata,
                                orte_app_context_t *context)
{
    int rc;
    orte_schizo_base_active_module_t *mod;

    /* if no personality was specified, then nothing to do */
    if (NULL == jdata->personality) {
        return ORTE_SUCCESS;
    }

    OPAL_LIST_FOREACH(mod, &orte_schizo_base.active_modules, orte_schizo_base_active_module_t) {
        if (0 == strcmp(jdata->personality, mod->component->mca_component_name)) {
            if (NULL != mod->module->setup_fork) {
                rc = mod->module->setup_fork(jdata, context);
                return rc;
            }
        }
    }
    return ORTE_ERR_NOT_SUPPORTED;
}

int orte_schizo_base_setup_child(orte_job_t *jdata,
                                 orte_proc_t *child,
                                 orte_app_context_t *app)
{
    int rc;
    orte_schizo_base_active_module_t *mod;

    /* if no personality was specified, then nothing to do */
    if (NULL == jdata->personality) {
        return ORTE_SUCCESS;
    }

    OPAL_LIST_FOREACH(mod, &orte_schizo_base.active_modules, orte_schizo_base_active_module_t) {
        if (0 == strcmp(jdata->personality, mod->component->mca_component_name)) {
            if (NULL != mod->module->setup_child) {
                rc = mod->module->setup_child(jdata, child, app);
                return rc;
            }
        }
    }
    return ORTE_ERR_NOT_SUPPORTED;
}
