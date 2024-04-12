/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2022 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2015 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014      Hochschule Esslingen.  All rights reserved.
 * Copyright (c) 2016-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2023      Triad National Security, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "pmix_common.h"
#include "src/class/pmix_list.h"
#include "src/mca/base/pmix_base.h"
#include "src/mca/mca.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_show_help.h"

/*
 * Local functions
 */
static int open_components(pmix_mca_base_framework_t *framework);

struct pmix_mca_base_dummy_framework_list_item_t {
    pmix_list_item_t super;
    pmix_mca_base_framework_t framework;
};

typedef struct fc_pair {
    pmix_list_item_t li;
    char *framework_name;
    char *component_name;
} fc_pair_t;

PMIX_CLASS_DECLARATION(fc_pair_t);

typedef enum {
    SHOW_LOAD_ERRORS_ALL,
    SHOW_LOAD_ERRORS_INCLUDE,
    SHOW_LOAD_ERRORS_EXCLUDE,
    SHOW_LOAD_ERRORS_NONE,
} show_load_type_t;

static show_load_type_t show_load_errors = SHOW_LOAD_ERRORS_ALL;
static pmix_list_t show_load_errors_include = PMIX_LIST_STATIC_INIT;
static pmix_list_t show_load_errors_exclude = PMIX_LIST_STATIC_INIT;


static void fc_pair_constructor(struct fc_pair *obj)
{
    obj->framework_name = NULL;
    obj->component_name = NULL;
}

static void fc_pair_destructor(struct fc_pair *obj)
{
    free(obj->framework_name);
    obj->framework_name = NULL;
    free(obj->component_name);
    obj->component_name = NULL;
}

PMIX_CLASS_INSTANCE(fc_pair_t, pmix_list_item_t,
                    fc_pair_constructor,
                    fc_pair_destructor);

/*
 * Parse the content of the show_load_errors value
 *
 * Valid values:
 * - "all"
 * - "none"
 * - comma-delimited list of items, each of which is
 *   "framework[/component]"
 *
 * The comma-delimited list may be prefixed with a "^".
 */
int pmix_mca_base_show_load_errors_init(void)
{
    PMIX_CONSTRUCT(&show_load_errors_include, pmix_list_t);
    PMIX_CONSTRUCT(&show_load_errors_exclude, pmix_list_t);

    // Check to see if mca_base_component_show_load_errors is a
    // boolean value
    pmix_value_t value;
    PMIX_VALUE_LOAD(&value, pmix_mca_base_component_show_load_errors, PMIX_STRING);
    pmix_boolean_t ret = PMIx_Value_true(&value);
    PMIx_Value_destruct(&value);

    // Treat true values as a synonym for "all", and false values
    // as a synonym for "none".
    if (PMIX_BOOL_TRUE == ret) {
        show_load_errors = SHOW_LOAD_ERRORS_ALL;
    } else if (PMIX_BOOL_FALSE == ret) {
        show_load_errors = SHOW_LOAD_ERRORS_NONE;
    } else {
        // must not be a bool, so treat as a string
        if (0 == strcasecmp(pmix_mca_base_component_show_load_errors, "all")) {
            show_load_errors = SHOW_LOAD_ERRORS_ALL;
        } else if (0 == strcasecmp(pmix_mca_base_component_show_load_errors, "none")) {
            show_load_errors = SHOW_LOAD_ERRORS_NONE;
        } else {
            // We have a comma-delimited list of values.  Is it
            // "include"-style, or "exclude" style?
            size_t pos = 0;
            pmix_list_t *list = &show_load_errors_include;
            show_load_errors = SHOW_LOAD_ERRORS_INCLUDE;
            if ('^' == pmix_mca_base_component_show_load_errors[0]) {
                pos = 1;
                list = &show_load_errors_exclude;
                show_load_errors = SHOW_LOAD_ERRORS_EXCLUDE;
            }

            // Examine each of the values in the comma-delimited list.
            // Each value can be of the form "framework" or
            // "framework/component".
            char **values = PMIx_Argv_split(pmix_mca_base_component_show_load_errors + pos,
                                            ',');
            if (values == NULL) {
                ret = PMIX_ERROR;
                pmix_show_help("help-mca-base.txt",
                               "internal error during init", true,
                               __func__, __FILE__, __LINE__,
                               ret,
                               "Failed to argv split pmix_mca_base_component_show_load_errors");
                return ret;
            }

            char **split;
            int argc;
            fc_pair_t *fcp;
            for (int i = 0; values[i] != NULL; ++i) {
                split = PMIx_Argv_split(values[i], '/');
                if (NULL == split) {
                    ret = PMIX_ERROR;
                    pmix_show_help("help-mca-base.txt",
                                   "internal error during init", true,
                                   __func__, __FILE__, __LINE__,
                                   ret,
                                   "Failed to argv split pmix_mca_base_component_show_load_errors value");
                    return ret;
                }

                argc = PMIx_Argv_count(split);
                if (0 == argc) {
                    // This should never happen
                    ret = PMIX_ERROR;
                    pmix_show_help("help-mca-base.txt",
                                   "internal error during init", true,
                                   __func__, __FILE__, __LINE__,
                                   ret,
                                   "Argv split resulted in 0 tokens");
                    return ret;
                }

                if (0 == strlen(split[0])) {
                    // Empty entry (e.g., consecutive commas); silently
                    // skip it
                    continue;
                }

                if (argc > 2) {
                    ret = PMIX_ERR_BAD_PARAM;
                    pmix_show_help("help-mca-base.txt",
                                   "show_load_errors: too many /", true,
                                   values[i]);
                    return ret;
                }

                fcp = PMIX_NEW(fc_pair_t);
                if (NULL == fcp) {
                    ret = PMIX_ERR_OUT_OF_RESOURCE;
                    pmix_show_help("help-mca-base.txt",
                                   "internal error during init", true,
                                   __func__, __FILE__, __LINE__,
                                   ret,
                                   "Failed to alloc new fc_pair_t");
                    return ret;
                }

                fcp->framework_name = split[0];
                if (2 == argc) {
                    fcp->component_name = split[1];
                }

                pmix_list_append(list, &fcp->li);
            }
            PMIx_Argv_free(values);
        }
    }

    return PMIX_SUCCESS;
}


bool pmix_mca_base_show_load_errors(const char *framework_name,
                                    const char *component_name)
{
    if (SHOW_LOAD_ERRORS_ALL == show_load_errors) {
        return true;
    } else if (SHOW_LOAD_ERRORS_NONE == show_load_errors) {
        return false;
    }
    if (NULL == framework_name) {
        return false;
    }

    // If we get here, it means we have an include or exclude list.
    // Setup for what to do based on whether it's an include or
    // exclude list.
    pmix_list_t *list;
    bool value_if_match_found;

    if (SHOW_LOAD_ERRORS_INCLUDE == show_load_errors) {
        list = &show_load_errors_include;
        value_if_match_found = true;
    } else {
        list = &show_load_errors_exclude;
        value_if_match_found = false;
    }

    // See if the framework_name/component_name pair is found in the
    // active list.
    fc_pair_t *item;
    PMIX_LIST_FOREACH(item, list, fc_pair_t) {
        if (0 == strcmp(framework_name, item->framework_name)) {
            if (NULL == component_name) {
                // If there's no component name, then we're matching
                // all components in this framework.
                return value_if_match_found;
            } else if (0 == strcmp(component_name, item->component_name)) {
                // We matched both the framework *and* component name.
                return value_if_match_found;
            }
        }
    }

    // We didn't find a match.
    return !value_if_match_found;
}

int pmix_mca_base_show_load_errors_finalize(void)
{
    PMIX_DESTRUCT(&show_load_errors_include);
    PMIX_DESTRUCT(&show_load_errors_exclude);

    return PMIX_SUCCESS;
}

/**
 * Function for finding and opening either all MCA components, or the
 * one that was specifically requested via a MCA parameter.
 */
int pmix_mca_base_framework_components_open(pmix_mca_base_framework_t *framework,
                                            pmix_mca_base_open_flag_t flags)
{
    /* Open flags are not used at this time. Suppress compiler warning. */
    if (flags & PMIX_MCA_BASE_OPEN_FIND_COMPONENTS) {
        bool open_dso_components = !(flags & PMIX_MCA_BASE_OPEN_STATIC_ONLY);
        /* Find and load requested components */
        int ret = pmix_mca_base_component_find(NULL, framework, false, open_dso_components);
        if (PMIX_SUCCESS != ret) {
            return ret;
        }
    }

    /* Open all registered components */
    return open_components(framework);
}

/*
 * Traverse the entire list of found components (a list of
 * pmix_mca_base_component_t instances).  If the requested_component_names
 * array is empty, or the name of each component in the list of found
 * components is in the requested_components_array, try to open it.
 * If it opens, add it to the components_available list.
 */
static int open_components(pmix_mca_base_framework_t *framework)
{
    pmix_list_t *components = &framework->framework_components;
    int output_id = framework->framework_output;
    pmix_mca_base_component_list_item_t *cli, *next;
    int ret;

    /* If pmix_mca_base_framework_register_components was called with the MCA_BASE_COMPONENTS_ALL
       flag we need to trim down and close any extra components we do not want open */
    ret = pmix_mca_base_components_filter(framework);
    if (PMIX_SUCCESS != ret) {
        return ret;
    }

    /* Announce */
    pmix_output_verbose(PMIX_MCA_BASE_VERBOSE_COMPONENT, output_id,
                        "mca: base: components_open: opening %s components",
                        framework->framework_name);

    /* Traverse the list of components */
    PMIX_LIST_FOREACH_SAFE (cli, next, components, pmix_mca_base_component_list_item_t) {
        const pmix_mca_base_component_t *component = cli->cli_component;

        pmix_output_verbose(PMIX_MCA_BASE_VERBOSE_COMPONENT, output_id,
                            "mca: base: components_open: found loaded component %s",
                            component->pmix_mca_component_name);

        if (NULL != component->pmix_mca_open_component) {
            /* Call open if register didn't call it already */
            ret = component->pmix_mca_open_component();

            if (PMIX_SUCCESS == ret) {
                pmix_output_verbose(PMIX_MCA_BASE_VERBOSE_COMPONENT, output_id,
                                    "mca: base: components_open: "
                                    "component %s open function successful",
                                    component->pmix_mca_component_name);
            } else {
                if (PMIX_ERR_NOT_AVAILABLE != ret) {
                    /* If the component returns PMIX_ERR_NOT_AVAILABLE,
                       it's a cue to "silently ignore me" -- it's not a
                       failure, it's just a way for the component to say
                       "nope!".

                       Otherwise, however, display an error.  We may end
                       up displaying this twice, but it may go to separate
                       streams.  So better to be redundant than to not
                       display the error in the stream where it was
                       expected. */

                    if (pmix_mca_base_show_load_errors(component->pmix_mca_type_name,
                                                       component->pmix_mca_component_name)) {
                        pmix_output_verbose(PMIX_MCA_BASE_VERBOSE_ERROR, output_id,
                                            "mca: base: components_open: component %s "
                                            "/ %s open function failed",
                                            component->pmix_mca_type_name,
                                            component->pmix_mca_component_name);
                    }
                    pmix_output_verbose(PMIX_MCA_BASE_VERBOSE_COMPONENT, output_id,
                                        "mca: base: components_open: "
                                        "component %s open function failed",
                                        component->pmix_mca_component_name);
                }

                pmix_mca_base_component_close(component, output_id);

                pmix_list_remove_item(components, &cli->super);
                PMIX_RELEASE(cli);
            }
        }
    }

    /* All done */

    return PMIX_SUCCESS;
}
