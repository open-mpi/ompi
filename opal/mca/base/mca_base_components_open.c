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
 * Copyright (c) 2008-2022 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2011-2015 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014      Hochschule Esslingen.  All rights reserved.
 * Copyright (c) 2022      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "opal/class/opal_list.h"
#include "opal/class/opal_cstring.h"
#include "opal/constants.h"
#include "opal/mca/base/base.h"
#include "opal/mca/mca.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"

/*
 * Local functions
 */
static int open_components(mca_base_framework_t *framework);

struct mca_base_dummy_framework_list_item_t {
    opal_list_item_t super;
    mca_base_framework_t framework;
};

typedef struct fc_pair {
    opal_list_item_t li;
    char *framework_name;
    char *component_name;
} opal_base_fc_pair_t;

OBJ_CLASS_DECLARATION(opal_base_fc_pair_t);

typedef enum {
    SHOW_LOAD_ERRORS_ALL,
    SHOW_LOAD_ERRORS_INCLUDE,
    SHOW_LOAD_ERRORS_EXCLUDE,
    SHOW_LOAD_ERRORS_NONE,
} show_load_type_t;

static show_load_type_t show_load_errors = SHOW_LOAD_ERRORS_ALL;
static opal_list_t show_load_errors_include = {0};
static opal_list_t show_load_errors_exclude = {0};


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

OBJ_CLASS_INSTANCE(opal_base_fc_pair_t, opal_list_item_t,
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
int mca_base_show_load_errors_init(void)
{
    OBJ_CONSTRUCT(&show_load_errors_include, opal_list_t);
    OBJ_CONSTRUCT(&show_load_errors_exclude, opal_list_t);

    // Check to see if mca_base_component_show_load_errors is a
    // boolean value
    opal_cstring_t *cstr = opal_cstring_create(mca_base_component_show_load_errors);
    if (NULL == cstr) {
        int ret = OPAL_ERROR;
        opal_show_help("help-mca-base.txt",
                       "internal error during init", true,
                       __func__, __FILE__, __LINE__,
                       ret,
                       "Failed to create opal_cstring");
        return ret;
    }
    bool value;
    int ret = opal_cstring_to_bool(cstr, &value);
    OBJ_RELEASE(cstr);

    if (OPAL_SUCCESS == ret) {
        // True true values as a synonym for "all", and false values
        // as a synonym for "none".  This is mainly for backwards
        // compatibility with Open MPI <= v4.x, where
        // mca_base_component_show_load_errors was a boolean value.
        if (value) {
            show_load_errors = SHOW_LOAD_ERRORS_ALL;
        } else {
            show_load_errors = SHOW_LOAD_ERRORS_NONE;
        }
    } else if (strcasecmp(mca_base_component_show_load_errors, "all") == 0) {
        show_load_errors = SHOW_LOAD_ERRORS_ALL;
    }
    else if (strcasecmp(mca_base_component_show_load_errors, "none") == 0) {
        show_load_errors = SHOW_LOAD_ERRORS_NONE;
    } else {
        // We have a comma-delimited list of values.  Is it
        // "include"-style, or "exclude" style?
        size_t pos = 0;
        opal_list_t *list = &show_load_errors_include;
        show_load_errors = SHOW_LOAD_ERRORS_INCLUDE;
        if (mca_base_component_show_load_errors[0] == '^') {
            pos = 1;
            list = &show_load_errors_exclude;
            show_load_errors = SHOW_LOAD_ERRORS_EXCLUDE;
        }

        // Examine each of the values in the comma-delimited list.
        // Each value can be of the form "framework" or
        // "framework/component".
        char **values = opal_argv_split(mca_base_component_show_load_errors + pos,
                                        ',');
        if (values == NULL) {
            ret = OPAL_ERROR;
            opal_show_help("help-mca-base.txt",
                           "internal error during init", true,
                           __func__, __FILE__, __LINE__,
                           ret,
                           "Failed to argv split opal_mca_base_component_show_load_errors");
            return ret;
        }

        char **split;
        int argc;
        opal_base_fc_pair_t *fcp;
        for (int i = 0; values[i] != NULL; ++i) {
            split = opal_argv_split(values[i], '/');
            if (NULL == split) {
                ret = OPAL_ERROR;
                opal_show_help("help-mca-base.txt",
                               "internal error during init", true,
                               __func__, __FILE__, __LINE__,
                               ret,
                               "Failed to argv split opal_mca_base_component_show_load_errors value");
                return ret;
            }

            argc = opal_argv_count(split);
            if (0 == argc) {
                // This should never happen
                ret = OPAL_ERROR;
                opal_show_help("help-mca-base.txt",
                               "internal error during init", true,
                               __func__, __FILE__, __LINE__,
                               ret,
                               "Argv split resulted in 0 tokens");
                return ret;
            }

            if (strlen(split[0]) == 0) {
                // Empty entry (e.g., consecutive commas); silently
                // skip it
                continue;
            }

            if (argc > 2) {
                ret = OPAL_ERR_BAD_PARAM;
                opal_show_help("help-mca-base.txt",
                               "show_load_errors: too many /", true,
                               values[i]);
                return ret;
            }

            fcp = OBJ_NEW(opal_base_fc_pair_t);
            if (NULL == fcp) {
                ret = OPAL_ERR_OUT_OF_RESOURCE;
                opal_show_help("help-mca-base.txt",
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

            opal_list_append(list, &fcp->li);
        }
        opal_argv_free(values);
    }

    return OPAL_SUCCESS;
}


bool mca_base_show_load_errors(const char *framework_name,
                               const char *component_name)
{
    if (SHOW_LOAD_ERRORS_ALL == show_load_errors) {
        return true;
    } else if (SHOW_LOAD_ERRORS_NONE == show_load_errors) {
        return false;
    }

    // If we get here, it means we have an include or exclude list.
    // Setup for what to do based on whether it's an include or
    // exclude list.
    opal_list_t *list;
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
    opal_base_fc_pair_t *item;
    OPAL_LIST_FOREACH(item, list, opal_base_fc_pair_t) {
        if (strcmp(framework_name, item->framework_name) == 0) {
            if (NULL == item->component_name) {
                // If there's no component name, then we're matching
                // all components in this framework.
                return value_if_match_found;
            } else if (strcmp(component_name, item->component_name) == 0) {
                // We matched both the framework *and* component name.
                return value_if_match_found;
            }
        }
    }

    // We didn't find a match.
    return !value_if_match_found;
}

int mca_base_show_load_errors_finalize(void)
{
    OBJ_DESTRUCT(&show_load_errors_include);
    OBJ_DESTRUCT(&show_load_errors_exclude);

    return OPAL_SUCCESS;
}

/**
 * Function for finding and opening either all MCA components, or the
 * one that was specifically requested via a MCA parameter.
 */
int mca_base_framework_components_open(mca_base_framework_t *framework, mca_base_open_flag_t flags)
{
    /* Open flags are not used at this time. Suppress compiler warning. */
    if (flags & MCA_BASE_OPEN_FIND_COMPONENTS) {
        bool open_dso_components = !(flags & MCA_BASE_OPEN_STATIC_ONLY);
        /* Find and load requested components */
        int ret = mca_base_component_find(NULL, framework, false, open_dso_components);
        if (OPAL_SUCCESS != ret) {
            return ret;
        }
    }

    /* Open all registered components */
    return open_components(framework);
}

/*
 * Traverse the entire list of found components (a list of
 * mca_base_component_t instances).  If the requested_component_names
 * array is empty, or the name of each component in the list of found
 * components is in the requested_components_array, try to open it.
 * If it opens, add it to the components_available list.
 */
static int open_components(mca_base_framework_t *framework)
{
    opal_list_t *components = &framework->framework_components;
    uint32_t open_only_flags = MCA_BASE_METADATA_PARAM_NONE;
    int output_id = framework->framework_output;
    mca_base_component_list_item_t *cli, *next;
    int ret;

    /*
     * Pre-process the list with parameter constraints
     * e.g., If requested to select only CR enabled components
     *       then only make available those components.
     *
     * JJH Note: Currently checkpoint/restart is the only user of this
     *           functionality. If other component constraint options are
     *           added, then this logic can be used for all constraint
     *           options.
     *
     * NTH: Logic moved to mca_base_components_filter.
     */

    /* If mca_base_framework_register_components was called with the MCA_BASE_COMPONENTS_ALL flag
       we need to trim down and close any extra components we do not want open */
    ret = mca_base_components_filter(framework, open_only_flags);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    /* Announce */
    opal_output_verbose(MCA_BASE_VERBOSE_COMPONENT, output_id,
                        "mca: base: components_open: opening %s components",
                        framework->framework_name);

    /* Traverse the list of components */
    OPAL_LIST_FOREACH_SAFE (cli, next, components, mca_base_component_list_item_t) {
        const mca_base_component_t *component = cli->cli_component;

        opal_output_verbose(MCA_BASE_VERBOSE_COMPONENT, output_id,
                            "mca: base: components_open: found loaded component %s",
                            component->mca_component_name);

        if (NULL != component->mca_open_component) {
            /* Call open if register didn't call it already */
            ret = component->mca_open_component();

            if (OPAL_SUCCESS == ret) {
                opal_output_verbose(MCA_BASE_VERBOSE_COMPONENT, output_id,
                                    "mca: base: components_open: "
                                    "component %s open function successful",
                                    component->mca_component_name);
            } else {
                if (OPAL_ERR_NOT_AVAILABLE != ret) {
                    /* If the component returns OPAL_ERR_NOT_AVAILABLE,
                       it's a cue to "silently ignore me" -- it's not a
                       failure, it's just a way for the component to say
                       "nope!".

                       Otherwise, however, display an error.  We may end
                       up displaying this twice, but it may go to separate
                       streams.  So better to be redundant than to not
                       display the error in the stream where it was
                       expected. */

                    if (mca_base_show_load_errors(component->mca_type_name,
                                                  component->mca_component_name)) {
                        opal_output_verbose(MCA_BASE_VERBOSE_ERROR, output_id,
                                            "mca: base: components_open: component %s "
                                            "/ %s open function failed",
                                            component->mca_type_name,
                                            component->mca_component_name);
                    }
                    opal_output_verbose(MCA_BASE_VERBOSE_COMPONENT, output_id,
                                        "mca: base: components_open: "
                                        "component %s open function failed",
                                        component->mca_component_name);
                }

                mca_base_component_close(component, output_id);

                opal_list_remove_item(components, &cli->super);
                OBJ_RELEASE(cli);
            }
        }
    }

    /* All done */

    return OPAL_SUCCESS;
}
